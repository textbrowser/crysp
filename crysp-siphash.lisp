;; Copyright (c) 2018 Alexis Megas.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from Crysp without specific prior written permission.
;;
;; CRYSP IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; CRYSP, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defconstant C0 #x736f6d6570736575)
(defconstant C1 #x646f72616e646f6d)
(defconstant C2 #x6c7967656e657261)
(defconstant C3 #x7465646279746573)
(defconstant LONG_BYTES 8)
(defconstant POW_2_64 (expt 2 64))
(defvar C_ROUNDS (make-array 2 :initial-contents '(2 4)))
(defvar D_ROUNDS (make-array 2 :initial-contents '(4 8)))

(defun bytes_to_number (data start)
  (setf number (logior (ash (logand (aref data (+ start 7)) #xff) 56)
		       (ash (logand (aref data (+ start 6)) #xff) 48)
		       (ash (logand (aref data (+ start 5)) #xff) 40)
		       (ash (logand (aref data (+ start 4)) #xff) 32)
		       (ash (logand (aref data (+ start 3)) #xff) 24)
		       (ash (logand (aref data (+ start 2)) #xff) 16)
		       (ash (logand (aref data (+ start 1)) #xff) 8)
		       (logand (aref data start) #xff)))
  number
)

(defun rotl (x b)
  (logior (logand (ash x (mod b 64)) (1- (ash 1 64)))
          (logand (ash x (- (- 64 (mod b 64)))) (1- (ash 1 64))))
)

(defun siphash_round (m_v0 m_v1 m_v2 m_v3)
  (setf m_v0 (mod (+ m_v0 m_v1) POW_2_64))
  (setf m_v1 (rotl m_v1 13))
  (setf m_v1 (logxor m_v0 m_v1))
  (setf m_v0 (rotl m_v0 32))
  (setf m_v2 (mod (+ m_v2 m_v3) POW_2_64))
  (setf m_v3 (rotl m_v3 16))
  (setf m_v3 (logxor m_v2 m_v3))
  (setf m_v2 (mod (+ m_v1 m_v2) POW_2_64))
  (setf m_v1 (rotl m_v1 17))
  (setf m_v1 (logxor m_v1 m_v2))
  (setf m_v2 (rotl m_v2 32))
  (setf m_v0 (mod (+ m_v0 m_v3) POW_2_64))
  (setf m_v3 (rotl m_v3 21))
  (setf m_v3 (logxor m_v0 m_v3))
  (setf array (make-array 4 :element-type '(signed-byte 64)))
  (setf (aref array 0) m_v0)
  (setf (aref array 1) m_v1)
  (setf (aref array 2) m_v2)
  (setf (aref array 3) m_v3)
  array
)

(defun crysp_siphash (c_round d_round data key)
  (declare (type (signed-byte 64) b))
  (declare (type (signed-byte 64) hmac))
  (declare (type (signed-byte 64) k0))
  (declare (type (signed-byte 64) k1))
  (declare (type (signed-byte 64) m))
  (declare (type (signed-byte 64) m_v0))
  (declare (type (signed-byte 64) m_v1))
  (declare (type (signed-byte 64) m_v2))
  (declare (type (signed-byte 64) m_v3))

  (setf hmac 0)
  (setf k0 0)
  (setf k1 0)
  (setf m 0)
  (setf m_v0 0)
  (setf m_v1 0)
  (setf m_v2 0)
  (setf m_v3 0)

  ;; Initialization.

  (setf k0 (bytes_to_number key 0))
  (setf k1 (bytes_to_number key LONG_BYTES))
  (setf m_v0 (logxor k0 C0))
  (setf m_v1 (logxor k1 C1))
  (setf m_v2 (logxor k0 C2))
  (setf m_v3 (logxor k1 C3))

  ;; Compression.

  (loop for i from 0 to (1- (floor (array-total-size data) 8)) do
	(setf m (bytes_to_number data (* i 8)))
	(setf m_v3 (logxor m_v3 m))

	(loop for j from 0 to (1- (aref C_ROUNDS c_round)) do
	      (setf array (siphash_round m_v0 m_v1 m_v2 m_v3))
	      (setf m_v0 (aref array 0))
	      (setf m_v1 (aref array 1))
	      (setf m_v2 (aref array 2))
	      (setf m_v3 (aref array 3)))

	(setf m_v0 (logxor m_v0 m)))

  (setf offset (* (floor (array-total-size data) 8) 8))
  (setf b (ash (array-total-size data) 56))
  (setf remainder (mod (array-total-size data) 8))

  (if (= remainder 7) (setf b (logior (ash (aref data (+ offset 6)) 48) b)))
  (if (= remainder 7) (setf remainder (1- remainder)))
  (if (= remainder 6) (setf b (logior (ash (aref data (+ offset 5)) 40) b)))
  (if (= remainder 6) (setf remainder (1- remainder)))
  (if (= remainder 5) (setf b (logior (ash (aref data (+ offset 4)) 32) b)))
  (if (= remainder 5) (setf remainder (1- remainder)))
  (if (= remainder 4) (setf b (logior (ash (aref data (+ offset 3)) 24) b)))
  (if (= remainder 4) (setf remainder (1- remainder)))
  (if (= remainder 3) (setf b (logior (ash (aref data (+ offset 2)) 16) b)))
  (if (= remainder 3) (setf remainder (1- remainder)))
  (if (= remainder 2) (setf b (logior (ash (aref data (+ offset 1)) 8) b)))
  (if (= remainder 2) (setf remainder (1- remainder)))
  (if (= remainder 1) (setf b (logior (aref data offset) b)))

  (setf m_v3 (logxor m_v3 b))

  (loop for i from 0 to (1- (aref C_ROUNDS c_round)) do
	(setf array (siphash_round m_v0 m_v1 m_v2 m_v3))
	(setf m_v0 (aref array 0))
	(setf m_v1 (aref array 1))
	(setf m_v2 (aref array 2))
	(setf m_v3 (aref array 3)))

  (setf m_v0 (logxor m_v0 b))

  ;; Finalization.

  (setf m_v2 (logxor m_v2 #xff))

  (loop for i from 0 to (1- (aref D_ROUNDS d_round)) do
	(setf array (siphash_round m_v0 m_v1 m_v2 m_v3))
	(setf m_v0 (aref array 0))
	(setf m_v1 (aref array 1))
	(setf m_v2 (aref array 2))
	(setf m_v3 (aref array 3)))

  (setf hmac (logxor m_v0 m_v1 m_v2 m_v3))
  hmac
)

(defun test1 ()
  (setf data (make-array 15 :element-type '(signed-byte 8)))
  (setf key (make-array 16 :element-type '(signed-byte 8)))

  (loop for i from 0 to (1- (array-total-size data)) do
	(setf (aref data i) i))

  (loop for i from 0 to (1- (array-total-size key)) do
	(setf (aref key i) i))

  (print (write-to-string (crysp_siphash 0 0 data key) :base 16))
)
