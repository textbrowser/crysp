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

(defconstant IV0 #x6a09e667f3bcc908)
(defconstant IV1 #xbb67ae8584caa73b)
(defconstant IV2 #x3c6ef372fe94f82b)
(defconstant IV3 #xa54ff53a5f1d36f1)
(defconstant IV4 #x510e527fade682d1)
(defconstant IV5 #x9b05688c2b3e6c1f)
(defconstant IV6 #x1f83d9abfb41bd6b)
(defconstant IV7 #x5be0cd19137e2179)
(defvar SIGMA (make-array
	       '(10 16)
	       :initial-contents '((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
				   (14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3)
				   (11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4)
				   (7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8)
				   (9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13)
				   (2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9)
				   (12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11)
				   (13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10)
				   (6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5)
				   (10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0))))

(defun compress (chunk h is_last_block tt)
  (let ((V (make-array 16
		       :element-type '(unsigned-byte 64)
		       :initial-element 0))
	(m (make-array 16
		       :element-type '(unsigned-byte 64)
		       :initial-element 0)))

    (loop for i from 0 to 7 do
	  (setf (aref V i) (aref h i)))

    (setf (aref V 8) IV0)
    (setf (aref V 9) IV1)
    (setf (aref V 10) IV2)
    (setf (aref V 11) IV3)
    (setf (aref V 12) IV4)
    (setf (aref V 13) IV5)
    (setf (aref V 14) IV6)
    (setf (aref V 15) IV7)
    (setf (aref V 12) (logxor (aref V 12) (aref tt 0)))
    (setf (aref V 13) (logxor (aref V 13) (aref tt 1)))

    (when is_last_block
      (setf (aref V 14) (logxor (aref V 14) #xffffffffffffffff)))

    (loop for i from 0 to 15 do
	  (setf (aref m i) (aref chunk i)))

    h)
)

(defun mix (Va Vb Vc Vd x y)
  (let ((V (make-array 4
		       :element-type '(unsigned-byte 64)
		       :initial-element 0)))
    (setf Va (+ Va Vb x))
    (setf Vd (rotate_right (logxor Vd Va) 32))
    (setf Vc (+ Vc Vd))
    (setf Vb (rotate_right (logxor Vb Vc) 24))
    (setf Va (+ Va Vb y))
    (setf Vd (rotate_right (logxor Vd Va) 16))
    (setf Vc (+ Vc Vd))
    (setf Vb (rotate_right (logxor Vb Vc) 63))
    (setf (aref V 0) (logand Va #xffffffffffffffff))
    (setf (aref V 1) (logand Vb #xffffffffffffffff))
    (setf (aref V 2) (logand Vc #xffffffffffffffff))
    (setf (aref V 3) (logand Vd #xffffffffffffffff))
    V)
)

(defun rotate_right (x n)
  (logior (ash x (- n)) (ash x (- 64 n)))
)

(defun crysp_blake2b (data key)
  (if (not (arrayp data)) (return-from crysp_blake2b 'expecting-data-array))
  (if (not (arrayp key)) (return-from crysp_blake2b 'expecting-key-array))
)

(defun test1 ()
)
