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
