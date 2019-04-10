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

(defun cbitleftrotation (u c)
  (declare (type (unsigned-byte 32) c u))

  (logand (logior (ash u c) (ash u (- (- 32 c)))) #xffffffff)
)

(defun columnround (x)
  (declare (type (array (unsigned-byte 32) 16) x))

  (if (not (arrayp x))
      (return-from columnround (make-array 16
					   :element-type '(unsigned-byte 32)
					   :initial-element 0)))

  (let* ((y (make-array 16
			:element-type '(unsigned-byte 32)
			:initial-element 0)))
    (setq z (quarterround (aref x 0) (aref x 4) (aref x 8) (aref x 12)))
    (setf (aref y 0) (aref z 0))
    (setf (aref y 4) (aref z 1))
    (setf (aref y 8) (aref z 2))
    (setf (aref y 12) (aref z 3))
    (setq z (quarterround (aref x 5) (aref x 9) (aref x 13) (aref x 1)))
    (setf (aref y 5) (aref z 0))
    (setf (aref y 9) (aref z 1))
    (setf (aref y 13) (aref z 2))
    (setf (aref y 1) (aref z 3))
    (setq z (quarterround (aref x 10) (aref x 14) (aref x 2) (aref x 6)))
    (setf (aref y 10) (aref z 0))
    (setf (aref y 14) (aref z 1))
    (setf (aref y 2) (aref z 2))
    (setf (aref y 6) (aref z 3))
    (setq z (quarterround (aref x 15) (aref x 3) (aref x 7) (aref x 11)))
    (setf (aref y 15) (aref z 0))
    (setf (aref y 3) (aref z 1))
    (setf (aref y 7) (aref z 2))
    (setf (aref y 11) (aref z 3))
    y)
)

(defun crysp_salsa20_hash (x)
  (declare (type (array (unsigned-byte 32) 64) x))

  (if (not (arrayp x))
      (return-from crysp_salsa20_hash
		   (make-array 64
			       :element-type '(unsigned-byte 32)
			       :initial-element 0)))

  (let ((xarray (make-array 16
			    :element-type '(unsigned-byte 32)
			    :initial-element 0)))
    (setf (aref xarray 0) (littleendian (subseq x 0 4)))
    (setf (aref xarray 1) (littleendian (subseq x 4 8)))
    (setf (aref xarray 2) (littleendian (subseq x 8 12)))
    (setf (aref xarray 3) (littleendian (subseq x 12 16)))
    (setf (aref xarray 4) (littleendian (subseq x 16 20)))
    (setf (aref xarray 5) (littleendian (subseq x 20 24)))
    (setf (aref xarray 6) (littleendian (subseq x 24 28)))
    (setf (aref xarray 7) (littleendian (subseq x 28 32)))
    (setf (aref xarray 8) (littleendian (subseq x 32 36)))
    (setf (aref xarray 9) (littleendian (subseq x 36 40)))
    (setf (aref xarray 10) (littleendian (subseq x 40 44)))
    (setf (aref xarray 11) (littleendian (subseq x 44 48)))
    (setf (aref xarray 12) (littleendian (subseq x 48 52)))
    (setf (aref xarray 13) (littleendian (subseq x 52 56)))
    (setf (aref xarray 14) (littleendian (subseq x 56 60)))
    (setf (aref xarray 15) (littleendian (subseq x 60 64)))
    (setq z (doubleround xarray)) ;; Inefficient.
    (setq z (doubleround z))
    (setq z (doubleround z))
    (setq z (doubleround z))
    (setq z (doubleround z))
    (setq z (doubleround z))
    (setq z (doubleround z))
    (setq z (doubleround z))
    (setq z (doubleround z))
    (setq z (doubleround z))
    (concatenate 'array
		 (littleendian_inverse (logand (+ (aref z 0)
						  (aref xarray 0))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 1)
						  (aref xarray 1))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 2)
						  (aref xarray 2))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 3)
						  (aref xarray 3))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 4)
						  (aref xarray 4))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 5)
						  (aref xarray 5))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 6)
						  (aref xarray 6))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 7)
						  (aref xarray 7))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 8)
						  (aref xarray 8))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 9)
						  (aref xarray 9))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 10)
						  (aref xarray 10))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 11)
						  (aref xarray 11))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 12)
						  (aref xarray 12))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 13)
						  (aref xarray 13))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 14)
						  (aref xarray 14))
					       #xffffffff))
		 (littleendian_inverse (logand (+ (aref z 15)
						  (aref xarray 15))
					       #xffffffff))))
)

(defun doubleround (x)
  (if (not (arrayp x))
      (return-from doubleround (make-array 16
					   :element-type '(unsigned-byte 32)
					   :initial-element 0)))

  (rowround (columnround x))
)

(defun littleendian (bytes)
  (declare (type (array (unsigned-byte 32) 4) bytes))

  (if (not (arrayp bytes))
      (return-from littleendian 0))

  (+ (aref bytes 0)
     (ash (aref bytes 1) 8)
     (ash (aref bytes 2) 16)
     (ash (aref bytes 3) 24))
)

(defun littleendian_inverse (number)
  (declare (type (unsigned-byte 32) number))

  (let ((bytes (make-array 4
			   :element-type '(unsigned-byte 32)
			   :initial-element 0)))
    (setf (aref bytes 0) (logand number #xff))
    (setf (aref bytes 1) (logand (ash number -8) #xff))
    (setf (aref bytes 2) (logand (ash number -16) #xff))
    (setf (aref bytes 3) (logand (ash number -24) #xff))
    bytes)
)

(defun quarterround (a b c d)
  (declare (type (unsigned-byte 32) a b c d))

  (let ((z (make-array 4
		       :element-type '(unsigned-byte 32)
		       :initial-element 0)))
    (setf (aref z 1) (logxor b
			     (cbitleftrotation (logand (+ a d)
						       #xffffffff)
					       7)))
    (setf (aref z 2) (logxor c
			     (cbitleftrotation (logand (+ a (aref z 1))
						       #xffffffff)
					       9)))
    (setf (aref z 3) (logxor d
			     (cbitleftrotation (logand (+ (aref z 1)
							  (aref z 2))
						       #xffffffff)
					       13)))
    (setf (aref z 0) (logxor a
			     (cbitleftrotation (logand (+ (aref z 2)
							  (aref z 3))
						       #xffffffff)
					       18)))
    z)
)

(defun rowround (y)
  (declare (type (array (unsigned-byte 32) 16) y))

  (if (not (arrayp y))
      (return-from rowround (make-array 16
					:element-type '(unsigned-byte 32)
					:initial-element 0)))

  (let* ((z (make-array 16
			:element-type '(unsigned-byte 32)
			:initial-element 0)))
    (setq z1 (quarterround (aref y 0) (aref y 1) (aref y 2) (aref y 3)))
    (setq z2 (quarterround (aref y 5) (aref y 6) (aref y 7) (aref y 4)))
    (setq z3 (quarterround (aref y 10) (aref y 11) (aref y 8) (aref y 9)))
    (setq z4 (quarterround (aref y 15) (aref y 12) (aref y 13) (aref y 14)))
    (setf (aref z 0) (aref z1 0))
    (setf (aref z 1) (aref z1 1))
    (setf (aref z 2) (aref z1 2))
    (setf (aref z 3) (aref z1 3))
    (setf (aref z 5) (aref z2 0))
    (setf (aref z 6) (aref z2 1))
    (setf (aref z 7) (aref z2 2))
    (setf (aref z 4) (aref z2 3))
    (setf (aref z 10) (aref z3 0))
    (setf (aref z 11) (aref z3 1))
    (setf (aref z 8) (aref z3 2))
    (setf (aref z 9) (aref z3 3))
    (setf (aref z 15) (aref z4 0))
    (setf (aref z 12) (aref z4 1))
    (setf (aref z 13) (aref z4 2))
    (setf (aref z 14) (aref z4 3))
    z)
)

(defun test1 ()
  (let ((data (make-array 16
			  :element-type '(unsigned-byte 32)
			  :initial-element 0)))
    (print 'columnround)
    (setq data #(#x00000001 0 0 0
		 #x00000001 0 0 0
		 #x00000001 0 0 0
		 #x00000001 0 0 0))
    (print (write-to-string (columnround data) :base 16))
    (print 'doubleround)
    (setq data #(#x00000001 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (print (write-to-string (doubleround data) :base 16)))
  (let ((data (make-array 4
			  :element-type '(unsigned-byte 32)
			  :initial-element 0)))
    (setf (aref data 0) 86)
    (setf (aref data 1) 75)
    (setf (aref data 2) 30)
    (setf (aref data 3) 9)
    (print 'littleendian)
    (print (write-to-string (littleendian data) :base 16))
    (print 'littleendian_inverse)
    (print (write-to-string (littleendian_inverse (littleendian data)))))
  (let ((data (make-array 4
			  :element-type '(unsigned-byte 32)
			  :initial-element 0)))
    (setf (aref data 0) 255)
    (setf (aref data 1) 255)
    (setf (aref data 2) 255)
    (setf (aref data 3) 250)
    (print 'littleendian)
    (print (write-to-string (littleendian data) :base 16))
    (print 'littleendian_inverse)
    (print (write-to-string (littleendian_inverse (littleendian data)))))
  (let ((data (make-array 16
			  :element-type '(unsigned-byte 32)
			  :initial-element 0)))
    (print 'rowround)
    (setq data #(#x00000001 0 0 0
		 #x00000001 0 0 0
		 #x00000001 0 0 0
		 #x00000001 0 0 0))
    (print (write-to-string (rowround data) :base 16)))
  nil
)

(defun test2 ()
  (let ((data (make-array 64
			  :element-type '(unsigned-byte 32)
			  :initial-element 0)))
    (print 'crysp_salsa20_hash)
    (print (crysp_salsa20_hash data))
    (setq data #(211 159 13 115 76 55 82 183 3 117 222 37 191 187 234 136
		 49 237 179 48 1 106 178 219 175 199 166 48 86 16 179 207
		 31 240 32 63 15 83 93 161 116 147 48 113 238 55 204 36
		 79 201 235 79 3 81 156 47 203 26 244 243 88 118 104 54))
    (print (crysp_salsa20_hash data))
    (setq data #(88 118 104 54 79 201 235 79 3 81 156 47 203 26 244 243
		 191 187 234 136 211 159 13 115 76 55 82 183 3 117 222 37
		 86 16 179 207 49 237 179 48 1 106 178 219 175 199 166 48
		 238 55 204 36 31 240 32 63 15 83 93 161 116 147 48 113))
    (print (crysp_salsa20_hash data)))
  nil
)
