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
  (let ((number 0))
    (setf number (logand (logior (ash u c) (ash u (- (- 32 c)))) #xffffffff))
    number)
)

(defun littleendian (bytes)
  (if (not (arrayp bytes))
      (return-from littleendian 0))

  (let ((number 0))
    (setf number (+ (aref bytes 0)
		    (ash (aref bytes 1) 8)
		    (ash (aref bytes 2) 16)
		    (ash (aref bytes 3) 24)))
    number)
)

(defun littleendian_inverse (number)
  (let ((bytes (make-array 4
			   :element-type '(unsigned-byte 8)
			   :initial-element 0)))
    (setf (aref bytes 0) (logand number #xff))
    (setf (aref bytes 1) (logand (ash number -8) #xff))
    (setf (aref bytes 2) (logand (ash number -16) #xff))
    (setf (aref bytes 3) (logand (ash number -24) #xff))
    bytes)
)

(defun quarterround (y)
  (if (not (arrayp y))
      (return-from quarterround (make-array 4
					    :element-type '(unsigned-byte 32)
					    :initial-element 0)))

  (let ((z (make-array 4
		       :element-type '(unsigned-byte 32)
		       :initial-element 0)))
    (setf (aref z 1)
	  (logxor (aref y 1) (cbitleftrotation (+ (aref y 0) (aref y 3)) 7)))
    (setf (aref z 2)
	  (logxor (aref y 2) (cbitleftrotation (+ (aref y 0) (aref z 1)) 9)))
    (setf (aref z 3)
	  (logxor (aref y 3) (cbitleftrotation (+ (aref z 1) (aref z 2)) 13)))
    (setf (aref z 0)
	  (logxor (aref y 0) (cbitleftrotation (+ (aref z 2) (aref z 3)) 18)))
    z)
)

(defun test1 ()
  (let ((data (make-array 4
			  :element-type '(unsigned-byte 8)
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
			  :element-type '(unsigned-byte 8)
			  :initial-element 0)))
    (setf (aref data 0) 255)
    (setf (aref data 1) 255)
    (setf (aref data 2) 255)
    (setf (aref data 3) 250)
    (print 'littleendian)
    (print (write-to-string (littleendian data) :base 16))
    (print 'littleendian_inverse)
    (print (write-to-string (littleendian_inverse (littleendian data)))))
  (let ((data (make-array 4
			  :element-type '(unsigned-byte 32)
			  :initial-element 0)))
    (print 'quarterround)
    (print (write-to-string (quarterround data) :base 16))
    (setf (aref data 0) #x00000001)
    (setf (aref data 1) #x00000000)
    (setf (aref data 2) #x00000000)
    (setf (aref data 3) #x00000000)
    (print (write-to-string (quarterround data) :base 16))
    (setf (aref data 0) #x00000000)
    (setf (aref data 1) #x00000001)
    (setf (aref data 2) #x00000000)
    (setf (aref data 3) #x00000000)
    (print (write-to-string (quarterround data) :base 16))
    (setf (aref data 0) #x00000000)
    (setf (aref data 1) #x00000000)
    (setf (aref data 2) #x00000001)
    (setf (aref data 3) #x00000000)
    (print (write-to-string (quarterround data) :base 16))
    (setf (aref data 0) #x00000000)
    (setf (aref data 1) #x00000000)
    (setf (aref data 2) #x00000000)
    (setf (aref data 3) #x00000001)
    (print (write-to-string (quarterround data) :base 16))
    (setf (aref data 0) #xe7e8c006)
    (setf (aref data 1) #xc4f9417d)
    (setf (aref data 2) #x6479b4b2)
    (setf (aref data 3) #x68c67137)
    (print (write-to-string (quarterround data) :base 16))
    (setf (aref data 0) #xd3917c5b)
    (setf (aref data 1) #x55f1c407)
    (setf (aref data 2) #x52a58a7a)
    (setf (aref data 3) #x8f887a3b)
    (print (write-to-string (quarterround data) :base 16)))
  nil
)
