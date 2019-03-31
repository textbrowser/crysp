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

(defun littleendian (data)
  (let ((number 0))
    (setf number (+ (aref data 0)
		    (ash (aref data 1) 8)
		    (ash (aref data 2) 16)
		    (ash (aref data 3) 24)))
    number)
)

(defun test1 ()
  (let ((data (make-array 4
			  :element-type '(unsigned-byte 8)
			  :initial-element 0)))
    (setf (aref data 0) 86)
    (setf (aref data 1) 75)
    (setf (aref data 2) 30)
    (setf (aref data 3) 9)
    (print (write-to-string (littleendian data) :base 16)))
  (let ((data (make-array 4
			  :element-type '(unsigned-byte 8)
			  :initial-element 0)))
    (setf (aref data 0) 255)
    (setf (aref data 1) 255)
    (setf (aref data 2) 255)
    (setf (aref data 3) 250)
    (print (write-to-string (littleendian data) :base 16)))
)
