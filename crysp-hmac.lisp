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

;; Please read https://en.wikipedia.org/wiki/HMAC for implementation details.

(defun crysp_hmac (block_length data key method)
  (if (not (arrayp data)) (return-from crysp_hmac 'expecting-data-array))
  (if (not (arrayp key)) (return-from crysp_hmac 'expecting-key-array))

  (let* ((array1 (make-array 0
			     :adjustable t
			     :element-type '(unsigned-byte 8)))
	 (array2 (make-array 0
			     :adjustable t
			     :element-type '(unsigned-byte 8)))
	 (ipad (make-array block_length :initial-element #x36))
	 (k (make-array (array-total-size key)
			:adjustable t
			:element-type '(unsigned-byte 8)))
	 (left (make-array block_length
			   :element-type '(unsigned-byte 8)
			   :initial-element 0))
	 (opad (make-array block_length :initial-element #x5c))
	 (right (make-array block_length
			    :element-type '(unsigned-byte 8)
			    :initial-element 0)))

    (dotimes (i (array-total-size key))
      (setf (aref k i) (aref key i)))

    (if (< block_length (array-total-size k))
	(setf k (funcall method k)))

    (if (> block_length (array-total-size k))
	(let ((array (make-array (- block_length (array-total-size k))
				 :element-type '(unsigned-byte 8)
				 :initial-element 0)))
	  (setq k (concatenate 'vector k array))))

    (dotimes (i block_length)
      (setf (aref left i) (logxor (aref k i) (aref opad i))))

    (dotimes (i block_length)
      (setf (aref right i) (logxor (aref k i) (aref ipad i))))

    (setf array1 (funcall method (concatenate 'vector right data)))
    (setf array2 left)

    (dotimes (i (array-total-size array1))
      (setf array2
	    (concatenate 'vector array2 (number_to_bytes (aref array1 i)))))

    (funcall method array2))
)

(defun test1 ()
  (print "3926a207c8c42b0c 41792cbd3e1a1aaa f5f7a25704f62dfc 939c4987dd7ce060 9c5bb1c2447355 b3216f10b537e9af a7b64a4e5391b0d6 31172d07939e087a")
  (print (write-to-string
	  (crysp_hmac 128
		      (make-array 3
				  :element-type '(unsigned-byte 8)
				  :initial-contents '(97 98 99))
		      (make-array 3
				  :element-type '(unsigned-byte 8)
				  :initial-contents '(107 101 121))
		      'crysp_sha_512) :base 16))
  nil
)
