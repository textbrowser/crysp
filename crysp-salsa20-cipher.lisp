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

(defvar s_sigma_0
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(101 120 112 97)))

(defvar s_sigma_1
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(110 100 32 51)))

(defvar s_sigma_2
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(50 45 98 121)))

(defvar s_sigma_3
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(116 101 32 107)))

(defvar s_tau_0
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(101 120 112 97)))

(defvar s_tau_1
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(110 100 32 49)))

(defvar s_tau_2
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(54 45 98 121)))

(defvar s_tau_3
  (make-array 4
	      :element-type '(unsigned-byte 32)
	      :initial-contents '(116 101 32 107)))

(defun crysp_salsa20_cipher_encrypt (data key v)
  (if (not (and (arrayp data) (arrayp key) (arrayp v)))
      (return-from
       crysp_salsa20_cipher_encrypt
       (make-array 1
		   :element-type '(unsigned-byte 32)
		   :initial-element 0)))

  (let ((a (make-array 16
		       :element-type '(unsigned-byte 32)
		       :initial-element 0))
	(c (make-array (array-total-size data)
		       :element-type '(unsigned-byte 32)
		       :initial-element 0))
	(s (make-array 64
		       :element-type '(unsigned-byte 32)
		       :initial-element 0)))

    (loop for i from 0 to (- (array-total-size c) 1) do
	  (setq a (concatenate 'array
			       v
			       (littleendian_inverse (floor i 64))
			       #(0 0 0 0)))
	  (setq s (crysp_salsa20_hash (concatenate 'array
						   s_sigma_0
						   (subseq key 0 16)
						   s_sigma_1
						   a
						   s_sigma_2
						   (subseq key 16 32)
						   s_sigma_3)))
	  (setf (aref c i) (logxor (aref data i) (aref s (mod i 64)))))
    c)
)

(defun test1 ()
  (setq k_0 #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
  (setq k_1 #(201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216))
  (setq n #(101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116))
  (setq result #(69 37 68 39 41 15 107 193 255 139 122 6 170 233 217 98
		 89 144 182 106 21 51 200 65 239 49 222 34 215 114 40 126
		 104 197 7 225 197 153 31 2 102 78 76 176 84 245 246 184
		 177 160 133 130 6 72 149 119 192 195 132 236 234 103 246 74))
  (equalp (crysp_salsa20_hash (concatenate 'array
					   s_sigma_0
					   k_0
					   s_sigma_1
					   n
					   s_sigma_2
					   k_1
					   s_sigma_3))
	  result)
)

(defun test2 ()
  (setq data (make-array 512
			 :element-type '(unsigned-byte 32)
			 :initial-element 0))
  (setq k #(128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (setq v #(0 0 0 0 0 0 0 0))
  (print (write-to-string (crysp_salsa20_cipher_encrypt data k v) :base 16))
)
