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
;;    derived from Spot-On without specific prior written permission.
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

(defun rotl (x b)
  (logior (ash x b) (ash x (- b 64)))
)

(defun crysp_siphash (data key)
  (declare (type (signed-byte 64) hmac))
  (declare (type (signed-byte 64) m_v0))
  (declare (type (signed-byte 64) m_v1))
  (declare (type (signed-byte 64) m_v2))
  (declare (type (signed-byte 64) m_v3))

  (setf hmac 0)
  (setf m_v0 0)
  (setf m_v1 0)
  (setf m_v2 0)
  (setf m_v3 0)

  (defun round ()
    (setf m_v0 (+ m_v0 m_v1))
    (setf m_v1 (rotl m_v1 13))
    (setf m_v1 (logxor m_v0 m_v1))
    (setf m_v0 (rotl m_v0 32))
    (setf m_v2 (+ m_v2 m_v3))
    (setf m_v3 (rotl m_v3 16))
    (setf m_v3 (logxor m_v2 m_v3))
    (setf m_v2 (+ m_v1 m_v2))
    (setf m_v1 (rotl m_v1 17))
    (setf m_v1 (logxor m_v1 m_v2))
    (setf m_v2 (rotl m_v2 32))
    (setf m_v0 (+ m_v0 m_v3))
    (setf m_v3 (rotl m_v3 21))
    (setf m_v3 (logxor m_v0 m_v3))
  )

  hmac
)
