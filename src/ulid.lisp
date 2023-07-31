;; (ql:quickload '(:cl-intbytes))
(in-package :ulid)

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun get-unix-time-ms ()
  (* (get-unix-time) 1000))

(defun %insecure-random-bytes (n)
  "Returns an array of n random bytes."
    (let ((bytes (make-array n :element-type '(unsigned-byte 8))))
        (loop :for i :from 0 :below n
            :do (setf (aref bytes i) (random 256)))
        bytes))

(defvar *random-byte-fn* #'%insecure-random-bytes
  "Function to generate random bytes. Defaults to %insecure-random-bytes, which is
not cryptographically secure. To use a cryptographically secure random byte
generator you can use the the cl-uuid/strong-random system, which pulls in the ironclad
system as a dependency.")

(defstruct (ulid (:constructor %make-ulid))
  (bytes (make-array 16 :element-type '(unsigned-byte 8)) :read-only t)
  (code nil :read-only t))
;; (%make-ulid)

(defun make-ulid-string (&optional (time (get-unix-time-ms)))
  (let* ((timestamp-bytes (int->octets time +timestamp-len+))
         (random-bytes (funcall *random-byte-fn*  +randomness-len+))
         (combined-bytes (concatenate '(vector (unsigned-byte 8))
                                      timestamp-bytes random-bytes)))
    (encode combined-bytes)))
;; (make-ulid-string)

(defun make-ulid (&optional (time (get-unix-time-ms)))
  (let* ((ulid (%make-ulid))
         (timestamp-bytes (int->octets time +timestamp-len+))
         (random-bytes (funcall *random-byte-fn* +randomness-len+))
         (combined-bytes (concatenate '(vector (unsigned-byte 8))
                                      timestamp-bytes random-bytes))
         (ulid-str (encode combined-bytes)))
    (%make-ulid :bytes combined-bytes :code ulid-str)))
;; (make-ulid)
