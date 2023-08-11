;; (ql:quickload '(:cl-intbytes))
(in-package :ulid)

;;;; https://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs
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

(defvar *random-bytes-fn* #'%insecure-random-bytes
  "Function to generate random bytes. cl-ulid/insecure-core system defaults to
%insecure-random-bytes, which is not cryptographically secure. To use a
cryptographically secure random byte generator you can use the the
cl-uuid system, which pulls in the ironclad system as a
dependency, or you can supply your own and setf *random-byte-fn* to it.")

(defvar *random-number-fn* #'random
  "Function to generate random numbers. cl-ulid/insecure-core defaults to #'random,
which is not cryptographically secure. To use a cryptographically secure random number
generator you can use the the cl-uuid system, which pulls in the ironclad
system as a dependency.")

(defvar *default-ulid-generator* nil
  "The default ulid generator. This generator maintains state to ensure that the
monotonicity guarantee is maintained.")

(defvar *ulid-generator* nil
  "The ulid generator. This generator maintains state to ensure that the monotonicity")

(declaim (special *ulid-generator*))

(defstruct (ulid (:constructor %make-ulid))
  ;; (timestamp 0 :type 'timestamp-integer :read-only t)
  ;; (randomness 0 :type 'randomness-integer :read-only t)
  (ulid-bytes (make-array +bytes-len+ :element-type '(unsigned-byte 8)) :type ulid:ulid-byte-array))
;; (%make-ulid)

(defstruct (ulid-generator (:constructor %make-ulid-generator))
  (last-timestamp 0 :type timestamp-integer)
  (last-randomness 0 :type randomness-integer)
  (random-number-fn *random-number-fn* :type (function (ulid::randomness-integer) randomness-integer) :read-only t)
  (random-bytes-fn *random-bytes-fn* :type (function (fixnum) (simple-array (unsigned-byte 8))) :read-only t))

(defun make-ulid-generator (&key
                              (last-timestamp (get-unix-time-ms))
                              (last-randomness (funcall *random-number-fn* +randomness-max+))
                              (random-number-fn *random-number-fn*)
                              (random-bytes-fn *random-bytes-fn*))
  (%make-ulid-generator :last-timestamp last-timestamp
                        :last-randomness last-randomness
                        :random-number-fn random-number-fn
                        :random-bytes-fn random-bytes-fn))

(defun call-with-ulid-generator (fn &key (generator *default-ulid-generator*))
  (let ((*ulid-generator* (or generator (make-ulid-generator))))
    (funcall fn)))

(defmacro with-ulid-generator((&key generator) &body body)
  `(call-with-ulid-generator (lambda () ,@body)
    ,@(when generator `(:generator ,generator))))

(defun make-ulid (&key
                    (time (get-unix-time-ms))
                    (generator *default-ulid-generator*))
  (check-type time timestamp-integer)
  (if (= time (ulid-generator-last-timestamp generator))
      (let ((randomness (incf (ulid-generator-last-randomness generator))))
        (when (> randomness +randomness-max+)
          (setf (ulid-generator-last-randomness generator) 0)))
      (setf (ulid-generator-last-randomness generator)
            (funcall (ulid-generator-random-number-fn generator) +randomness-max+)))
  ;; (let* ((timestamp-bytes (int->octets time +timestamp-len+))
  ;;        (random-bytes (int->octets (ulid-generator-last-randomness generator) +randomness-len+)))
  ;;   (setf (ulid-generator-last-timestamp generator) time)
  ;;   (encode-timestamp-and-randomness-bytes timestamp-bytes random-bytes))
  (let ((ulid-bytes (make-ulid-byte-array))
        (ulid-string (make-ulid-string-array)))
    (ub48set/be ulid-bytes 0 time)
    (ub80set/be ulid-bytes +timestamp-bytes-len+ (ulid-generator-last-randomness generator))
    (values (encode-ulid-bytes ulid-bytes ulid-string) ulid-bytes)))



;; (make-ulid-string)
;; (time (loop for i from 0 to 1000 collect (make-ulid-string)))

;; (with-output-to-string (s) (format s "~a" "hello"))
;; (call-with-ulid-generator (lambda () (format t "~a" *ulid-generator*)))
;; (with-ulid-generator () (format t "~a" *ulid-generator*))
;; (with-ulid-generator (:generator (make-ulid-generator)) (format t "~a" *ulid-generator*))
;; (defun make-ulid-string (&key
;;                            (time (get-unix-time-ms))
;;                            (random (funcall *random-number-fn*)))
;;   (let* ((timestamp-bytes (int->octets time +timestamp-len+))
;;          (random-bytes (funcall *random-byte-fn*  +randomness-len+))
;;          (combined-bytes (concatenate '(vector (unsigned-byte 8))
;;                                       timestamp-bytes random-bytes)))
;;     (encode combined-bytes)))
;; ;; (make-ulid-string)

;; (defun make-ulid (&key
;;                     (time (get-unix-time-ms))
;;                     (random (funcall *random-number-fn*))
;;   (let* ((ulid (%make-ulid))
;;          (timestamp-bytes (int->octets time +timestamp-len+))
;;          (random-bytes (funcall *random-byte-fn* +randomness-len+))
;;          (combined-bytes (concatenate '(vector (unsigned-byte 8))
;;                                       timestamp-bytes random-bytes))
;;          (ulid-str (encode combined-bytes)))
;;     (%make-ulid :bytes combined-bytes :code ulid-str)))
;; ;; (make-ulid)

;; (defgeneric ulid-timestamp (ulid)
;;   (:documentation
;;    "Returns the timestamp of the ulid as a unix time in milliseconds."))

;; (defmethod ulid-timestamp ((ulid ulid))
;;   (octets->int (subseq (ulid-bytes ulid) 0 +timestamp-len+) +timestamp-len+))

;; (defmethod ulid-timestamp ((ulid string))
;;   (octets->int (subseq (decode ulid) 0 +timestamp-len+) +timestamp-len+))

;; (defgeneric ulid-random (ulid)
;;   (:documentation
;;    "Returns the random number part of the ulid."))

;; (defmethod ulid-random ((ulid ulid))
;;   (octets->int (subseq (ulid-bytes ulid) +timestamp-len+) +random-len+))

;; (defmethod ulid-random ((ulid string))
;;   (octets->int (subseq (decode ulid) +timestamp-len+) +random-len+))
