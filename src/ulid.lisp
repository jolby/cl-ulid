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
  "The ulid generator closure. This generator maintains state to ensure that the monotonicity guarantee is maintained.")

(declaim (special *ulid-generator* *last-timestamp* *last-randomness*))
(declaim (inline %make-ulid))

(defun %make-ulid (timestamp randomness)
  (check-type timestamp timestamp-integer)
  (check-type randomness randomness-integer)
  (let ((ulid-bytes (make-ulid-bytes-array))
        (ulid-string (make-ulid-string-array)))
    (ub48set/be ulid-bytes 0 timestamp)
    (ub80set/be ulid-bytes +timestamp-bytes-len+ randomness)
    (values (encode-ulid-bytes ulid-bytes ulid-string) ulid-bytes)))

(defun make-ulid-generator (&key
                              (random-number-fn *random-number-fn*))
  "Return a closure that generates ULIDs. The closure uses the closed over state to
maintain the ULID spec guarantees:
Monotonicity:

When generating a ULID within the same millisecond, we can provide some
guarantees regarding sort order. Namely, if the same millisecond is detected,
the `random` component is incremented by 1 bit in the least significant bit
position (with carrying).

Random overflow:

If, in the extremely unlikely event that, you manage to
generate more than 2<sup>80</sup> ULIDs within the same millisecond, or cause
the random component to overflow with less, the generation will fail.
"
  (let ((last-timestamp 0)
        (last-randomness 0))
    (lambda (&optional (timestamp (get-unix-time-ms)))
      (let ((random-number nil))
        (if (= timestamp last-timestamp)
            ;; If the timestamp is the same as the last timestamp, increment the *last-randomness* by 1
            ;; instead of generating a new random number.
            (progn
              (setf random-number (1+ last-randomness)
                    last-randomness random-number)
              (when (> random-number +randomness-max+)
                (error (format nil
                               "Randomness overflow! local-random: ~a, *last-randomness*: ~a~%"
                               random-number last-randomness))))
            (progn
              (setf random-number (funcall random-number-fn +randomness-max+)
                    last-randomness random-number
                    last-timestamp timestamp)))
        (%make-ulid timestamp random-number)))))

(defun ulid (&optional (timestamp (get-unix-time-ms)))
  "Generate a ULID. If timestamp is not provided, the current time in ms is used."
  (funcall (or *ulid-generator* *default-ulid-generator*) timestamp))
