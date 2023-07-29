;; (ql:quickload '(:serapeum :cl+ssl :secure-random))

(defpackage :ulid
  (:use :cl)
  (:import-from #:alexandria
                #:iota)
  (:import-from #:serapeum
                #:get-unix-time)
  (:export
   #:ulid))

(in-package :ulid)

;; Crockford's Base32
(defparameter *encoding* (coerce "0123456789ABCDEFGHJKMNPQRSTVWXYZ" 'simple-vector))
(defparameter *encoding-len* (length *encoding*))
(defparameter *time-max* (1- (expt 2 48)))
(defparameter *time-len* 10)
(defparameter *random-len* 16)

(defstruct ulid-prng
  (get-random-number #'secure-random:number))

(defstruct ulid-factory
  (prng (make-ulid-prng)))

(defun create-error (message source)
  (make-condition 'simple-error :format-control message :format-arguments (list source)))

(defun replace-char-at (str index char)
  (if (>= index (length str))
    str
    (concatenate 'string
      (subseq str 0 index)
      (string char)
      (subseq str (1+ index)))))

(defun increment-base32 (str)
  (do ((done nil)
       (index (length str))
       (max-char-index (*encoding-len* - 1))
       char
       char-index)
      ((or done (negp index)))
    (setq char (aref str index))
    (setq char-index (position char *encoding* :test #'char=))
    (if (null char-index)
      (error (create-error "incorrectly encoded string" "ulid"))
      (if (= char-index max-char-index)
        (setq str (replace-char-at str index (aref *encoding* 0)))
        (setq done (replace-char-at str index (aref *encoding* (1+ char-index)))))))
  (if (stringp done)
    done
    (error (create-error "cannot increment this string" "ulid"))))

(defun random-char ()
  (with-slots (get-random-number) #'ulid-prng
    (let ((rand (floor (*encoding-len* (funcall get-random-number *encoding-len*)))))
      (if (= rand *encoding-len*)
        (setq rand (- rand 1)))
      (char *encoding* rand))))

;; (random-char)

(defun encode-time (now len)
  (unless (and (numberp now) (integerp now))
    (error "time must be an integer and positive"))
  (when (>= now *time-max*)
    (error (create-error (format nil "cannot encode time greater than ~a" *time-max*) "ulid")))
  (let ((mod) (str ""))
    (dotimes (_ len str)
      (setq mod (mod now *encoding-len*))
      (setq str (concatenate 'string (string (aref *encoding* mod)) str))
      (setq now (- now mod *encoding-len*)))))

(defun encode-random (len)
  (with-slots (get-random-number) #'ulid-prng
    (let ((str ""))
      (dotimes (_ len str)
        (setq str (concatenate 'string (string (random-char get-random-number)) str))))))

(defun decode-time (id)
  (when (/= (length id) (+ *time-len* *random-len*))
    (error (create-error "malformed ulid" "ulid")))
  (with-input-from-string (s (subseq id 0 *time-len*)
    (let ((time 0))
      (loop for char = (read-char s nil)
            while char
            do (let ((encoding-index (position char *encoding* :test #'char=)))
                 (when encoding-index
                   (incf time (* encoding-index (expt *encoding-len* (1- *time-len*)))))))
      (when (> time *time-max*)
        (error (create-error "malformed ulid, timestamp too large" "ulid")))
      time))))

(defun ulid (&optional seed-time)
  (when (null seed-time)
    (setq seed-time (local-time:timestamp-to-unix (local-time:now))))
  (concatenate 'string (encode-time seed-time *time-len*) (encode-random *random-len*)))

(defun monotonic-ulid (seed-time last-time last-random)
  (with-slots (prng) (defun ulid-factory)
    (when (null seed-time)
      (setq seed-time (local-time:timestamp-to-unix (local-time:now))))
    (if (<= seed-time last-time)
      (progn
        (setq last-random (increment-base32 last-random))
        (concatenate 'string (encode-time last-time *time-len*) last-random))
      (progn
        (setq last-time seed-time)
        (setq last-random (encode-random *random-len* prng))
        (concatenate 'string (encode-time seed-time *time-len*) last-random)))))

(defun ulid-factory ()
  (make-ulid-factory :prng (make-ulid-prng)))


(defparameter *ulid-factory* (ulid-factory))

;; (ulid)

;; ;; Crockford's Base32 https://en.wikipedia.org/wiki/Base32
;; (defparameter *encoding* (coerce "0123456789ABCDEFGHJKMNPQRSTVWXYZ" 'simple-vector)
;;   ;;(mapcar #'(lambda (x) (aref "0123456789ABCDEFGHJKMNPQRSTVWXYZ" x)) (iota 32))
;;   "The encoding base32 table.")

;; (defparameter *encoding-len* (length *encoding*)
;;   "The length of the encoding base32 table.")
;; (defparameter *time-len* 10
;;   "The length of the time based encoding.")
;; (defparameter *random-len* 16
;;   "The length of the random string.")

;; (defun secure-random ()
;;   "A function that returns a random float."
;;   (secure-random:number 100000.0))

;; (defun now ()
;;   "A function that returns unix timestamp in seconds with milliseconds precision."
;;   (/ (get-unix-time) 1000))

;; (defun encode-time (&optional (time (now)) (len *time-len*))
;;   "Generates the time-based part of a ulid."
;;   (setf time (floor (* time 1000)) )
;;   (loop for i downfrom len to 1
;;         for mod = (mod time *encoding-len*)
;;         collect (aref *encoding* mod) into result
;;         do (setf time (/ (- time mod) *encoding-len*))
;;         finally (return (coerce result 'string))))

;; (defun encode-random (&optional (len *random-len*))
;;   "Generates the random part of a ulid."
;;   (coerce (loop for i from 1 to len
;;                 collect (aref *encoding* (floor (* (secure-random) *encoding-len*))))
;;           'string))

;; (defun ulid (&optional (time (now)))
;;   "Generates a ulid."
;;   (concatenate 'string (encode-time time) (encode-random)))

;; (encode-time)
;; (encode-random)
;; (ulid)
