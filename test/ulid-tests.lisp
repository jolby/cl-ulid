(in-package :ulid/tests)

(def-suite ulid/tests-suite)

(in-suite ulid/tests-suite)

(test ulid/tests-suite-exists
  (is-true t))

(defun vec-elts-equal (v1 v2)
  (loop for i from 0 below (length v1)
        always (= (aref v1 i) (aref v2 i))))

(defun make-mock-random-fn (initial-num)
  (let ((num initial-num))
    (lambda (n)
      (declare (ignore n))
      (incf num))))

(test test-max-random
  (let* ((random-fn (make-mock-random-fn (1- +randomness-max+)))
         (ulid-generator (make-ulid-generator :random-number-fn random-fn))
         (frozen-time (get-unix-time-ms)))
    (let ((*ulid-generator* ulid-generator))
      (multiple-value-bind (ulid-str ulid-bytes) (ulid frozen-time)
        (format t "~%ulid-str: ~a~%" ulid-str)
        (format t "ulid-bytes: ~a~%" ulid-bytes)
        (is (not (null ulid-str)) "First ulid string should generate")
        (is (not (null ulid-bytes)) "First ulid bytes should generate"))
      (signals simple-error (ulid frozen-time)))))
;; (run! 'ulid/tests::test-max-random)

(test timestamp-and-randomness-type-and-bounds-checking
  (let ((too-big-timestamp (1+ +time-max+))
        (too-big-randomness (1+ +randomness-max+)))
    (signals simple-type-error (ulid::%make-ulid too-big-timestamp 1))
    (signals simple-type-error (ulid::%make-ulid 1 too-big-randomness))
    (signals simple-type-error (ulid::%make-ulid -1 1))
    (signals simple-type-error (ulid::%make-ulid 1 -1))))

(test lexigraphic-sorting
  (let* ((frozen-time (get-unix-time-ms))
         (random-fn (make-mock-random-fn 1))
         (ulid-generator (make-ulid-generator :random-number-fn random-fn))
         (*ulid-generator* ulid-generator)
         (same-ms-ulids (loop :for i :below 1000
                              :collect (ulid frozen-time)))
         (normal-ulids (loop :for i :below 1000
                             :for time = frozen-time :then (1+ time)
                             :collect (ulid time))))
         (is (and
              (every #'string-lessp same-ms-ulids (rest same-ms-ulids))
              (every #'string-lessp normal-ulids (rest normal-ulids))))))
;; (run! 'ulid/tests::lexigraphic-sorting)



;;(run! 'ulid/tests::timestamp-and-randomness-type-checking)
;;(check-type (1+ +time-max+) ulid::timestamp-integer)
;;(check-type -1 ulid::timestamp-integer)

;;(defparameter *random-bytes-fn* (make-mock-random-fn (1- +randomness-max+)))
;;(funcall *random-bytes-fn* 1)
;; (test valid-time-and-random-integers )

;; (test valid-base32-encoder-input-1
;;   ;; one byte too many ==> error
;;   (signals simple-error (encode-timestamp (vector 255 255 255 255 255 255 125)))
;;   (finishes (encode-timestamp (vector 255 255 255 255 255 124)))
;;   (signals simple-error (encode-randomness (vector 255 255 255 255 255 255 255 255 255 125 125))
;;   (finishes (encode-randomness (vector 255 255 255 255 255 255 255 255 255 125)))))

;; (test round-trip.smoke-1
;;   (let ((all-passed 't))
;;     (loop :for i :below 1000
;;           :do (let* ((rand-16-bytes (funcall *random-bytes-fn* ulid::+bytes-len+))
;;                      (lisp-encode (encode rand-16-bytes))
;;                      (lisp-decode (decode lisp-encode)))
;;                 (unless (vec-elts-equal rand-16-bytes lisp-decode)
;;                   (setf all-passed nil))))
;;     (format t "all-passed: ~a~%" all-passed)
;;     (is-true all-passed)))

;; (test timestamp-retreival
;;   (let* ((ts (ulid::get-unix-time-ms))
;;          (ulid-str (ulid:make-ulid-string ts))
;;          (ulid (ulid:make-ulid ts)))
;;     (is (= ts (ulid:ulid-timestamp ulid-str) (ulid:ulid-timestamp ulid)))))

;; (ql:quickload '(:ulid/tests))
;; (run! 'ulid/tests-suite-exists)
;; (run! 'ulid/tests::valid-base32-encoder-input-1)
;; (run! 'ulid/tests::test-max-random)
;; (run! 'ulid/tests::round-trip.smoke-1)
;; (run! 'ulid/tests::timestamp-retreival)
;; (run! 'ulid/tests-suite)

