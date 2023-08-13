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

(test lexicographic-sorting
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
;; (run! 'ulid/tests::lexicographic-sorting)

(test round-trip.smoke-1
  (let* ((frozen-time (get-unix-time-ms))
         (random-fn (make-mock-random-fn 0))
         (ulid-generator (make-ulid-generator :random-number-fn random-fn))
         (*ulid-generator* ulid-generator))
    (loop for i :below 10
          for rnd = 1 :then (1+ rnd)
          :for time = frozen-time :then (1+ time)
          :do (multiple-value-bind (ulid-str ulid-bytes) (ulid time)
                (multiple-value-bind (ts random) (decode-to-values ulid-str)
                  (is (= ts time) "Timestamp should round-trip")
                  (is (= random rnd) "Randomness should round-trip"))
                (multiple-value-bind (ts random) (decode-to-values ulid-bytes)
                  (is (= ts time) "Timestamp should round-trip")
                  (is (= random rnd) "Randomness should round-trip"))))))
;; (run! 'ulid/tests::round-trip.smoke-1)
;; (defparameter *r-1-inc* (make-mock-random-fn 1))
;; (funcall *r-1-inc* 1)

;; (ql:quickload '(:ulid/tests))
;; (run! 'ulid/tests-suite-exists)
;; (run! 'ulid/tests::test-max-random)
;; (run! 'ulid/tests::timestamp-and-randomness-type-and-bounds-checking)
;; (run! 'ulid/tests::lexicographic-sorting)
;; (run! 'ulid/tests::round-trip.smoke-1)
;; (run! 'ulid/tests-suite)
