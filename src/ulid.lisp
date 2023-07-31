;; (ql:quickload '(:serapeum :cl+ssl :secure-random))

(defpackage :ulid
  (:use :cl)
  (:export
   #:ulid))

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
  (/ (get-unix-time) 1000))

(defstruct ulid
  (bytes (make-array 16 :element-type '(unsigned-byte 8))))
