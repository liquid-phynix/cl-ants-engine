; vim: set filetype=lisp autoindent:

(declaim (optimize speed))

(defpackage :engine
  (:use :cl :sb-thread))

(in-package :engine)

(defparameter *debug* t)
(defparameter *board* nil) 

(deftype index () 'fixnum)

(defstruct struct-r-c
  (row         0 :type index)
  (col         0 :type index)
  (board-index 0 :type index))

;id 0-255
;index 0-16777215
(deftype cell () '(unsigned-byte 32))

(deftype r-c-vector () '(simple-array struct-r-c (*)))

