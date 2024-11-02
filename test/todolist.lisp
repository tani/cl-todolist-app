(defpackage #:todolist/test/todolist
  (:use #:cl #:parachute #:todolist/src/todolist))
(in-package #:todolist/test/todolist)

(define-test fib-test
  (of-type string (index nil)))
