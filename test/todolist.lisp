(defpackage #:todolist/test/todolist
  (:use #:cl #:parachute #:todolist/src/todolist))
(in-package #:todolist/test/todolist)

(defun remove-whitespace (s)
  "Remove whitespace from a string including spaces, tabs, and newlines."
  (cl-ppcre:regex-replace-all "[ \t\n\r]+" s ""))

(defun almost-equal-p (a b)
  "Check if two strings are almost equal by ignoring whitespace."
  (string= (remove-whitespace a) (remove-whitespace b)))

(define-test index-test
  (is almost-equal-p " a" "a"))
