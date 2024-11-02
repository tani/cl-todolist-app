(defpackage #:todolist/test/todolist
  (:use #:cl #:parachute #:todolist/src/todolist))
(in-package #:todolist/test/todolist)

(defun remove-whitespace (s)
  "Remove whitespace from a string including spaces, tabs, and newlines."
  (cl-ppcre:regex-replace-all "[ \\t\\n\\r]+" s ""))

(defun almost-equal-p (a b)
  "Check if two strings are almost equal by ignoring whitespace."
  (string= (remove-whitespace a) (remove-whitespace b)))

(define-test index-test
  (is almost-equal-p
      (index nil)
      "
      <!DOCTYPE html>
      <html lang=en>
       <head>
        <meta charset=UTF-8>
        <title>Todo List</title>
       </head>
       <body>
        <h1>Todo List</h1>
        <form action=\"/add#list\" target=htmz>
         <input type=text name=value>
         <input type=submit value=Add>
        </form>
        <ul id=list>
        </ul>
        <iframe hidden name=htmz onload=\"setTimeout(()=>document.querySelector(contentWindow.location.hash||null)?.replaceWith(...contentDocument.body.childNodes))\"></iframe>
       </body>
      </html>
      "))

(define-test add-test
  (let ((*countger* 0)
        (*database* nil))
    (is almost-equal-p
        (add-list '(("value" . "foo")))
        "
        <ul id=list>
          <li><a href=\"/remove?key=1#list\" target=htmz>foo</a>
        </ul>
        ")
    (is = *counter* 1)
    (is equal *database* '((1 . "foo")))
    ))

(define-test remove-test
  (let ((*counter* 1)
        (*database* '((1 . "foo"))))
    (is almost-equal-p
        (remove-list '(("key" . "1")))
        "
        <ul id=list>
        </ul>
        ")
    (is = *counter* 1)
    (is equal *database* nil)
    ))
