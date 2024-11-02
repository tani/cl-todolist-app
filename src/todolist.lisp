(defpackage #:todolist/src/todolist
  (:nicknames #:todolist)
  (:use #:cl)
  (:export #:index
           #:render-list
           #:add-list
           #:remove-list
           #:main))
(in-package #:todolist/src/todolist)

(defvar *counter* 0)

(defvar *items* (make-hash-table))

(defvar *htmz* "setTimeout(()=>document.querySelector(contentWindow.location.hash||null)?.replaceWith(...contentDocument.body.childNodes))")

(defun index (params)
  (declare (ignore params))
  (spinneret:with-html-string
    (:doctype)
    (:html
      (:head
        (:title "Todo List"))
      (:body
        (:h1 "Todo List")
        (:form :action "/add#list" :target "htmz"
          (:input :type "text" :name "value")
          (:input :type "submit" :value "Add"))
        (:ul :id "list" "")
        (:iframe :hidden "" :name "htmz" :onload *htmz*)))))

(defun render-list ()
  (spinneret:with-html-string
    (:ul :id "list"
      (loop for key being the hash-keys of *items*
            for value being the hash-values of *items*
            collect (:li (:a :href (format nil "/remove?key=~a#list" key) :target "htmz" value))))))

(defun add-list (params)
  (let ((item (cdr (assoc "value" params :test #'string=))))
    (setf (gethash (incf *counter*) *items*) item)
    (render-list)))

(defun remove-list (params)
  (let ((key (parse-integer (cdr (assoc "key" params :test #'string=)))))
    (remhash key *items*)
    (render-list)))

(defun main ()
  (let ((app (make-instance 'ningle:app)))
    (setf (ningle:route app "/") #'index)
    (setf (ningle:route app "/add") #'add-list)
    (setf (ningle:route app "/remove") #'remove-list)
    (let ((handler (clack:clackup app :server :hunchentoot)))
      (read)
      (clack:stop handler))))

