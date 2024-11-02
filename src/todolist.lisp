(defpackage #:todolist/src/todolist
  (:nicknames #:todolist)
  (:use #:cl)
  (:export #:index
           #:render-list
           #:add-list
           #:remove-list
           #:*counter*
           #:*database*
           #:main))
(in-package #:todolist/src/todolist)

(defvar *counter* 0
  "Counter used to generate unique keys for the to-do list items.")

(defvar *database* nil
  "List of to-do list items.
   Each item is a cons cell with a unique key and a value.")

(defvar *htmz* "setTimeout(()=>document.querySelector(contentWindow.location.hash||null)?.replaceWith(...contentDocument.body.childNodes))"
  "JavaScript code borrowed from https://leanrada.com/htmz/")

(defun index (params)
  "`params` is an alist but is not used in this function.

   Renders the main page of the to-do list.
   The page contains a form to add new items to the list and an iframe to display the list.
   The list is generated by calling the render-list function."
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
  "Renders the to-do list as an HTML unordered list.
   Each item in the list is a link that allows the user to remove the item.
   The list is generated from the global *database* variable."
  (spinneret:with-html-string
    (:ul :id "list"
      (loop for (key . value) in *database* do
        (:li (:a :href (format nil "/remove?key=~a#list" key) :target "htmz" value))))))

(defun add-list (params)
  "`params` is an alist of the form ((\"value\" . \"content\")).

   Adds a new item to the to-do list.
   The item is stored in the global *database* variable.
   Returns the partial HTML list."
  (let ((value (cdr (assoc "value" params :test #'string=))))
    (setq *database* (cons (cons (incf *counter*) value) *database*))
    (render-list)))

(defun remove-list (params)
  "`params` is an alist of the form ((\"key\" . \"content\")).

   Removes an item from the to-do list.
   The item is removed from the global *database* variable.
   Returns the partial HTML list."
  (let ((key (cdr (assoc "key" params :test #'string=))))
    (setq *database* (remove (parse-integer key) *database* :key #'car))
    (render-list)))

(defun main ()
  "Main function that starts the web server.
   The server listens on port 5000 and serves the to-do list application.
   To stop the server, type any character in the REPL."
  (let ((app (make-instance 'ningle:app)))
    (setf (ningle:route app "/") #'index)
    (setf (ningle:route app "/add") #'add-list)
    (setf (ningle:route app "/remove") #'remove-list)
    (let ((handler (clack:clackup app :server :hunchentoot)))
      (read)
      (clack:stop handler))))
