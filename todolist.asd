(defsystem #:todolist
  :class package-inferred-system
  :build-operation program-op
  :build-pathname #.(or (uiop:getenv "CL_BUILD_PATHNAME") "todolist")
  :entry-point "todolist:main"
  :depends-on (#:hunchentoot #:clack #:clack-handler-hunchentoot #:ningle #:spinneret #:todolist/src/todolist)
  :in-order-to ((test-op (test-op #:todolist/test))))

(defsystem #:todolist/test
  :depends-on (#:cl-ppcre #:parachute #:todolist/test/todolist)
  :perform (test-op (o c)
             (symbol-call :parachute :test :todolist/test/todolist)))
