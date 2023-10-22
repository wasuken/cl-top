(defsystem "cl-top"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
		:components
		((:file "main" :depends-on ("util"))
		 (:file "util"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-top/tests"))))

(defsystem "cl-top/tests"
  :author ""
  :license ""
  :depends-on ("cl-top"
	       "rove")
  :components ((:module "tests"
		:components
		((:file "main")
		 (:file "util"))))
  :description "Test system for cl-top"
  :perform (test-op (op c) (symbol-call :rove :run c)))
