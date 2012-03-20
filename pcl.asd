;;;; pcl.asd

(asdf:defsystem :pcl
  :serial t
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "a-simple-database")
	       (:file "unit-test-framework")
	       (:file "pathname")
	       (:file "spam")))