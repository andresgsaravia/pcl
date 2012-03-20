;;;; pcl.asd

(asdf:defsystem :pcl
  :serial t
  :components ((:file "package")
               (:file "a-simple-database")
	       (:file "unit-test-framework")
	       (:file "pathname")))

