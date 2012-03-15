;;;; Chapter 9. Practical: Building a Unit Test Framework

(in-package :pcl.unit-test-framework)

(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms
	       collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; test examples

(deftest test-+ ()
  (check 
    (= (+ 1 2) 3)
    (= (+ -1 2) 1)
    (= (+ -1 -2) -3)))

(deftest test-* ()
  (check
    (= (* 1 9) 9)
    (= (* 0 3) 0)
    (+ (* -2 -3) -6)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))