;;;; Chapter 15. Practical: A Portable Pathname Library

;   :file-pathname-p
;   :directory-p
;   :file-p
(in-package :pcl.pathname)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  "Is P in directory form?"
  (and 
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  "Converts any pathname to directory form"
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p pathname))
	(make-pathname 
	 :directory (append (or (pathname-directory pathname)
				(list :relative))
			    (list (file-namestring pathname)))
	 :name      nil
	 :type      nil
	 :defaults  pathname)
	pathname)))

(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname 
   :directory (append (pathname-directory wildcard)
		      (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directy names."))
  (let ((wildcard (directory-wildcard dirname)))
    #+(or sbcl cmu lispworks)
    (directory wildcard)
    #+openmcl
    (directory wildcard :directories t)
    #+allegro
    (directory wildcard :directories-are-files nil)
    #+clisp
    (nconc (directory wildcard)
	   (directory (clisp-subdirectories-wildcard wildcard)))
    #+(not (or sbcl cmu lispworks openmcl allegro clisp))
    (error "LIST-DIRECTORY not implemented")))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname 
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  #+clisp
  (or (ignore-errors 
       (probe-file (pathname-as-file pathname)))
      (ignore-errors
       (let ((directory-form (pathname-as-directory pathname)))
	 (when (ext:probe-directory directory-form)
	   directory-form)))))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels ((walk (name)
	     (cond
	       ((directory-pathname-p name) 
		(when (and directories (funcall test name))
		  (funcall fn name))
		(dolist (x (list-directory name))
		  (walk x)))
	       ((funcall test name)
		(funcall fn name)))))
    (walk (pathname-as-directory dirname))))

