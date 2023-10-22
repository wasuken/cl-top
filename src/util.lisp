(in-package :cl-user)
(defpackage cl-top.util
  (:use :cl :cl-ppcre)
  (:export #:round-to-digits
	   #:slurp
	   #:split
	   #:remove-not-char
	   #:kv-format-parse
	   #:proc-intern
	   #:trim-pid-from-path
	   #:global-stat-format-parse
	   #:safe-zero
	   #:now-datetime-format-string
	   #:mformat
	   #:take
	   #:->
	   )
  )
(in-package :cl-top.util)

;; (ql:quickload 'cl-ppcre)

(defun round-to-digits (number digits)
  (let ((factor (expt 10 digits)))
    (float (/ (round (* number factor)) factor))))

(defun slurp (filepath)
  (with-open-file (out filepath :direction :input)
    (format nil "~{~A~%~}" (loop for l = (read-line out nil nil) while l collect l))
    )
  )

(defun split (s sep)
  (let ((result '())
	(readed-s ""))
    (loop for c across s
	  do (cond ((eq sep c)
		    (setf result (cons readed-s result))
		    (setf readed-s ""))
		   (t
		    (setf readed-s (concatenate 'string readed-s (format nil "~a" c))))))
    (when (> (length readed-s) 0)
      (setf result (cons readed-s result))
      (setf readed-s ""))
    (reverse result)))

(defun remove-not-char (s)
  (cl-ppcre:regex-replace-all "\\W" s ""))

(defun kv-format-parse (s)
  (let* ((kv (split s #\:))
	 (key (remove-not-char (string-downcase (car kv))))
	 (value (format nil "~{~A ~}"
			(mapcar #'(lambda (x) (string-trim '(#\Newline #\Tab #\Space) x)) (cdr kv))))
	 )
    (values key value)
    )
  )

(defun proc-intern (pid key)
  (intern (format nil "~A/~A" pid (string-downcase key)))
  )

(defun trim-pid-from-path (path)
  (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" path))
  )

(defun global-stat-format-parse (s)
  (let* ((ary (split s #\ ))
	 (iary (mapcar #'parse-integer
		       (remove-if #'(lambda (x) (< (length x) 1)) (cdr ary)))))
    (values (car ary)
	    (apply #'+ iary)
	    iary)))

(defun now-datetime-format-string ()
  (let ((current-time (get-universal-time)))
    (format nil "~A"
	    (multiple-value-bind (second minute hour date month year)
		(decode-universal-time current-time)
	      (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
		      year month date hour minute second))))
)

(defmacro safe-zero (a b a-bind b-bind body default)
  `(let ((,a-bind ,a)
	 (,b-bind ,b))
     (if (or (zerop ,a-bind) (zerop ,b-bind))
	 ,default
	 ,body)
     )
  )

(defun take (n lst)
  (if (> n 0)
      (cons (car lst) (take (1- n) (cdr lst)))))

(defmacro -> (v &rest body)
  (let ((rst '()))
    (loop for x in body
	  do (setf rst `(,x ,(if (null rst)
				 v
				 rst)
			    )
		   ))
    rst)
  )
