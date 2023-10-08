(defpackage cl-top
  (:use :cl)
  (:export :cmd))
(in-package :cl-top)

;; blah blah blah.

;; (ql:quickload 'cl-ppcre)
(defun list-proc-path ()
  (remove-if-not #'(lambda (x)
		     (cl-ppcre:all-matches-as-strings
		      "/proc/[0-9]+/*" (namestring x)))
		 (directory "/proc/*"))
  )

(setf status-file-path-list (mapcar #'(lambda (x) (merge-pathnames x "/status")) (list-proc-path)))

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

;; List header name
(defun list-header-name ()
  (mapcar #'(lambda (x) (car (split x #\:)))
	  (split (slurp (car status-file-path-list)) #\Newline)))
