(defpackage cl-top
  (:use :cl :cl-ppcre)
  (:export :main-loop))
(in-package :cl-top)

;; blah blah blah.

;; (ql:quickload 'cl-ppcre)

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


(defun kv-format-parse (s)
  (let* ((kv (split s #\:))
	 (key (car kv))
	 (value (format nil "~{~A ~}" (mapcar #'(lambda (x) (string-trim "	" x)) (cdr kv)))))
    (values key value)
    )
  )

(defun proc-intern (pid key)
  (intern (format nil "~A/~A" pid (string-downcase key)))
  )

(defun trim-pid-from-path (path)
  (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" path))
  )

(defun set-table-from-command (tbl dirpath)
  (let ((content (slurp (format nil "~Acmdline" dirpath)))
	(pid (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" dirpath))))
    (setf (gethash (proc-intern pid "command") tbl)
	  (cl-ppcre:regex-replace "\n" content ""))
    )
  )

(defun set-table-from-status (tbl dirpath)
  (let ((content-lines (split (slurp (format nil "~Astatus" dirpath)) #\Newline))
	(kv '())
	(key "")
	(value "")
	(pid (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" dirpath)))
	)
    (loop for l in content-lines
	  do (multiple-value-bind (key value)
		 (kv-format-parse l)
	       (setf (gethash (proc-intern pid key) tbl) value))
	  )
    tbl
   )
  )


(defun set-table-from-statm (tbl dirpath)
  (let* ((statm-path (format nil "~Astatm" dirpath))
	 (content (slurp statm-path))
	 (pid (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" dirpath)))
	 (vals (split (car (split content #\Newline)) #\Space)))
    (setf (gethash (proc-intern pid "virt") tbl) (nth 0 vals))
    (setf (gethash (proc-intern pid "res") tbl) (nth 1 vals))
    (setf (gethash (proc-intern pid "shr") tbl) (nth 2 vals))
    (setf (gethash (proc-intern pid "mem") tbl) (nth 2 vals))
    ;; mem 0 1?
    tbl
    )
  )

(defun set-table-from-stat (tbl dirpath)
  (let* ((stat-path (format nil "~Astat" dirpath))
	 (content (slurp stat-path))
	 (pid (parse-integer (cl-ppcre:scan-to-strings "[0-9]+" dirpath)))
	 (vals (split (car (split content #\Newline)) #\Space)))
    (setf (gethash (proc-intern pid "s") tbl) (nth 2 vals))
    (setf (gethash (proc-intern pid "pr") tbl) (nth 17 vals))
    (setf (gethash (proc-intern pid "ni") tbl) (nth 18 vals))
    tbl
    )
  )

(defun set-from-mem (tbl)
  (let* ((content (slurp "/proc/meminfo"))
	 (content-lines (split content #\Newline))
	 (kv '()))
    (loop for l in content-lines
	  do (multiple-value-bind (key value)
		 (kv-format-parse l)
	       (when (> (length key) 0)
		 (setf (gethash (intern key) tbl) value))
	       )
	  )
    )
  )

(defun set-from-cpu (tbl)
  (let* ((content (slurp "/proc/cpuinfo"))
	 (content-lines (split content #\Newline))
	 (kv '()))
    (loop for l in content-lines
	  do (multiple-value-bind (key value)
		 (kv-format-parse l)
	       (when (> (length key) 0)
		 (setf (gethash (intern key) tbl) value))
	       )
	  )
    )
  )

(defun list-proc-path ()
  (remove-if-not #'(lambda (x)
		     (cl-ppcre:all-matches-as-strings
		      "^/proc/[0-9]+/$" (namestring x)))
		 (directory "/proc/*"))
  )

;; process -> (key value) from proc file
(defun generate-table ()
  (let ((proc-path-list (list-proc-path))
	(tbl (make-hash-table))
	(pid-list (mapcar #'(lambda (p) (trim-pid-from-path (namestring p)))
			  (list-proc-path))))
    (set-from-cpu tbl)
    (set-from-mem tbl)
    (loop for p in (remove-if
		    #'(lambda (p)
			(<= (length p) 0))
		    (mapcar #'(lambda (p)
				(namestring p))
			    proc-path-list))
	  do (progn
	       ;; こいつらのていぎがそんざいしない
	       (set-table-from-status tbl p)
	       (set-table-from-stat tbl p)
	       (set-table-from-statm tbl p)
	       (set-table-from-command tbl p)
	       )
	  )
    (values tbl pid-list)
    )
  )

(defun print-proc-table-header (tbl pid-list)
  (format t "none~%")
  )

(defparameter +line-print-fmt+ "~8A~8A~8A~8A~8A~8A~8A~8A~8A~8A~8A~8A~%")

(defun print-proc-table-body-line (tbl pid)
  (let ((cmd (gethash (proc-intern pid "command") tbl)))
    (format t
	    +line-print-fmt+
	    pid
	    (string-trim '(#\Space #\Newline #\Tab)
			 (car (split (gethash (proc-intern pid "uid") tbl "?") #\Tab)))
	    (string-trim '(#\Space #\Newline #\Tab)  (gethash (proc-intern pid "pr") tbl "?"))
	    (string-trim '(#\Space #\Newline #\Tab)  (gethash (proc-intern pid "ni") tbl "?"))
	    (string-trim '(#\Space #\Newline #\Tab)  (gethash (proc-intern pid "virt") tbl "?"))
	    (string-trim '(#\Space #\Newline #\Tab)  (gethash (proc-intern pid "res") tbl "?"))
	    (string-trim '(#\Space #\Newline #\Tab)  (gethash (proc-intern pid "shr") tbl "?"))
	    (string-trim '(#\Space #\Newline #\Tab)  (gethash (proc-intern pid "s") tbl "?"))
	    "none"
	    "none"
	    "none"
	    "none"
	    ;;(if (> (length cmd) 10)
	    ;;	(subseq cmd 0 10))
	    ;;
	    )
    )

  )

(defun print-proc-table-body (tbl pid-list)
  (format t
	  +line-print-fmt+
	  "PID"
	  "USER"
	  "PR"
	  "NI"
	  "VIRT"
	  "RES"
	  "SHR"
	  "S"
	  "CPU"
	  "MEM"
	  "TIME"
	  "COMMAND"
	  )
  (loop for pid in pid-list do (print-proc-table-body-line tbl pid))
  )

(defun take (n lst)
  (if (> n 0)
      (cons (car lst) (take (1- n) (cdr lst)))))

;; print top all
(defun print-proc-table ()
  (multiple-value-bind (tbl pid-list) (generate-table)
    (let ((top-pid-list (take 20 pid-list)))
      (print-proc-table-header tbl top-pid-list)
      (format t "=====================================~%")
      (print-proc-table-body tbl top-pid-list)
      )

    )
  )

;; run every second.
(defun main-loop ()
  (loop
    do (progn
	 (format t "~c[2J" #\ESC)
	 (print-proc-table)
	 (sleep 1))
    )
  )
