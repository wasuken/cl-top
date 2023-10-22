(in-package :cl-user)
(defpackage cl-top
  (:use #:cl #:cl-ppcre)
  (:import-from #:cl-top.util
		#:round-to-digits
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
  (:export #:main-loop))
(in-package :cl-top)

;; (ql:quickload 'cl-ppcre)

(defparameter +page-size+ 4096)

(defparameter *cpu-info-prev* '())

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
    (setf (gethash (proc-intern pid "utime") tbl) (nth 13 vals))
    (setf (gethash (proc-intern pid "stime") tbl) (nth 14 vals))
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
	       (let ((s (string-trim " " value)))
		 (when (and (> (length key) 0) (> (length s) 3))
		   (setf (gethash (intern key) tbl)
			 (subseq (string-trim " " s) 0 (- (length s) 3)))
		   )
		 )
	       )
	  )
    tbl
    )
  )


(defun set-from-stat (tbl)
  (let* ((content (slurp "/proc/stat"))
	 (content-lines (split content #\Newline))
	 (kv '()))
    (loop for l in content-lines
	  do (multiple-value-bind (key value ivalue)
		 (global-stat-format-parse l)
	       (when (> (length key) 0)
		 (setf (gethash (intern key) tbl) value)
		 (setf (gethash (intern (format nil "~As" key)) tbl) ivalue)
		 )
	       )
	  )
    tbl
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
    tbl
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
    ;; (set-from-stat (set-from-cpu (set-from-mem (set-from-uptime (set-from-loadavgs tbl)))))
    (setf tbl
	  (-> tbl
	      set-from-stat
	      set-from-cpu
	      set-from-mem
	      set-from-uptime
	      set-from-loadavgs)
	  )
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


(defun set-from-uptime (tbl)
  (setf (gethash (intern "uptime") tbl)
	(car (split (string-trim '(#\Newline)
				 (slurp "/proc/uptime"))
		    #\Space)))
  tbl
  )

(defun set-from-loadavgs (tbl)
  (setf (gethash (intern "loadavgs") tbl)
	(subseq (split (string-trim '(#\Newline)
				    (slurp "/proc/loadavg"))
		       #\Space)
		0 3))
  tbl
  )

(defun print-proc-table-header (tbl pid-list)
  ;; line 1.
  ;;; uptime
  (let* ((nowdt (now-datetime-format-string))
	 (uptime (round (/ (read-from-string (gethash (intern "uptime") tbl)) 60)))
	 (loadavgs (mapcar #'read-from-string (gethash (intern "loadavgs") tbl)))
	 (cpu-info (gethash (intern "cpus") tbl))
	 (cpu-info-now (mapcar #'- cpu-info *cpu-info-prev*))
	 (proc-status-list (mapcar #'(lambda (ppid)
				       (let ((v (gethash (intern (format nil "~A/s" ppid)) tbl)))
					 (cond ((string= "R" v)
						0)
					       ((string= "S" v)
						1)
					       ((string= "T" v)
						2)
					       ((string= "Z" v)
						3)
					       )
					 )
				       )
				   pid-list))
	 pe)
    (format t
	    "top - ~A up ~A min,  ~A user,  load average: ~A, ~A, ~A~%"
	    nowdt uptime 1 (nth 0 loadavgs) (nth 1 loadavgs) (nth 2 loadavgs))
    (format t
	    "Tasks: ~3,,@A total,   ~3,,@A running, ~3,,@A sleeping,   ~3,,@A stopped,   ~3,,@A zombie~%"
	    (length pid-list)
	    (count 0 proc-status-list)
	    (count 1 proc-status-list)
	    (count 2 proc-status-list)
	    (count 3 proc-status-list))
    (progn
      (if (null *cpu-info-prev*)
	  (format t
		  "%Cpu(s):  ~A us,  ~A sy,  ~A ni, ~A id,  ~A wa,  ~A hi,  ~A si,  ~A st~%"
		  0 0 0 0 0 0 0 0)
	  (format
	   t
	   "%Cpu(s):  ~A us,  ~A sy,  ~A ni, ~A id,  ~A wa,  ~A hi,  ~A si,  ~A st~%"
	   (nth 0 cpu-info-now)
	   (nth 1 cpu-info-now)
	   (nth 2 cpu-info-now)
	   (nth 3 cpu-info-now)
	   (nth 4 cpu-info-now)
	   (nth 5 cpu-info-now)
	   (nth 6 cpu-info-now)
	   (nth 7 cpu-info-now)
	   )
	  )
      (setf *cpu-info-prev* cpu-info)
      )
    (format t
	    "MiB Mem :    ~8,,A total,   ~8,,A free,   ~8,,A used,   ~8,,A buff/cache~%"
	    (gethash (intern "memtotal") tbl)
	    (gethash (intern "memfree") tbl)
	    ;; used
	    (- (parse-integer (gethash (intern "memtotal") tbl))
	       (parse-integer (gethash (intern "memfree") tbl))
	       (parse-integer (gethash (intern "buffers") tbl))
	       (parse-integer (gethash (intern "cached") tbl)))
	    ;; buf/cache
	    (+ (parse-integer (gethash (intern "buffers") tbl))
	       (parse-integer (gethash (intern "cached") tbl)))
	    )
    (format t "MiB Swap:    ~8,,A total,   ~8,,A free,   ~8,,A used.   ~8,,A avail Mem~%"
	    (gethash (intern "swaptotal") tbl)
	    (gethash (intern "swapfree") tbl)
	    (- (parse-integer (gethash (intern "swaptotal") tbl))
	       (parse-integer (gethash (intern "swapfree") tbl))
	       )
	    (gethash (intern "memavailable") tbl)
	    )
    )
  )

(defun proc-table-body-line (tbl pid)
  (let ((cmd (car (reverse (split (string-trim '(#\Newline) (gethash (proc-intern pid "command") tbl)) #\/)))))
    (list
     pid
     (remove-not-char (car (split (gethash (proc-intern pid "uid") tbl "?")
				  #\Tab)
			   )
		      )
     (remove-not-char (gethash (proc-intern pid "pr") tbl))
     (remove-not-char (gethash (proc-intern pid "ni") tbl))
     (round (/ (* (parse-integer
		   (remove-not-char (gethash (proc-intern pid "virt") tbl)))
		  +page-size+)
	       (float 1024)))
     (round (/ (* (parse-integer
		   (remove-not-char (gethash (proc-intern pid "res") tbl)))
		  +page-size+)
	       (float 1024)))
     (round (/ (* (parse-integer
		   (remove-not-char (gethash (proc-intern pid "shr") tbl)))
		  +page-size+)
	       (float 1024)))
     (remove-not-char (gethash (proc-intern pid "s") tbl))
     (safe-zero (parse-integer (gethash (proc-intern pid "utime") tbl))
		(parse-integer (gethash (proc-intern pid "stime") tbl))
		utime
		stime
		(round-to-digits
		 (* (/ (+ utime stime)
		       (* (parse-integer (gethash (intern "cpucores") tbl))
			  (gethash (intern "cpu") tbl)))
		    100.0)
		 4)
		0)
     (safe-zero (round (/ (* (parse-integer
			      (remove-not-char (gethash (proc-intern pid "res") tbl)))
			     +page-size+)
			  (float 1024)))
		(parse-integer (gethash (intern "memtotal") tbl))
		res
		mem-total
		(round-to-digits (float (* (/ res mem-total) 100)) 1)
		0)
     (safe-zero (parse-integer (gethash (proc-intern pid "utime") tbl))
		(parse-integer (gethash (proc-intern pid "stime") tbl))
		utime
		stime
		(round-to-digits (float (/ (+ utime stime) 100)) 4)
		0)
     (cond ((> (length cmd) 30)
	    (subseq cmd 0 30))
	   ((null cmd) "")
	   (t cmd))
     ))

  )


(defun print-proc-table-body (tbl pid-list)
  (format t
	  "~a[30;47m~%~{~9,,A~}~a[0m~%"
	  #\ESC
	  '("PID"
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
	    "COMMAND")
	  #\ESC
	  )
  (loop for x in (take 20
		       (sort
			(mapcar #'(lambda (pid) (proc-table-body-line tbl pid)) pid-list)
			#'>
			:key #'(lambda (x) (nth 9 x))))
	do (format t "~{~9,,A~}~%" x))
  )

;; print top all
(defun print-proc-table ()
  (multiple-value-bind (tbl pid-list) (generate-table)
    (let ((top-pid-list (take 20 pid-list)))
      (print-proc-table-header tbl pid-list)
      ;; (format t "===============================================================================================================~%")
      (print-proc-table-body tbl pid-list)
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
