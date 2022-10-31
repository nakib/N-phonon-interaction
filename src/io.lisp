;Prevent possible exploitation by the #. read macro.
(setq *read-eval* nil)

(defun string-to-float (line)
  "Convert string to float."
  
  (with-input-from-string (string line)
    (loop for number = (read string nil nil)
	  while number
	  collect number)))

(defun read-file (file)
  "Read from float data from file."
  
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
	  collect (string-to-float line))))

(defun write-file (file data)
  "Write data to file."

  (with-open-file (stream file
			  :if-does-not-exist :create
			  :if-exists :supersede
			  :direction :output)
    (loop for rowdata in data
	  do (format stream "    ~{~f~^ ~}" rowdata)
	     (format stream "~%"))))
