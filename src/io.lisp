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
