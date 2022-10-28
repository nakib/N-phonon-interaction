(defun choose (n r)
  "Returns the binomial expansion coefficient n choose r."

  (if (and (>= r 0) (>= n 0) (>= n r))
      (/ (! n) (* (! r) (! (- n r))))
      nil))

(defun ! (n)
  "Returns factorial of n."

  (cond ((< n 0) nil)
	((= n 0) 1)
	(t (* n (! (1- n))))))

; TODO check why there's loss of precision...
(defun add-and-fold (q1 q2)
  "Add two wave vectors in crystal coordinates and fold into the first
Brillouin zone."
  
  (mapcar #'mod 
	  (mapcar #'+ q1 q2) '(1 1 1)))

(defun delta (e1 e2 sigma)
  "Returns the sigma-spread Gaussian representation of the delta function."
  
  (/ (exp (* -0.5 (expt (/ (- e1 e2) sigma) 2)))
     (* sigma (sqrt (* 2 pi)))))
