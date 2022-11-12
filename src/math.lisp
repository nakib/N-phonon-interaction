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

(defun demux (i grid)
  "Demultiplexes a 0-based index i into integer 0-based triplet (x y z).
The x index cycles first, followed by y and then z."
  
  (multiple-value-bind (quotient x) (floor i (first grid))
    (multiple-value-bind (z y) (floor quotient (second grid))
      (list x y z))))

(defun mux (q grid)
  "Inverse operation of the demux function."
  
  (+ (first q)
     (* (first grid) (+ (second q) (* (second grid) (third q))))))

(defun combine (operator q1 q2)
  "Combine 2 vectors under the action of the operator."

  (mapcar operator q1 q2))

(defun conserve-quasimomentum (indices operators grid)
  "Combines with +/- operators a list of wave vectors, represented 
as 0-based multiplexed indices. The resulting wave vector is folded
back to the first Brillouin zone."

  (let ((qs (mapcar (lambda (index) (demux index grid)) indices))
	(final-q '(0 0 0)))
    ;; Sum all but the last wave vector
    (loop for iq from 1 to (- (length qs) 1)
	  do (setq final-q (combine (nth iq operators) final-q (nth iq qs))))
    (mux (mapcar #'mod final-q grid) grid)))

(defun delta (e1 e2 sigma)
  "Returns the sigma-spread Gaussian representation of the delta function."
  
  (/ (exp (* -0.5 (expt (/ (- e1 e2) sigma) 2)))
     (* sigma (sqrt (* 2 pi)))))

(defun cartesian-product (lols)
  "Returns the Cartesian product of a list of lists (lols)."
  (if (null lols)
      (list nil)
      (loop for i in (first lols)
            append (loop for j in (cartesian-product (rest lols))  
                         collect (cons i j)))))
