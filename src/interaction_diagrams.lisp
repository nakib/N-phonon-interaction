(load "math.lisp")

(defun sign-metric (x l)
  (if (<= x l) '+ '-))

(defun flip-sign (signs)
  "Returns array of flipped signs."
  (mapcar (lambda (s)
	    (cond ((equal s '+) '-)
		  ((equal s '-) '+)
		  (t s)))
	  signs))

(defun coallescence-side-degeneracy (x)
  (if (< x 3) 0 (choose (1- x) 2)))

(defun decay-side-degeneracy (x)
  (if (< x 3) 0 (choose (1- x) 2)))

(defun delta-arguments-signs (number-of-phonons diagram-index)
  "Returns a list of signs for the delta function arguments
for a given phonon scattering diagram index and the total
number of phonons."

  (if (and (>= number-of-phonons 2) (>= diagram-index 1) (< diagram-index number-of-phonons))
      (let ((argument-sign-list '(-)))
	(loop for term from 2 to number-of-phonons
	      do (push (sign-metric diagram-index (1- term)) argument-sign-list))
	(reverse argument-sign-list))))

(defun integrated-diagram-weights (number-of-phonons)
  "Returns for a given total number of phonons a list of weights 
for all the interaction diagrams due to the double counting of 
equivalent processes under integration of all but the inital phonon. 
I know that was a mouthful. The examples below illustrate better.


Example:
3-phonon scattering:
1 -> 2 + 3 : 1 x 2
1 + 2 -> 3 : 0 x 2

4-phonon scattering:
1 -> 2 + 3 + 4 : 3 x 2
1 + 2 -> 3 + 4 : 1 x 2
1 + 2 + 3 -> 4 : 1 x 2

5-phonon scattering:
1 -> 2 + 3 + 4 + 5 : 6 x 2
1 + 2 -> 3 + 4 + 5 : 3 x 2
1 + 2 + 3 -> 4 + 5 : 2 x 2
1 + 2 + 3 + 4 -> 5 : 3 x 2

n-phonon scattering:
1 -> 2 + 3 + ... + n-1 + n : nC2 x 2
1 + 2 -> 3 + ... + n-1 + n : (n-1)C2 x 2
1 + 2 + 3 -> 4 + ... + n-1 + n : [2C2 + (n-2)C2] x 2
1 + 2 + 3 + 4 -> 5 + ... + n-1 + n : [3C2 + (n-3)C2] x 2
.
.
.
"
  (loop for diagram-index from 1 to (1- number-of-phonons)
	collect (max 1 (* 2 (+ (coallescence-side-degeneracy diagram-index) (decay-side-degeneracy (- number-of-phonons (1- diagram-index))))))))

"
(1/Nq^(N-1))\sum_q2...qN \sum_s2...sN \delta[-e(s1q1) + e2(s2q2) ... +/- eN(sNqN)]

"
"
(defun phase-space-at-energy (number-of-phonons
			      energy
			      fbz-energy-qlist
			      sigma))
"

(defun phase-space (number-of-phonons
		    grid
		    ibz-wavevector-list ;;fractional coordinates
		    fbz-wavevector-list ;;fractional coordinates
		    ;ibz-energies-qlist
		    ;fbz-energy-qlist
		    sigma)

  ;;TODO Factor out the let *-qlist-muxed and the interaction N-tuplet creation from below. These are diagram type independent.
  
  ;;Loop over all diagrams
  (loop for diagram from 1 to (1- number-of-phonons)
	do (format t "~%Diagram number = ~a~%" diagram)
	   
	   (let* ((signs (delta-arguments-signs number-of-phonons diagram))
		  (flipped-signs (flip-sign signs))
		  (ibz-qlist-muxed
		    (mapcar (lambda (q) (mux (combine '* q grid) grid)) ibz-wavevector-list))
		  (fbz-qlist-muxed
		    (mapcar (lambda (q) (mux (combine '* q grid) grid)) fbz-wavevector-list))
		  (list-of-interaction-qs '())
		  (qN-muxed nil))

	     ;;Form interaction N-tuplet of interacting wavevectors.
	     ;;These are the elements of the Cartesian product set of
	     ;;{q_1}, {q_2}, ... {q_N}.
	     (dotimes (i (1- number-of-phonons))
	       (push fbz-qlist-muxed list-of-interaction-qs))
	     (push ibz-qlist-muxed list-of-interaction-qs)

	     ;;(print list-of-interaction-qs)
	     ;;(print (cartesian-product list-of-interaction-qs))
	     	     
	     (loop for int-N-tup in (cartesian-product list-of-interaction-qs)
		   do
		      ;;Calculate quasimomentum of the last phonon wavevector
		      (setq qN-muxed (conserve-quasimomentum int-N-tup flipped-signs grid))

		      ;;Generate delta function S-expression
		      
		   ))))

