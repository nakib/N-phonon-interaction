(load "math.lisp")

(defun sign-metric (x l)
  (if (<= x l) '+ '-))

(defun coallescence-side-degeneracy (x)
  (if (< x 3) 0 (choose (1- x) 2)))

(defun decay-side-degeneracy (x)
  (if (< x 3) 0 (choose (1- x) 2)))

(defun delta-arguments (number-of-phonons diagram-index)
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
