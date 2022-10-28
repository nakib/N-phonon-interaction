(load "io.lisp")
(load "interaction_diagrams.lisp")

(defvar *q-mesh* '(6 6 6))

;;NOTE: The behaviors of first and last on *irreducible-wavevector-list* are different.
;; For the latter an extra pair of brackets appear. No issue with using nth.
(defvar *ibz-wavevector-list*
  (read-file "../wGaN_6x6x6data/ph.wavevecs_ibz"))

(defvar *fbz-wavevector-list*
  (read-file "../wGaN_6x6x6data/ph.wavevecs_fbz"))

(defvar *ibz-energy-list*
  (read-file "../wGaN_6x6x6data/ph.ens_ibz"))

(defvar *fbz-energy-list*
  (read-file "../wGaN_6x6x6data/ph.ens_fbz"))

(defun state-energy (state energy-list)
  "Returns energy of the phonon state from energy list."
  
  (nth (second state)
       (nth (first state) energy-list)))

(defun wavevector (i wavevector-list)
  "Returns ith wavevector from the wavevector list."
  
  (nth i wavevector-list))

(defun density-of-states-at-energy (energy fbz-energy-qlist sigma)
  "Returns the phonon density of states at a given sampling energy."
  
  (let ((sum 0.0))
    (loop for fbz-energies-q in fbz-energy-qlist
	  do (loop for fbz-energy in fbz-energies-q
		   do (incf sum (delta energy fbz-energy sigma))))
    (/ sum (length fbz-energy-qlist))))
		      
(defun density-of-states (ibz-energies-qlist fbz-energy-qlist sigma)
  "Returns the phonon mode resolved density of states."
  
  (loop for energies-q in ibz-energies-qlist
	collect (mapcar
		 (lambda (energy)
		   (density-of-states-at-energy energy fbz-energy-qlist sigma))
		 energies-q)))
