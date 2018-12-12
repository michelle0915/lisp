(defparameter *rnd-initialized* nil)

(defun rnd (&optional (max 100) (min 1))
  (unless *rnd-initialized*
	(setq *rnd-initialized* t) )
  (+ (random (1+ (- max min)))
	 min) )

(defun rndseq (num &optional (max 100) (min 1))
  (loop for i below num
		collect (rnd max min) ))

(defun numseq (num)
  (loop for i below num
		collect (1+ i) ))

(defun numarray (bottom top step)
  (let ((result nil))
	(labels ((fn (bottom top step result)
				(setq result (cons bottom result)) 
				(if (> (+ bottom step) top)
				  result
				  (fn (+ bottom step) top step result) )))
	  
	  (nreverse (fn bottom top step result)) )))

(defun sum (seq)
  (reduce #'+ seq) )
