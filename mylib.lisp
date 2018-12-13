(proclaim '(inline sum square last1 append1))

;;math
(defun sum (seq)
  (reduce #'+ seq) )

(defun square (x)
  (expt x 2) )

(defun norm (vectr)
  (sqrt (sum (map 'list #'square vectr))) )

;;list operation
(defun last1 (lst)
  (car (last lst)) )

(defun append1 (lst obj)
  (append lst (list obj)) )

;;other utils
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

(defun num2bin (num)
  (labels ((fn (num)
               (if (= num 0)
                   nil
                   (multiple-value-bind (rem mod) (truncate num 2)
                     (cons mod (fn rem)) ))))
    (nreverse (fn num)) ))

;;test, debug
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro time-n (n &body body)
  (let ((loops (gensym)))
    `(time (dotimes (,loops ,n) ,@body )) ))
