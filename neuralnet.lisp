(load "mylib.lisp")
(load "matlib.lisp")

(defun cal-y (x1 x2 w1 w2 b)
  (+ b (v-ip (list x1 x2) (list w1 w2))) )

(defun fire (x1 x2 w1 w2 b)
  (if (<= (cal-y x1 x2 w1 w2 b) 0)
	0
	1 ))

(defun ptron-and (x1 x2)
  (let ((w1 0.5)
		(w2 0.5)
		(b -0.7) )
	(fire x1 x2 w1 w2 b) ))

(defun ptron-or (x1 x2)
  (let ((w1 0.5)
		(w2 0.5)
		(b -0.3) )
	(fire x1 x2 w1 w2 b) ))

(defun ptron-nand (x1 x2)
  (let ((w1 -0.5)
		(w2 -0.5)
		(b 0.7) )
	(fire x1 x2 w1 w2 b) ))

(defun ptron-xor (x1 x2)
  (ptron-and (ptron-or x1 x2) (ptron-nand x1 x2)) )

;;activation function
(defun a (x w b)
  (+ b (v-ip x w)) )

(defun h-step (a)
  (if (numberp a)
	(if (<= a 0)
	  0
	  1 )
	(map 'vector #'h-step a) ))

(defun h-sigmoid (a)
  (if (numberp a)
	(/ 1 (1+ (exp (* -1 a))))
	(map 'vector #'h-sigmoid a) ))

(defun h-relu (a)
  (if (numberp a)
	(max a 0)
	(map 'vector #'h-relu a) ))

(defun o-identity (x) x)

(defun o-softmax (x)
  (let* ((small-x (mat-scl- x (aref (sort x #'>) 0)))
		 (denom (sum (map 'vector #'exp small-x))) )
	(map 'vector #'(lambda (a) (/ (exp a) denom)) small-x) ))

;;3-layer neural network
(defparameter w1 #2A((0.1 0.2) (0.3 0.4) (0.5 0.6)))
(defparameter b1 #(0.1 0.2 0.3))
(defparameter inner-fn1 #'h-sigmoid)
(defparameter w2 #2A((0.1 0.2 0.3) (0.4 0.5 0.6)))
(defparameter b2 #(0.1 0.2))
(defparameter inner-fn2 #'h-sigmoid)
(defparameter w3 #2A((0.1 0.2) (0.3 0.4)))
(defparameter b3 #(0.1 0.2))
(defparameter inner-fn3 #'(lambda (x) x)) ;恒等関数
(defparameter x #(1.0 0.5))

(setq network (list (list w1 b1 inner-fn1)
					(list w2 b2 inner-fn2)
					(list w3 b3 inner-fn3) ))

(defun forward (network x)
  (if (null network)
	x
	(let ((w (caar network))
		  (b (cadar network))
		  (inner-fn (caddar network)))
	  (forward (cdr network) (funcall inner-fn (mat-mat+ b (mat-lt w x)))) )))

;;output-plot
(defun printxy (x y &optional strm)
  (when (car x)
	(format (if strm strm t) "~d ~d~%" (car x) (car y))
	(printxy (cdr x) (cdr y) strm) ))

(defun outputfile (fname fn x)
  (let ((y (funcall fn x)))
	(with-open-file (out-strm
					  fname
					  :direction
					  :output)
	  (printxy x y out-strm) )))


;(outputfile "relu.dat" #'h-relu (numarray -5 5 0.1))


