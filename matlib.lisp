;;inner-product
(defun v-ip (v1 v2)
  (cond ((= (length v1) (length v2)) (apply #'+ (map 'list #'* v1 v2)))
        (t (error "length of two vectors not equal")) ))

;;symmetric-matrix
(defun transpose (mat)
  (let* ((mat-row (array-dimension mat 0))
         (mat-col (array-dimension mat 1))
         (result (make-array (list mat-col mat-row))) )
    (loop for x below mat-row
          do (loop for y below mat-col
                   do (setf (aref result y x)
                            (aref mat x y) )))
    result ))

;;get-matrix-row
(defun row (mat index)
  (let* ((mat-col (array-dimension mat 1))
         (result (make-array mat-col)) )
    (loop for x below mat-col
          do (setf (aref result x)
                   (aref mat index x) ))
    result ))

;;linear-transformation
(defun mat-lt (mat1 mat2)
  (let ((mat1-row (array-dimension mat1 0))
        (mat1-col (array-dimension mat1 1)) )
    (case (array-rank mat2)
      ;matrix-vector
      (1 (let ((v-row (array-dimension mat2 0)))
           (cond ((not (= mat1-col v-row))
                  (error "tow operands are mismatch for linear-transformation") )
                 (t (let ((result (make-array mat1-row)))
                      (loop for x below mat1-row
                            do (setf (aref result x)
                                     (v-ip (row mat1 x) mat2) ))
                      result )))))
      ;matrix-matrix
      (2 (let* ((mat2-tr (transpose mat2))
                (mat2-row (array-dimension mat2-tr 0))
                (mat2-col (array-dimension mat2-tr 1)) )
           (cond ((not (= mat1-col mat2-col))
                  (error "tow operands are mismatch for linear-transformation") )
                 (t (let ((result (make-array (list mat1-row mat2-row))))
                      (loop for x below mat1-row
                            do (loop for y below mat2-row
                                     do (setf (aref result x y)
                                              (v-ip (row mat1 x) (row mat2-tr y)) )))
                      result )))))
      (otherwise (error "invalid operands")) )))

;;matrix-matrix operation builder
(defun mat-each-elem-operator (fn)
  #'(lambda (mat1 mat2)
      (cond ((and (= 1 (array-rank mat1)) (= 1 (array-rank mat2))) ;vector-vector
             (let ((v1-row (array-dimension mat1 0))
                   (v2-row (array-dimension mat2 0)) )
               (cond ((not (= v1-row v2-row))
                      (error "size of two vectors not equal") )
                     (t (let ((result (make-array v1-row)))
                          (loop for x below v1-row
                                do (setf (aref result x)
                                         (funcall fn (aref mat1 x) (aref mat2 x)) ))
						  result )))))
            ((and (= 2 (array-rank mat1)) (= 2 (array-rank mat2))) ;matrix-matrix
             (let ((mat1-row (array-dimension mat1 0))
                   (mat1-col (array-dimension mat1 1))
                   (mat2-row (array-dimension mat2 0))
                   (mat2-col (array-dimension mat2 1)) )
               (cond ((not (and (= mat1-row mat2-row) (= mat1-col mat2-col)))
                      (error "size of two matrix not equal") )
                     (t (let ((result (make-array (list mat1-row mat1-col))))
                          (loop for x below mat1-row
                                do (loop for y below mat1-col
                                         do (setf (aref result x y)
                                                  (funcall fn (aref mat1 x y) (aref mat2 x y)) )))
                          result )))))
			(t (error "invalid arguments")) )))

(defun mat-mat+ (mat1 mat2)
  (funcall (mat-each-elem-operator #'+) mat1 mat2))

(defun mat-mat- (mat1 mat2)
  (funcall (mat-each-elem-operator #'-) mat1 mat2))

(defun mat-mat* (mat1 mat2)
  (funcall (mat-each-elem-operator #'*) mat1 mat2))

(defun mat-mat/ (mat1 mat2)
  (funcall (mat-each-elem-operator #'/) mat1 mat2))

;;matrix-scalar operation builder
(defun mat-each-elem-operator-s (fn)
  #'(lambda (mat scalar)
      (cond ((= 1 (array-rank mat)) ;vector-scalar
			 (let* ((v-row (array-dimension mat 0))
				   (result (make-array (list v-row))) )
			   (loop for x below v-row
					 do (setf (aref result x)
							  (funcall fn (aref mat x) scalar) ))
			   result ))

            ((= 2 (array-rank mat)) ;matrix-scalar
			 (let* ((mat-row (array-dimension mat 0))
					(mat-col (array-dimension mat 1))
					(result (make-array (list mat-row mat-col))) )
			   (loop for x below mat-row
					 do (loop for y below mat-col
							  do (setf (aref result x y)
									   (funcall fn (aref mat x y) scalar) )))
			   result ))
			(t (error "invalid arguments")) )))

(defun mat-scl+ (mat scalar)
  (funcall (mat-each-elem-operator-s #'+) mat scalar))

(defun mat-scl- (mat scalar)
  (funcall (mat-each-elem-operator-s #'-) mat scalar))

(defun mat-scl* (mat scalar)
  (funcall (mat-each-elem-operator-s #'*) mat scalar))

(defun mat-scl/ (mat scalar)
  (funcall (mat-each-elem-operator-s #'/) mat scalar))


;;test data
(setq m1 #2A((1 2 2) (6 3 4)))
(setq m2 #2A((5 2) (9 1) (2 1)))
(setq m3 #2A((3 -2 2) (1 3 7)))
(setq v1 #(1 4))
(setq v2 #(2 6))
