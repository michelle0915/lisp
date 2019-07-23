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
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (lst acc)
                (let ((rest (nthcdr n lst)))
                  (if (consp rest)
                      (rec rest (cons (subseq lst 0 n) acc))
                      (nreverse (cons lst acc))))))
    (if source (rec source nil) nil)))

(defun filter (test lst)
  (labels ((rec (lst acc)
                (if (null lst)
                    acc
                    (let ((val (funcall test (car lst))))
                      (if val
                          (rec (cdr lst) (cons (car lst) acc))
                          (rec (cdr lst) acc))))))
    (nreverse (rec lst nil))))

(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree)) (count-leaves (cdr tree))) ))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args )))

(defmacro gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                    `(,s (gensym)))
                syms)
     ,@body))

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
        collect (1+ i)))

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
  `(with-output-to-string (s)
     (let ((*print-right-margin* 40)
           (*print-miser-width* nil))
       (pprint (macroexpand-1 ',expr) s))))

(defmacro time-n (n &body body)
  (let ((loops (gensym)))
    `(time (dotimes (,loops ,n) ,@body )) ))

;(mac (time-n 10 (print 'hihihhihihihihihihhiih)))

; macros
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar (lambda (s)
                       `(,s (gensym ,(subseq (symbol-name s)
                                             2))))
                     syms)
         ,@body))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
      `(let ,(mapcar #'list (list ,@gs) (list ,@os))
         ,(progn ,@body)))))

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
      ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))
(set-dispatch-macro-character
  #\# #\" #'|#"-reader|)
;"

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
          ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                    t
                    (list (car d)))
                (apply (lambda ,@(cdr d))
                       ,(if (eq t (car d))
                            g!args
                            `(cdr ,g!args)))))
           ds))))

;; anaphor
(defmacro alambda (args &rest body)
  `(labels ((self ,args ,@body))
     #'self))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
                  collect (symb 'a i))
     ,(funcall (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
  #\# #\` #'|#`-reader|)

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states) #',(caar states))))

(defun let-binding-transform (bs)
  (if bs
      (cons (cond ((symbolp (car bs))
                   (list (car bs)))
                  ((consp (car bs))
                   (car bs))
                  (t
                    (error "Bad let bindings")))
            (let-binding-transform (cdr bs)))))

(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons '(this)
                       (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
         (:pandoric-get (sym)
           ,(pandoriclet-get letargs))
         (:pandoric-set (sym val)
           ,(pandoriclet-set letargs))
         (t (&rest args)
           (apply this args))))))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error "Unknown pandoric get: ~a"
               sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                  (setq ,(car a1) val))
               letargs)
     (t (error "Unknown pandoric set: ~a"
               sym))))

(declaim (inline get-pandric))

(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

(defmacro! with-pandoric (syms o!box &rest body)
  `(symbol-macrolet
     (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                syms))
     ,@body))

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
    (setq this new)))

(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
     (setq this ,new)))

(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
         this (lambda ,largs ,@body)
         self (dlambda
                (:pandoric-get (sym)
                  ,(pandoriclet-get pargs))
                (:pandoric-set (sym val)
                  ,(pandoriclet-set pargs))
                (t (&rest args)
                  (apply this args)))))))
