(in-package :cl-anonfun)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol= (a b)
    "Package insensitive symbol comparison."
    (and (symbolp a) (symbolp b)
         (or (eq a b)
             (string= (symbol-name a) (symbol-name b)))))

  (defun arg-p (arg)
    (and (symbolp arg)
         (eq (elt (symbol-name arg) 0) #\%)))

  (defun arg-num (arg)
    (and (arg-p arg)
         (let ((spec (subseq (symbol-name arg) 1)))
           (if (string= spec "") 0 (parse-integer spec)))))

  (defun extract-args (body)
    (cond ((arg-p body) (list body))
          ((consp body)
           (remove-duplicates
            (append (extract-args (car body))
                    (extract-args (cdr body)))
            :test #'symbol=))))

  (defun sort-args (args)
    (sort args (lambda (x y) (< (arg-num x) (arg-num y)))))

  (defun make-ignore-vars-between (start end)
    (when end
      (loop for n from (1+ (arg-num start)) to (1- (arg-num end))
            collect (gensym (format nil "IGNORE_~D_" n)))))

  (defun make-lambda-list (args)
    (let (aux (rest (car (member '%& args :test #'symbol=))))
      (setf args (sort-args (remove rest args)))
      (when (and (symbol= (car args) '%)
                 (symbol= (cadr args) '%1))
        (setf aux `(,(car args) ,(cadr args)))
        (pop args))
      (loop with a = '%0
            for b in args
            for ignores = (make-ignore-vars-between a b)
            do (setf a b)
            append ignores into lambda-list
            collect b into lambda-list
            append ignores into ignore-vars
            finally
         (when rest
           (setf lambda-list (append lambda-list `(&rest ,rest))))
         (when aux
           (setf lambda-list (append lambda-list `(&aux ,aux))))
         (return (values lambda-list ignore-vars))))))

(defmacro fn (&rest body)
  (multiple-value-bind (lambda-list ignore-vars)
      (make-lambda-list (extract-args body))
    `(lambda ,lambda-list
       ,@(when ignore-vars
           `((declare (ignore ,@ignore-vars))))
       ,body)))

(defun fn-reader (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setf numarg 1))
  `(fn ,@(read stream t nil t)))

(defun enable-fn-syntax ()
  (set-dispatch-macro-character #\# #\% #'fn-reader))
