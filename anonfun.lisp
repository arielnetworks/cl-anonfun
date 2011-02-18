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
           (if (string= spec "") 1 (parse-integer spec)))))

  (defun extract-args (body)
    (cond ((arg-p body) (list body))
          ((consp body)
           (remove-duplicates
            (append (extract-args (car body))
                    (extract-args (cdr body)))
            :test #'symbol=))))

  (defun sort-args (args)
    (sort args (lambda (x y)
                 (cond
                   ((symbol= x '%) t)
                   ((symbol= y '%) nil)
                   (t (< (arg-num x) (arg-num y)))))))

  (defun make-ignore-vars-between (start end)
    (when end
      (loop for n from (1+ start) to (1- end)
            collect (gensym (format nil "IGNORE_~D_" n)))))

  (defun make-lambda-list (narg args)
    (let (aux (rest (car (member '%& args :test #'symbol=))))
      (setf args (sort-args (remove rest args)))
      (when (and (symbol= (car args) '%)
                 (symbol= (cadr args) '%1))
        (setf aux `(,(car args) ,(cadr args)))
        (pop args))
      (loop with x = 0
            for arg in args
            for y = (arg-num arg)
            for ignores = (make-ignore-vars-between x y)
            do (setf x y)
            append ignores into lambda-list
            collect arg into lambda-list
            append ignores into ignore-vars
            finally
         (when narg
           (let ((ignores (make-ignore-vars-between x (1+ narg))))
             (setf lambda-list (append lambda-list ignores))
             (setf ignore-vars (append ignore-vars ignores))))
         (when rest
           (setf lambda-list (append lambda-list `(&rest ,rest))))
         (when aux
           (setf lambda-list (append lambda-list `(&aux ,aux))))
         (return (values lambda-list ignore-vars)))))

  (defun make-fn (narg form)
    (multiple-value-bind (lambda-list ignore-vars)
        (make-lambda-list narg (extract-args form))
      `(lambda ,lambda-list
         ,@(when ignore-vars
             `((declare (ignore ,@ignore-vars))))
         ,form))))

(defmacro fn (form)
  (make-fn nil form))

(defmacro fnn (narg form)
  (make-fn narg form))

(defun fn-reader (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setf numarg 1))
  (let ((form (read stream t nil t)))
    (if (integerp form)
        `(fnn ,form ,(read stream t nil t))
        `(fn ,form))))

(defun %enable-fn-syntax ()
  (set-dispatch-macro-character #\# #\% #'fn-reader))

(defmacro enable-fn-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-fn-syntax)))
