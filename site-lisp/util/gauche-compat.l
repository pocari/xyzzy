;;
;; gaucheの便利なん、移植
;;

(provide "gauche-compat")
(export
 '(
   complement
   cut
   cute
   iota
   take
   circular-list
   x->string
   fold
   ))

(defun complement (pred)
  (lambda (&rest args)
    (not (apply pred args))))

(defun cut-place-holder-p (sym)
  (or (eq sym '<>)
      (eq sym '<...>)))

(defun cut-lambda-args (ph)
  (if (null ph)
      nil
    (if (eq (car ph) '<>)
	(cons (gensym)
	      (cut-lambda-args (cdr ph)))
      (cons '&rest (cons (gensym) (cut-lambda-args (cdr ph)))))))

(defun cut-lambda-body (form gensyms)
  (mapcar (lambda (elem)
	    (if (cut-place-holder-p elem)
		(pop gensyms)
	      elem))
	  form))

(defun cut-expander (args)
  (let* ((place-holders (remove-if-not #'cut-place-holder-p args))
	 (lambda-args (cut-lambda-args place-holders))
	 (lambda-body (cut-lambda-body args (remove '&rest lambda-args)))
	 (has-restargs (find '&rest lambda-args)))
    (when has-restargs
      (unless (eq '<...> (car (last place-holders)))
	(error "<...>は最後の引数にのみ有効です。")))
    (when (eq '<> (car args))
      (push 'funcall lambda-body))
    (values
     place-holders
     lambda-args
     lambda-body
     has-restargs)))

(defmacro cut (&rest args)
  (multiple-value-bind (place-holders lambda-args lambda-body has-restargs)
      (cut-expander args)
    `(lambda ,lambda-args ,lambda-body)))

(defmacro cute (proc-expr &rest args)
  "もっとよく出来ないか。"
  (let* ((place-holders (mapcar (lambda (sym) (gensym)) (remove-if-not 'cut-place-holder-p args)))
	 (tmp-place-holders (copy-list place-holders))
	 (fixed-params (remove-if-not (complement 'cut-place-holder-p) args))
	 (fixed-params-place-holders (mapcar (lambda (sym) (gensym)) fixed-params))
	 (tmp-fixed-params-place-holders (copy-list fixed-params-place-holders))
	 (lambda-body (mapcar (lambda (sym) (if (cut-place-holder-p sym) (pop tmp-place-holders) (pop tmp-fixed-params-place-holders))) args)))
    `(let ,(mapcar (lambda (sym value) `(,sym ,value)) fixed-params-place-holders fixed-params)
       (lambda ,place-holders
	 (funcall ',proc-expr ,@lambda-body)))))

(defun iota (count &optional (start 0) (step 1))
  (labels ((rec (c val acc)
	     (if (<= c 0)
		 acc
	       (rec (1- c) (+ val step) (cons val acc)))))
    (nreverse (rec count start '()))))

(defun list-tabulate (n init-proc)
  (mapcar (lambda (x) (funcall init-proc x)) (iota n)))

(defun take (lst n)
  (labels ((rec (n lst acc)
	     (if (or (= n 0)
		     (atom lst))
		 acc
	       (rec (1- n) (cdr lst) (cons (car lst) acc)))))
    (nreverse (rec n lst '()))))

(defun circular-list (&rest args)
  (let ((tmp (copy-list args)))
    (nconc tmp tmp)))

(defun x->string (obj)
  (format nil "~A" obj))

(defun fold (func init &rest lists)
  (let* ((acc init)
	 (f (lambda (&rest args)
	      (setf acc (apply func `(,@args ,acc))))))
    (apply #'mapc f lists)
    acc))
