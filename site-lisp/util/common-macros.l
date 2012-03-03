(provide "common-macros")

(export
 '(case-ex
   with-gensyms
   defmacro!
   nlet-tail
   ))

;;”äŠrŠÖ”‚ðŽw’è‚Å‚«‚écase
(defmacro case-ex ((key &key (test #'eql)) &body body)
  (let ((key-sym (gensym)))
    (labels ((inner (k lst)
	       (when lst
		 (if (consp (caar lst))
		     `(if (member ,k ',(caar lst) :test ,test)
			  ,(cadar lst)
			,(inner k (cdr lst)))
		   (if (or (eq (caar lst) t)
			   (eq (caar lst) 'otherwise))
		       (cadar lst)
		     `(if (funcall ,test ,k ,(caar lst))
			  ,(cadar lst)
			,(inner k (cdr lst))))))))
      `(let ((,key-sym ,key))
	 ,(inner key-sym body)))))

(setf (get 'case-ex 'ed::lisp-indent-hook) 1)

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar #'(lambda (x)
		     `(,x (gensym)))
		 symbols)
     ,@body))
(setf (get 'with-gensyms 'ed::lisp-indent-hook) 1)

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "g!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let , (mapcar
               (lambda (s)
                 `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
               syms)
         ,@body))))
(setf (get 'defmacro/g! 'ed::lisp-indent-hook) 1)

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "o!"
                :start1 0
		:end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "g!"
	(subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs)(list ,@os))
          ,(progn ,@body)))))
(setf (get 'defmacro! 'ed::lisp-indent-hook) 1)

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (mapcar (lambda (x) (gensym)) letargs)))
    `(macrolet
	 ((,n ,gs
	      `(progn
		 (psetq
		  ,@(apply #'nconc
			   (mapcar
			    #'list
			    ',(mapcar #'car letargs)
			    (list ,@gs))))
		 (go ,',g!n))))
       (block ,g!b
	 (let ,letargs
	   (tagbody
	    ,g!n (return-from
		     ,g!b (progn ,@body))))))))
(setf (get 'nlet-tail 'ed::lisp-indent-hook) (get 'let 'ed::lisp-indent-hook))
