(provide "anaphoric-macros")

(export
 '(
   it
   aif
   acond
   alambda
   ))

(defmacro aif (cond true-body false-body)
  "条件値をアナフォラitに束縛して各bodyを評価するif。"
  `(let ((it ,cond))
     (if it
	 ,true-body
       ,false-body)))
(setf (get 'aif 'ed::lisp-indent-hook) (get 'if 'ed::lisp-indent-hook))

(defun acond-expand (forms)
  "各条件節内でその条件値をアナフォラitに束縛してフォームを評価するcond。"
  (if forms
      `(let ((it ,(caar forms)))
	 (if it
	     (progn
	       ,@(cdar forms)
	       )
	   ,(acond-expand (cdr forms))))))
	   
(defmacro acond (&rest forms)
  (acond-expand forms))
(setf (get 'acond 'ed::lisp-indent-hook) (get 'cond 'ed::lisp-indent-hook))

(defmacro alambda (&body body)
  "アナフフォラselfで再帰可能なlambda式を定義する。"
  `(labels ((self ,@body))
     #'self))
(setf (get 'alambda 'ed::lisp-indent-hook) 1)
