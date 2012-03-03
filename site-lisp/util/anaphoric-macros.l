(provide "anaphoric-macros")

(export
 '(
   it
   aif
   acond
   alambda
   ))

(defmacro aif (cond true-body false-body)
  "�����l���A�i�t�H��it�ɑ������Ċebody��]������if�B"
  `(let ((it ,cond))
     (if it
	 ,true-body
       ,false-body)))
(setf (get 'aif 'ed::lisp-indent-hook) (get 'if 'ed::lisp-indent-hook))

(defun acond-expand (forms)
  "�e�����ߓ��ł��̏����l���A�i�t�H��it�ɑ������ăt�H�[����]������cond�B"
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
  "�A�i�t�t�H��self�ōċA�\��lambda�����`����B"
  `(labels ((self ,@body))
     #'self))
(setf (get 'alambda 'ed::lisp-indent-hook) 1)