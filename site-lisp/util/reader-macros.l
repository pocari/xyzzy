(provide "reader-macros")

#|
#^form
�`����lambda�����쐬���郊�[�h�}�N��

(mapcar (lambda (x &rest rest)
	  (apply #'+ x rest))
	'(1 2 3) '(10 20 30) '(100 200 300))
=> (111 222 333)

�͉��L�̂悤�ɋL�q�\
(mapcar #^(apply #'+ $1 $rest)
	'(1 2 3) '(10 20 30) '(100 200 300))
=> (111 222 333)

����́A
(mapcar (lambda ($1 &rest $rest)
	  (apply #'+ $1 $rest))
	'(1 2 3) '(10 20 30) '(100 200 300))
�ƓW�J�����B


$1 $2 ... ��$�Ŏn�܂镶���ȍ~���\�[�g����鏇�Ԃ�lambda���̈�������`�����B
$1 $2 $a
��3���t�H�[���Ɍ�������
(lambda ($1 $2 $a)
  ...)
�ƒ�`����B
�A���A$rest���������ꍇ�̂݁Alambda�����̍Ō��
&rest $rest
��������B
�Ȃ̂ŁA���̃t�H�[���̒��ŁA$xx�Ƃ����V���{����(lambda�����Ƃ��ĈȊO)�g�p�ł��Ȃ�

|#
(defun lambda-reader-arg-p (sym)
  (and (symbolp sym)
       (string= "$" (substring (symbol-name sym) 0 1))))

(defun lambda-reader (stream char param)
  (let* ((lambda-body (read stream))
	 (args (sort (remove-duplicates
		      (remove-if-not #'lambda-reader-arg-p (flatten lambda-body)))
		     (lambda (x y)
		       (string< (symbol-name x) (symbol-name y))))))
    (if (find '$rest args)
	(setf args (reverse (list* '$rest '&rest (reverse (remove '$rest args))))))
    `(lambda ,args ,lambda-body)))

(set-dispatch-macro-character #\# #\^ #'lambda-reader)

#|

#~/abcd/
�ŁA�R���p�C���ςݐ��K�\���I�u�W�F�N�g��Ԃ�

#~m/abcd/
��
(lambda (str)
  (string-match (compile-regexp "abcd") str))
��Ԃ��B
ex.
(#~m/\([0-9]+\)/ "abcdef987ghijk")
=> 6
(match-string 1)
=> "987"


#~s/abcd/
��
(lambda (str replacement)
  (when (string-match (compile-regexp "abcd") str)
    (string-replace-match str replacement))))
��Ԃ��B
ex.
(#~s/^\(abc\)\(def\)$/ "abcdef" "\\2 \\1")
=>"def abc"

|#
(defun regexp-reader-command-char-p (ch)
  (or (string= ch "m")
      (string= ch "s")))
      
(defun regexp-reader (stream char param)
 (let* (buf
	regexp
        after-end-delimiter
        (case-fold-p nil)
	(first-char (read-char stream))
	(delimiter (if (regexp-reader-command-char-p first-char) (read-char stream) first-char))
	(delimiter-end (cond ((char= delimiter #\<) #\>)
                             (t delimiter))))
   (do ((c (read-char stream) (read-char stream))
        (prev nil c))
       ((char= c delimiter-end))
     ;(format t "c:~A~%" c)
     (when (char= c #\\)
       (when (char= (peek-char nil stream nil) delimiter)
         (setf c delimiter)
         (read-char stream)))
     (setf buf (cons c buf)))
   (if (setf after-end-delimiter (read-char stream nil))
       (cond ((char= after-end-delimiter #\i)
              (setf case-fold-p t))
             (t
              (unread-char after-end-delimiter stream))))
   (setq regexp (compile-regexp (concatenate 'string (nreverse buf)) case-fold-p))
   (cond ((string= first-char "m")
	  (let ((arg1 (gensym)))
	    `(lambda (,arg1) (string-match ,regexp ,arg1))))
	 ((string= first-char "s")
	  (let ((arg1 (gensym))
		(arg2 (gensym)))
	    `(lambda (,arg1 ,arg2)
	       (when (string-match ,regexp ,arg1)
		 (string-replace-match ,arg1 ,arg2)))))
	 (t
	  regexp))
   ))

(set-dispatch-macro-character #\# #\~ #'regexp-reader)


#|

'(#$(1+ (lambda (x) (* 3 x))) 3)
=>((lambda (&rest #1=#:G4205) (funcall #'1+ (apply (lambda (x) (* 3 x)) #1#))) 3)
=> 10

(�ŏ��ɓK�p�����)�Ō�̊֐��̂ݔC�ӌ̈��������邪���1�����̊֐��̂݉\

|#
(defun compose-function-reader (stream char param)
  (apply #'compose (read stream)))

(set-dispatch-macro-character #\# #\$ #'compose-function-reader)

#|
debug print��\������B
(defun fact (n)
  (if (= n 1)
      1
    (* n #?(fact (- n 1)))))

(fact 5)
?== (fact (- n 1))
?== (fact (- n 1))
?== (fact (- n 1))
?== (fact (- n 1))
?-> 1
?-> 2
?-> 6
?-> 24
120

|#
(defun debug-print-reader (stream char param)
  (let ((form (read stream))
        (value (gensym)))
    `(progn
       (format t "?== ~A~%" ',form)
       (let ((,value ,form))
         (format t "?-> ~A~%" ,value)
         ,value))))

(set-dispatch-macro-character #\# #\? #'debug-print-reader)