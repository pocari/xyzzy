(provide "reader-macros")

#|
#^form
形式でlambda式を作成するリードマクロ

(mapcar (lambda (x &rest rest)
	  (apply #'+ x rest))
	'(1 2 3) '(10 20 30) '(100 200 300))
=> (111 222 333)

は下記のように記述可能
(mapcar #^(apply #'+ $1 $rest)
	'(1 2 3) '(10 20 30) '(100 200 300))
=> (111 222 333)

これは、
(mapcar (lambda ($1 &rest $rest)
	  (apply #'+ $1 $rest))
	'(1 2 3) '(10 20 30) '(100 200 300))
と展開される。


$1 $2 ... と$で始まる文字以降がソートされる順番でlambda式の引数が定義される。
$1 $2 $a
の3つをフォームに見つけたら
(lambda ($1 $2 $a)
  ...)
と定義する。
但し、$restを見つけた場合のみ、lambda引数の最後に
&rest $rest
を加える。
なので、このフォームの中で、$xxというシンボルは(lambda引数として以外)使用できない

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
で、コンパイル済み正規表現オブジェクトを返す

#~m/abcd/
で
(lambda (str)
  (string-match (compile-regexp "abcd") str))
を返す。
ex.
(#~m/\([0-9]+\)/ "abcdef987ghijk")
=> 6
(match-string 1)
=> "987"


#~s/abcd/
で
(lambda (str replacement)
  (when (string-match (compile-regexp "abcd") str)
    (string-replace-match str replacement))))
を返す。
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

(最初に適用される)最後の関数のみ任意個の引数を取れるが後は1引数の関数のみ可能

|#
(defun compose-function-reader (stream char param)
  (apply #'compose (read stream)))

(set-dispatch-macro-character #\# #\$ #'compose-function-reader)

#|
debug printを表示する。
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
