(provide "base-util")

(export
 '(mkstr
   symb
   flatten
   find-all
   occur
   split-at
   split-at*
   zip
   invert
   mapa...b
   mapa..b
   map0...n
   map0..n
   leaf-converter
   list->tsv
   curry
   compose
   ))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)(princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defun find-all (fn &rest args)
  "まぁ、名前どおり."
  (apply #'remove-if-not fn args))

(defun split-at (lst n)
  "lstをインデックスnより手前とn以降で分けたリストを多値で返す"
  (if (< (length lst) n)
      (error "given list too short for ~A (~S)" n lst)
    (cond ((zerop n)
	   (values (cdr lst) nil))
	  ((= n 1)
	   (values (list (car lst)) (cdr lst)))
	  ((= n 2)
	   (values (list (car lst) (cadr lst)) (cddr lst)))
	  ((= n 3)
	   (values (list (car lst) (cadr lst) (caddr lst)) (cdddr lst)))
	  ((= n 4)
	   (values (list (car lst) (cadr lst) (caddr lst) (cadddr lst)) (cddddr lst)))
	  (t
	   (labels ((helper (xs head i)
		      (if (<= i 0)
			  (values (nreverse head) xs)
			(helper (cdr xs) (cons (car xs) head) (1- i)))))
	     (helper lst nil n))))))

(defun split-at* (lst n &optional (fill-char nil))
  "lstをインデックスnより手前とn以降で分けたリストを多値で返す。(length lst)がnに満たない場合は、fill-elemで埋めたリストを分割する。"
  (split-at (if (< (length lst) n)
		(append lst (make-list (- n (length lst)) :initial-element fill-char))
	      lst) n))

(defun zip (&rest lists)
  "要素数が与えられたリストの中で一番短いリストにあわせられる以外はrubyのzipと同じ"
  (apply #'mapcar (lambda (&rest params) (copy-tree params)) lists))

(defun invert (pred)
  "与えられた述語の真偽を反転して返す関数を返す"
  (lambda (&rest params)
    (not (apply pred params))))

(defun mapa...b (fn start end)
  "fnにstart <= i < endの範囲のiを渡して実行した結果を要素とするリストを返す"
  (do ((i start (1+ i))
       (ret '()))
      ((<= end i) (nreverse ret))
    (push (funcall fn i) ret)))

(defun mapa..b (fn start end)
  "fnにstart <= i <= endの範囲のiを渡して実行した結果を要素とするリストを返す"
  (mapa...b fn start (1+ end)))

(defun map0...n (fn n)
  "fnに0 <= i < nの範囲のiを渡して実行した結果を要素とするリストを返す"
  (mapa...b fn 0 n))

(defun map0..n (fn n)
  "fnに0 <= i <=n の範囲のiを渡して実行した結果を要素とするリストを返す"
  (mapa...b fn 0 (1+ n)))
  
(defun leaf-converter (func tree)
  "treeの各葉をfuncの戻り値で置き換えた新しいtreeを返す"
  (cond ((atom tree) nil)
	((atom (car tree))
	 (cons (funcall func (car tree)) (leaf-converter func (cdr tree))))
	(t
	 (cons (leaf-converter func (car tree)) (leaf-converter func (cdr tree))))))

(defun list->tsv (list)
  "listの各要素をタブで連結した文字列を返す。"
  (format nil "~{~A~^	~}" list))


(defmacro curry (fn &rest args)
  "fnをargsでカリー化する"
  (let ((arg (gensym)))
    `(lambda (&rest ,arg)
       (apply ,fn ,@args ,arg))))

(defun compose-funcalls (functions arg)
  (if functions
      (let ((func (if (atom (car functions))
		      `#',(car functions)
		    (car functions))))
	(if (null (cdr functions))
	    `(apply ,func ,(compose-funcalls (cdr functions) arg))
	  `(funcall ,func ,(compose-funcalls (cdr functions) arg))))
    arg))

(defun compose (&rest functions)
  "functionsで渡された関数を合成する。"
  (let ((arg (gensym)))
    `(lambda (&rest ,arg)
       ,(compose-funcalls functions arg))))

