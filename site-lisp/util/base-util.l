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
  "�܂��A���O�ǂ���."
  (apply #'remove-if-not fn args))

(defun split-at (lst n)
  "lst���C���f�b�N�Xn����O��n�ȍ~�ŕ��������X�g�𑽒l�ŕԂ�"
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
  "lst���C���f�b�N�Xn����O��n�ȍ~�ŕ��������X�g�𑽒l�ŕԂ��B(length lst)��n�ɖ����Ȃ��ꍇ�́Afill-elem�Ŗ��߂����X�g�𕪊�����B"
  (split-at (if (< (length lst) n)
		(append lst (make-list (- n (length lst)) :initial-element fill-char))
	      lst) n))

(defun zip (&rest lists)
  "�v�f�����^����ꂽ���X�g�̒��ň�ԒZ�����X�g�ɂ��킹����ȊO��ruby��zip�Ɠ���"
  (apply #'mapcar (lambda (&rest params) (copy-tree params)) lists))

(defun invert (pred)
  "�^����ꂽ�q��̐^�U�𔽓]���ĕԂ��֐���Ԃ�"
  (lambda (&rest params)
    (not (apply pred params))))

(defun mapa...b (fn start end)
  "fn��start <= i < end�͈̔͂�i��n���Ď��s�������ʂ�v�f�Ƃ��郊�X�g��Ԃ�"
  (do ((i start (1+ i))
       (ret '()))
      ((<= end i) (nreverse ret))
    (push (funcall fn i) ret)))

(defun mapa..b (fn start end)
  "fn��start <= i <= end�͈̔͂�i��n���Ď��s�������ʂ�v�f�Ƃ��郊�X�g��Ԃ�"
  (mapa...b fn start (1+ end)))

(defun map0...n (fn n)
  "fn��0 <= i < n�͈̔͂�i��n���Ď��s�������ʂ�v�f�Ƃ��郊�X�g��Ԃ�"
  (mapa...b fn 0 n))

(defun map0..n (fn n)
  "fn��0 <= i <=n �͈̔͂�i��n���Ď��s�������ʂ�v�f�Ƃ��郊�X�g��Ԃ�"
  (mapa...b fn 0 (1+ n)))
  
(defun leaf-converter (func tree)
  "tree�̊e�t��func�̖߂�l�Œu���������V����tree��Ԃ�"
  (cond ((atom tree) nil)
	((atom (car tree))
	 (cons (funcall func (car tree)) (leaf-converter func (cdr tree))))
	(t
	 (cons (leaf-converter func (car tree)) (leaf-converter func (cdr tree))))))

(defun list->tsv (list)
  "list�̊e�v�f���^�u�ŘA�������������Ԃ��B"
  (format nil "~{~A~^	~}" list))


(defmacro curry (fn &rest args)
  "fn��args�ŃJ���[������"
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
  "functions�œn���ꂽ�֐�����������B"
  (let ((arg (gensym)))
    `(lambda (&rest ,arg)
       ,(compose-funcalls functions arg))))
