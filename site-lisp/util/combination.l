(provide "combination")

(require "gauche-compat")

(export
 '(combination
   rcombination
   permutation
   rpermutation
   direct-product
   ))

(defmacro create-combination-function (name succ-func)
  `(defun ,name (n k)
     (labels ((rec (start depth acc)
		(if (= depth k)
		    (list (reverse acc))
		  (mapcan (lambda (x)
			    (rec (,succ-func x) (1+ depth) (cons x acc)))
			  (iota (- n start) start)))))
       (rec 0 0 nil))))

(create-combination-function combination 1+) ;重複無し組み合わせ

(create-combination-function rcombination identity) ;重複有り組み合わせ

; (comb 4 2)
; => ((0 1) (0 2) (0 3) (1 2) (1 3) (2 3))
; 
; (rcomb 4 2)
; =>((0 0) (0 1) (0 2) (0 3) (1 1) (1 2) (1 3) (2 2) (2 3) (3 3))

(defun permutation (n k)
  (let ((lst (iota n 0)))
    (labels ((rec (depth acc)
	       (if (= depth k)
		   (list (reverse acc))
		 (mapcan (lambda (x)
			   (unless (member x acc)
			     (rec (1+ depth) (cons x acc))))
			 lst))))
      (rec 0 nil))))

(defun rpermutation (n k)
  (let ((lst (iota n 0)))
    (labels ((rec (depth acc)
	       (if (= depth k)
		   (list (reverse acc))
		 (mapcan (lambda (x)
			   (rec (1+ depth) (cons x acc)))
			 lst))))
      (rec 0 nil))))

; (permutation 4 2)
; => ((0 1) (0 2) (0 3) (1 0) (1 2) (1 3) (2 0) (2 1) (2 3) (3 0) (3 1) (3 2))
; 
; (rpermutation 4 2)
; => ((0 0) (0 1) (0 2) (0 3) (1 0) (1 1) (1 2) (1 3) (2 0) (2 1) (2 2) (2 3) (3 0) (3 1) (3 2) (3 3))

(defun direct-product (&rest lists)
  "listsの直積を返す."
  " (direct-product '(1 2) '(a b c))"
  " =>((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))"
  (if (atom (cdr lists))
      (mapcar #'list (car lists))
    (mapcan (lambda (x)
	      (mapcar (lambda (lst)
			(cons x (copy-list lst)))
		      (apply #'direct-product (cdr lists))))
	    (car lists))))
