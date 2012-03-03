(defmacro time (&body body)
  "bodyの評価にかかった時間を表示"
  (let ((elaps (gensym))
	(result (gensym)))
    `(prog2
	 (setf ,elaps (get-internal-real-time))
	 (setf ,result ,@body)
       (setf ,elaps (- (get-internal-real-time) ,elaps))
       (format t "Evaluated: ~S~%" ',@body)
       (format t "result   : ~S~%" ,result)
       (format t "Elaps    : ~A~%" ,elaps))))

(defmacro dump-symbols (&rest rest)
  "シンボル値シンボル名とともにをダンプ"
  (let ((fmt (reduce (lambda (acc e)
		       (concat acc "~S:[~S] "))
		     rest
		     :initial-value ""))
	(args (mapcan (lambda (x) `(',x ,x)) rest)))
    `(funcall #'format t ,(concat fmt "~%") ,@args)))
 