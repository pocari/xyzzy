(defmacro time (&body body)
  "body�̕]���ɂ����������Ԃ�\��"
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
  "�V���{���l�V���{�����ƂƂ��ɂ��_���v"
  (let ((fmt (reduce (lambda (acc e)
		       (concat acc "~S:[~S] "))
		     rest
		     :initial-value ""))
	(args (mapcan (lambda (x) `(',x ,x)) rest)))
    `(funcall #'format t ,(concat fmt "~%") ,@args)))
 