(provide "process-environ")

(export
 '(*global-environ*
   putenv
   putenvs
   undef-env
   clear-env
   show-environ
   ))

(defvar *global-environ* nil)

(defun putenv (key val)
  (check-type key string)
  (check-type val string)  
  (setq key (string-upcase key))
  (let ((env (assoc key *global-environ* :test #'string=)))
	(if env
		(rplacd env val)
	  (push (cons key val) *global-environ*))))

(defun putenvs (alist)
  (dolist (env alist)
	(putenv (car env) (cdr env) *global-environ*)))

(defun undef-env (key)
  (check-type key string)
  (setq key (string-upcase key))  
  (setf *global-environ*
		(remove-if #'(lambda (x)
					   (string= (car x) (string-upcase key)))
				   *global-environ*)))

(defun clear-env ()
  (setq *global-environ* nil))

(defun show-environ ()
  (interactive)
  (message "~S" *global-environ*))
  
(defun my-execute-subprocess (cmd &optional (arg nil f))
  (interactive "e& \np" :history0 'execute)
  (execute-subprocess cmd arg nil *global-environ*))
  
(define-key ctl-x-map #\& 'my-execute-subprocess)

; ex) 「=>」は*Command Output*への出力内容
; C-x & echo %HOGE%
; => %HOGE%
; (putenv "HOGE" "12345678")
; C-x & echo %HOGE%
; => 12345678


