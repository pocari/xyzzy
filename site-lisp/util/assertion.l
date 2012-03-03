(provide "assertion")

(export '(*assertion-enable*))

(export '(assert-true assert-not assert-same assert-not-same))

(defvar *assertion-enable* t "if non-nil then assetion enable (default t))")

(defmacro assert-form (&body body)
  `(if *assertion-enable*
       (progn
	 ,@body)))

(defmacro assert-true (e)
  (assert-form
   `(unless ,e
      (error "Assert Failed: ~S expected true but was nil" ',e))))

(defmacro assert-not (e)
  (assert-form
   `(if ,e
	(error "Assert Failed: ~S expected nil but was ~S" ',e ,e))))

(defmacro assert-same (value expected &optional (pred #'eql))
  (assert-form
   (let ((v1 (gensym))
	 (v2 (gensym)))
     `(let ((,v1 ,value)
	    (,v2 ,expected))
	(unless (funcall ,pred ,v1 ,v2)
	  (error "Assert Failed: ~S expected same:<~S <~S>> was not <~S>" ',value ',expected ,v2 ,v1))))))

(defmacro assert-not-same (value expected &optional (pred #'eql))
  (assert-form
   (let ((v1 (gensym))
	 (v2 (gensym)))
     `(let ((,v1 ,value)
	    (,v2 ,expected))
	(if (funcall ,pred ,v1 ,v2)
	    (error "Assert Failed: ~S expected not same:<~S <~S>> but was same" ',value ',expected ,v2 ,v1))))))
