;;xsv.l
;;
;;CSV�t�@�C������͂���
;;
;;�C���X�g�[��
;; ���̃t�@�C����site-lisp�ɕۑ�
;; (byte-compile-file "���̃t�@�C���ւ̃p�X")
;; �o�C�g�R���p�C�����Ȃ��ƁA�������x���ł��E�E�E�B
;;
;;�g����
;;  (require "xsv")
;;  xsv-create-parser�Ńp�[�T���쐬���A
;;  xsv-bind-stream��csv�t�@�C���ւ̃X�g���[�����Z�b�g��
;;  xsv-parse�Ńp�[�X����B
;;
;;  �p�[�T�쐬���Ɋe�C�x���g�ɑ΂���n���h�����Z�b�g���邱�Ƃ�
;;  �w�b�_�A���R�[�h�A�t�B�[���h�ɃA�N�Z�X����
;;
;;  xsv-create-parser�ł̐ݒ荀�ڂ͉��L
;;    delimiter
;;      �f���~�^���Z�b�g����B
;;      ������łȂ��A�������w�肵�܂��B�f�t�H���g�̓J���}(#\,)
;;  	handle-record
;;      1�s�p�[�X����x�ɍs��������o�^���܂��B
;;      �u1�s�̓��e��\�����X�g�v�������Ƃ��Ď󂯎��֐����Z�b�g�B
;;  	handle-header
;;      nil�łȂ��ꍇ�A1�s�ڂ��w�b�_�Ƃ݂Ȃ��܂��B
;;      �u�w�b�_�̓��e��\�����X�g�v�������Ƃ��Ď󂯎��֐����Z�b�g�B
;;  	handle-field
;;      nil�Ŗ����ꍇ�A�t�B�[���h���������x�ɌĂ΂�܂��B
;;      �u�t�B�[���h�v�������Ƃ��Ď󂯎��A�V�����t�B�[���h�l��Ԃ��֐����Z�b�g�B
;;
;;�T���v��
;; (require "xsv")
;; ;;�p�[�T�쐬
;; (setq parser
;; 	  (xsv-create-parser
;; 	   :handle-header #'(lambda (header)
;; 						  (format t "header:(~D):~A~%" (length header) header))
;; 	   :handle-record #'(lambda (record)
;; 						  (format t "record(~D):~A~%" (length record) record))
;; 	   :handle-field #'(lambda (field)
;; 						 (format nil "[~A]" field))))
;; ;;�p�[�X���s
;; (xsv-bind-stream (parser "C:/data.csv")
;;   (xsv-parse parser))
;; =>
;; header:(4):([field1] [field2] [field3] [field4])
;; record(4):([a-1] [b-1] [c-1] [d-1])
;; record(4):([a,-2] [b,","-2] [,"c-2",] [d-2])
;; record(4):([,"
;; a
;; -
;; 3,"] [,b-3
;; ","] [] [c-3])
;; nil


(provide "xsv")

(export '(xsv-create-parser xsv-parse xsv-bind-stream))

(setf (get 'xsv-bind-stream 'ed:lisp-indent-hook) 1)

(defstruct xsv-lexer
  (header nil)
  (delimiter #\,)
  (stream nil)
  (buffer '()))

(defstruct xsv-parser
  (lexer nil)
  (current-token nil)
  (current-value nil)
  (record nil)
  (handle-record nil)
  (handle-header nil)
  (handle-field  nil))

(defun xsv-pushback (lexer ch)
  (push ch (xsv-lexer-buffer lexer)))

(defun xsv-pushback-token (parser prev-type prev-value)
  (let ((type  (xsv-parser-current-token parser))
	(value (xsv-parser-current-value parser))
	(lexer (xsv-parser-lexer parser)))
    (cond
     ((eq type 'textdata)
      (let ((i (1- (length value))))
	(while (>= i 0)
	  (xsv-pushback lexer (elt value i))
	  (decf i))))
     ((eq type 'crlf)
      (xsv-pushback lexer #\C-j)
      (xsv-pushback lexer #\C-m))
     ((eq type 'eof)
      ;no op
      )
     (t
      (xsv-pushback lexer (character value))))
    (setf (xsv-parser-current-token parser) prev-type)
    (setf (xsv-parser-current-value parser) prev-value))
  )


(defun xsv-next-char (lexer)
  (if (null (xsv-lexer-buffer lexer))
      (read-char (xsv-lexer-stream lexer) nil nil)
    (pop (xsv-lexer-buffer lexer))))

(defun xsv-next-token (parser)
  (let ((result nil))
    (setf result (xsv-next-token-1 (xsv-parser-lexer parser)))
;    (format t "~A~%" result)
    (setf (xsv-parser-current-token parser) (car result))
    (setf (xsv-parser-current-value parser) (cadr result))
    result))
    

(defun xsv-next-token-1 (lexer)
  (let (c token-value ret)
    (loop
      (setf c (xsv-next-char lexer))
      (cond
       ((eq c (xsv-lexer-delimiter lexer))
	(setq ret (list 'delimiter (format nil "~C" c)))
	(return))
       ((eq c #\")
	(setq ret (list 'dquote "\""))
	(return))
       ((eq c #\RET)
	(setq ret (list 'cr (format nil "~C" c)))
	(setq c (xsv-next-char lexer))
	(if (eq c #\LFD)
	    (setq ret (list 'crlf (format nil "~C~C" #\RET #\LFD)))
	  (xsv-pushback lexer c))
	(return))
       ((eq c #\LFD)
	(setq ret (list 'lf (format nil "~C" c)))
	(return))
       ((eq c nil)
	(setq ret (list 'eof nil))
	(return))
       (t
	(push c token-value))))
    (if token-value
	(progn
	  (if (eq 'crlf (car ret))
	      (progn
		(xsv-pushback lexer #\LFD)
		(xsv-pushback lexer #\RET))
	    (xsv-pushback lexer c))
	  (list 'textdata (concatenate 'string (nreverse token-value))))
      ret)))

(defun xsv-parse (parser)
  (xsv-next-token parser)
  (xsv-parse-file parser))

(defun xsv-parse-file (parser)
  (if (xsv-parser-handle-header parser)
      (xsv-parse-header parser)
    (xsv-parse-record parser))
  (while (eq (xsv-parser-current-token parser) 'crlf)
    (xsv-next-token parser)
    (if (eq (xsv-parser-current-token parser) 'eof)
	(return))
    (xsv-parse-record parser)))

(defun xsv-parse-header (parser)
  (xsv-parse-file-helper parser (xsv-parser-handle-header parser)))

(defun xsv-parse-record (parser)
  (xsv-parse-file-helper parser (xsv-parser-handle-record parser)))

(defun xsv-parse-file-helper (parser handler)
  (xsv-parse-record0 parser)
  (when (xsv-parser-record parser)
    (funcall handler (nreverse (xsv-parser-record parser)))
    (setf (xsv-parser-record parser) nil)))

(defun xsv-parse-record0 (parser)
  (xsv-parse-field parser)
  (while (eq (xsv-parser-current-token parser) 'delimiter)
    (xsv-next-token parser)
    (xsv-parse-field parser)))

(defun xsv-parse-field (parser)
  (let (field)
    (if (eq 'dquote (xsv-parser-current-token parser))
	(progn
	  (xsv-next-token parser)
	  (setf field (xsv-parse-escaped parser)))
      (setf field (xsv-parse-non-escaped parser)))
    (when field
      (if (xsv-parser-handle-field parser)
	  (setf field (funcall (xsv-parser-handle-field parser) field)))
      (push field (xsv-parser-record parser)))
    ))

(defun xsv-parse-non-escaped (parser)
  (let (field)
    (while (eq 'textdata (xsv-parser-current-token parser))
      (setf field (concat field (xsv-parser-current-value parser)))
      (xsv-next-token parser))
    (if (eq 'delimiter (xsv-parser-current-token parser))
	(if (null field)
	    (setf field "")))
    field))

(defun xsv-parse-escaped (parser)
  (let (type value tmp field 2dquote)
    (loop
      (setf type (xsv-parser-current-token parser))
      (setf value (xsv-parser-current-value parser))
      (cond
       ((or (eq type 'textdata)
	    (eq type 'delimiter)
	    (eq type 'cr)
	    (eq type 'lf)
	    (eq type 'crlf))
	(setf field (concat field value)))
       ((eq type 'dquote)
	(let ((prev-type (xsv-parser-current-token parser))
	      (prev-value (xsv-parser-current-value parser)))
	  (xsv-next-token parser)
	  (if (eq 'dquote (xsv-parser-current-token parser))
	      (progn
		(setf field (concat field "\""))
		(setf 2dquote t))
	    (progn
	      (xsv-pushback-token parser prev-type prev-value)
	      (setf 2dquote nil)))))
       ((eq type 'eof)
	(return))
       (t
	(error "unknown token")))
      (when (and (not 2dquote) (eq type 'dquote))
	(return))
      (xsv-next-token parser))
    (when (not (eq 'dquote (xsv-parser-current-token parser)))
      (error (format nil "�����񂪕����Ă��܂���B[~A]" field)))
    (xsv-next-token parser)
    field
    ))

(defun debug (msg)
  (format t "~A~%" msg))

(defun test-parser (fp)
  (make-xsv-parser
   :lexer (make-xsv-lexer :stream fp)
   :handle-record #'(lambda (record)
		      (format t "record:(~D)[~A]~%" (length record) record))))

(defmacro xsv-bind-stream ((parser file) &body body)
  (let ((fp (gensym)))
    `(with-open-file (,fp ,file :encoding :raw :direction :input)
       (setf (xsv-lexer-stream (xsv-parser-lexer ,parser)) ,fp)
       ,@body)))

(defun xsv-create-parser (&key (delimiter #\,)
			       handle-record
			       handle-header
			       handle-field)
  (make-xsv-parser
   :lexer (make-xsv-lexer :delimiter delimiter)
   :handle-record handle-record
   :handle-header handle-header
   :handle-field handle-field))
