;;; -*- Mode: Lisp; -*-
;;;
;;; diff-region.l
;;; 
;;;   2�̃��[�W�����̃e�L�X�g��diff�ɂ�����B 
;;; 
;;; �� �C���X�g�[��
;;;   1. diff-region.l��site-lisp�ɓ����B
;;;   2. diff-region�����[�h����
;;;      .xyzzy��
;;;        (load-library "diff-region")
;;;      �������� .xyzzy �����[�h���邩xyzzy���ċN��
;;;
;;; �� �g�p���@
;;;   1. M-x diff-region �Ō��݂̃��[�W��������r���ƂȂ�A
;;;      diff-region���[�h�ɓ���܂��B
;;;   2. �܂��A��r�ΏۂƂȂ�̈�̍ŏ��Ƀ|�C���g�������Ă���Enter�A 
;;;      ���ɗ̈�̍Ō�Ƀ|�C���g�������Ă���Enter
;;;   3. �����diff�����s���܂��B
;;;   4. ��r�Ώۗ̈�̃|�C���g�ʒu��diff�I����N���A�����̂ŁA
;;;      �܂���r�������ꏊ������Ύ菇2.����J��Ԃ��܂��B
;;;      ��r���̃��[�W������ύX�������ꍇ�͎菇5.�ɍs����
;;;      ��������I�����Ă���菇1����J��Ԃ��Ă��������B
;;;      �J�n�ʒu�ƏI���ʒu���قȂ�o�b�t�@�ɂ����
;;;      ��Ɏw�肵���I���ʒu���V���ȊJ�n�ʒu�ƂȂ�܂��B
;;;   5. diff���I���������Ƃ̃o�b�t�@��
;;;        q
;;;        M-x diff-region
;;;      �̂����ꂩ����͂����diff-region���[�h�𔲂��ďI�����܂��B
;;;
;;; �� diff-region-mode�̃L�[�o�C���h
;;;    �f�t�H���g�ŃL�[�o�C���h�͈ȉ��̂悤�ɂȂ��Ă��܂��B
;;;     n       ���̍s�ֈړ� (next-line)
;;;     p       ��̍s�ֈړ� (previous-line)
;;;     f       �ꕶ���O�i   (forward-char)
;;;     b       �ꕶ�����   (backward-char)
;;;     u       �X�^�[�g�ʒu�L�����Z�� (diff-region-cancel-point)
;;;     RET     �|�C���g�ʒu���r�Ώۂ̊J�n�A�܂���
;;;             �I���ʒu�Ƃ���B (diff-region-set-point)
;;;     q       diff-region-mode���I������ (diff-region-quit)
;;;
;;;
;;; �� ���
;;;   �Ediff�I����Ɍ��̃o�b�t�@�̏�Ԃɖ߂������B
;;;
;;; �� Changelog
;;;   * 2003/10/21
;;;     �E��r���̈ꎞ�t�@�C�������Ƃ̃o�b�t�@�̃��W���[���[�h��
;;;       �J���悤�ɂ����B
;;;     �E��r�Ώۂ̃��[�W�����̊J�n�ʒu��I�тȂ�����悤�ɂ����B
;;;       ��r���Ƃ͂܂����ĂȂ��B
;;;     �EC-g���Ԃ��̂�΂������B��߂��B
;;;     �E*diff-region-mode*���o�b�t�@���[�J���łȂ������̂��C���B
;;;   * 2003/10/25
;;;     �E �E�B���h�E�̏�Ԃ�߂���悤�ɂ����B
;;;        NANRI����� diff-region-of-windows.l����قƂ�ǃp�N���Ă܂��B
;;;     �ENANRI����̂��Ƃ�ǂ��āAdiff-quti-w�̋�����ύX�B
;;;-----------------------------------------------------------------
;;; $Revision: 1.5 $
;;; $Date: 2003/10/24 16:35:13 $
;;;-----------------------------------------------------------------

(provide "diff-region")

(in-package "editor")
(require "diff")

(export
 '(*diff-region-mode*
   *diff-region-mode-map*
   *diff-region-ignore-buffer-regexp*
   *diff-region-current-window-configuration*))

(export
 '(diff-region
   diff-region-set-piont
   diff-region-quit))
   
(defstruct region-info
  start
  end
  buffer)

(defvar-local *diff-region-mode* nil)
(defvar-local *diff-region-buffer-read-only-after-quit* nil)

(defvar *diff-region-ignore-buffer-regexp* '("Minibuf0"))
(defvar *diff-region-current-window-configuration* nil)
(defvar *diff-region-tmp-files* nil)

(defvar *old-region-info* nil)
(defvar *new-region-info* nil)

(defvar *diff-region-mode-map* nil)
(unless *diff-region-mode-map*
  (let ((map (make-sparse-keymap)))
    (define-key map #\RET 'diff-region-set-point)
    (define-key map #\n   'next-line)
    (define-key map #\p   'previous-line)
    (define-key map #\f   'forward-char)
    (define-key map #\b   'backward-char)
    (define-key map #\u   'diff-region-cancel-point)
    (define-key map #\q   'diff-region-quit)
    (setq *diff-region-mode-map* map)))

;;
(defun diff-region-set-point ()
  (interactive)
  (if (or (not *new-region-info*)
	  (not (eq (selected-buffer) (region-info-buffer *new-region-info*))))
      (progn
	(setq *new-region-info* (make-region-info
				 :buffer (selected-buffer)
				 :start (point)))
	(message "start point is ~A in ~A"
		 (region-info-start *new-region-info*)
		 (region-info-buffer *new-region-info*)))
    (progn
      (setf (region-info-end *new-region-info*) (point))
      (diff-region-exec-diff))))

(defun diff-region-cancel-point ()
  (interactive)
  (setq *new-region-info* nil)
  (message "cancel"))

(defun diff-region-quit ()
  (interactive)
  (substitute-key-definition 'diff-quit-w 'diff-quit *diff-mode-map*)
  (diff-region nil nil nil))
  

(defun diff-quit-w ()
  (interactive)
  (when (prog1
            diff-buffer
          (and (find-buffer *diff-old-buffer-name*)
               (or (kill-buffer *diff-old-buffer-name*)
                   (quit)))
          (and (find-buffer *diff-new-buffer-name*)
               (or (kill-buffer *diff-new-buffer-name*)
                   (quit)))))
  (if *diff-region-current-window-configuration*
      (progn
	(set-window-configuration *diff-region-current-window-configuration*)
	(setq *diff-region-current-window-configuration* nil))
    (delete-window))
  t)

(defun diff-region-make-region-file (reginfo)
  (let* ((tmp-file (make-temp-file-name))
	 (buffer   (region-info-buffer reginfo))
	 (start    (region-info-start  reginfo))
	 (end      (region-info-end    reginfo)))
    (save-excursion
      (set-buffer buffer)
      (write-region start end tmp-file)
      (push `(,(regexp-quote tmp-file) . ,buffer-mode)  *auto-mode-alist*)
      (setq *diff-region-tmp-files* (cons tmp-file *diff-region-tmp-files*)))
    tmp-file))

(defun diff-region-exec-diff ()
  (let ((new-file (diff-region-make-region-file *new-region-info*))
	(old-file (diff-region-make-region-file *old-region-info*)))
    (setq *new-region-info* nil)
    (setq *diff-region-current-window-configuration* (current-window-configuration))
    (diff new-file old-file)))

(defun diff-region-initialize ()
  (setq *old-region-info* nil
	*new-region-info* nil)
  (substitute-key-definition 'diff-quit 'diff-quit-w *diff-mode-map*))
  
;;---------------- for each buffer 
(defun diff-region-initialize-buffer-local ()
  (set-minor-mode-map *diff-region-mode-map*)
  (setq *diff-region-buffer-read-only-after-quit* buffer-read-only)
  (unless buffer-read-only
    (setq buffer-read-only t)))

(defun diff-region-finalize-buffer-local ()
  (unset-minor-mode-map *diff-region-mode-map*)
  (setq buffer-read-only *diff-region-buffer-read-only-after-quit*))

;;-----------------  utils
(defun diff-region-delete-tmp-files ()
  (when *diff-region-tmp-files*
    (long-operation
      (message "Deleting tmp files maked by diff-region ...")
      (mapcar #'(lambda (x)
		  (and (file-exist-p x) (delete-file x :recycle nil)))
	      *diff-region-tmp-files*))))

(defun diff-region-ignore-buffer-p (buf)
  (let ((buf-name (buffer-name buf)))
    (find-if #'(lambda (reg) (string-match reg buf-name))
	     *diff-region-ignore-buffer-regexp*)))

(defun diff-region-get-target-buffer ()
  (remove-if #'(lambda (buf)
		 (diff-region-ignore-buffer-p buf))
	     (buffer-list)))

;;------------------
(defun diff-region (beg end &optional (arg nil sv))
  (interactive "r\np")
  (let ((current (selected-buffer)))
    (dolist (buffer (diff-region-get-target-buffer))
      (set-buffer buffer)
      (toggle-mode '*diff-region-mode* arg sv)
      (update-mode-line t)
      (when (and *diff-region-mode*
		 (eq buffer current))
	(diff-region-initialize)
	(setq *old-region-info* (make-region-info
				 :buffer (selected-buffer)
				 :start beg
				 :end end)))
      (if *diff-region-mode*
	  (diff-region-initialize-buffer-local)
	(diff-region-finalize-buffer-local)))
    (switch-to-buffer current))
  t)

(add-hook '*kill-xyzzy-hook* 'diff-region-delete-tmp-files)
(add-hook '*delete-buffer-hook*
	  #'(lambda (buf)
	      (when *diff-region-mode*
		(setq *diff-region-current-window-configuration* nil))
	      t))

(pushnew '(*diff-region-mode* . "Diff region") *minor-mode-alist* :key #'car)

;;;  End of diff-region.l