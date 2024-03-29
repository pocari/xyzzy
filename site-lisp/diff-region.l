;;; -*- Mode: Lisp; -*-
;;;
;;; diff-region.l
;;; 
;;;   2つのリージョンのテキストをdiffにかける。 
;;; 
;;; ■ インストール
;;;   1. diff-region.lをsite-lispに入れる。
;;;   2. diff-regionをロードする
;;;      .xyzzyに
;;;        (load-library "diff-region")
;;;      をかいて .xyzzy をロードするかxyzzyを再起動
;;;
;;; ■ 使用方法
;;;   1. M-x diff-region で現在のリージョンが比較元となり、
;;;      diff-regionモードに入ります。
;;;   2. まず、比較対象となる領域の最初にポイントを持っていきEnter、 
;;;      次に領域の最後にポイントを持っていきEnter
;;;   3. するとdiffを実行します。
;;;   4. 比較対象領域のポイント位置はdiff終了後クリアされるので、
;;;      まだ比較したい場所があれば手順2.から繰り返します。
;;;      比較元のリージョンを変更したい場合は手順5.に行って
;;;      いったん終了してから手順1から繰り返してください。
;;;      開始位置と終了位置を異なるバッファにすると
;;;      後に指定した終了位置が新たな開始位置となります。
;;;   5. diffが終わった後もとのバッファで
;;;        q
;;;        M-x diff-region
;;;      のいずれかを入力するとdiff-regionモードを抜けて終了します。
;;;
;;; ■ diff-region-modeのキーバインド
;;;    デフォルトでキーバインドは以下のようになっています。
;;;     n       下の行へ移動 (next-line)
;;;     p       上の行へ移動 (previous-line)
;;;     f       一文字前進   (forward-char)
;;;     b       一文字後退   (backward-char)
;;;     u       スタート位置キャンセル (diff-region-cancel-point)
;;;     RET     ポイント位置を比較対象の開始、または
;;;             終了位置とする。 (diff-region-set-point)
;;;     q       diff-region-modeを終了する (diff-region-quit)
;;;
;;;
;;; ■ 問題
;;;   ・diff終了後に元のバッファの状態に戻したい。
;;;
;;; ■ Changelog
;;;   * 2003/10/21
;;;     ・比較時の一時ファイルをもとのバッファのメジャーモードで
;;;       開くようにした。
;;;     ・比較対象のリージョンの開始位置を選びなおせるようにした。
;;;       比較もとはまだしてない。
;;;     ・C-gをつぶすのやばいかも。やめた。
;;;     ・*diff-region-mode*がバッファローカルでなかったのを修正。
;;;   * 2003/10/25
;;;     ・ ウィンドウの状態を戻せるようにした。
;;;        NANRIさんの diff-region-of-windows.lからほとんどパクってます。
;;;     ・NANRIさんのあとを追って、diff-quti-wの挙動を変更。
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
