;
; myutil.l
;           Y.Nishiguti
;
(provide "myutil")
(export
 '(which
   my-filer-get-path
   set-buffer-eol-code-cmd
   my-filer-run-console
   toggle-maximize-window
   window-normalization
   window-maxmization
   window-minimization
   get-logical-drives
   toggle-buffer-bar
   region-to-selection
   insert-comment
   google-search
   run-rtime
   xgrep
   insert-now-date
   kill-line-backward
   indent-this-buffer
   insert-number-to-bol
   countl
   insert-string-to-bol
   delete-string-from-bol
   occur
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "foreign")
  (require "wip/winapi"))
(c:define-dll-entry winapi:BOOL ShowWindow (winapi:HWND c:int) "user32")
(c:define-dll-entry winapi:BOOL IsZoomed (winapi:HWND) "user32")

(defun toggle-maximize-window ()
  (interactive)
  (if (/= 0 (IsZoomed (get-window-handle)))
      (window-normalization)
    (window-maxmization)))

(defun my-window-resize (mode)
  (winapi:ShowWindow (get-window-handle) mode))

(defun window-minimization ()
  (interactive)
  (my-window-resize 6))

(defun window-maxmization ()
  (interactive)
  (my-window-resize 3))

(defun window-normalization ()
  (interactive)
  (my-window-resize 9))

(defun get-logical-drives()
  "���ݗL���ȃh���C�u�����X�g�ɂ��ĕԂ�"
  (unless (fboundp 'GetLogicalDrives)
    (c:define-dll-entry winapi:LONG GetLogicalDrives nil "kernel32"))
  (let ((drives nil)
	(offset 25)
	(drive-bits (GetLogicalDrives)))
    (while (>= offset 0)
      (if (logbitp offset drive-bits)
	  (setf drives (cons (concat (string (code-char (+ 65 offset))) ":/")
			     drives)))
      (setf offset (1- offset)))
    drives))

(defun toggle-buffer-bar()
  "�o�b�t�@�o�[�����������������"
  (interactive)
  (if (nth 3 (find-command-bar 'buffer-bar))
      (hide-command-bar 'buffer-bar)
    (show-command-bar 'buffer-bar)))

(defun region-to-selection(from to)
  "���[�W�����͈͂�I����Ԃɂ���"
  (interactive "r")
  (cond ((< from to)
	 (goto-char from)
	 (while (< (point) to)
	   (selection-forward-char)))
	(t
	 (goto-char from)
	 (while (> (point) to)
	   (selection-backward-char)))))

(defun insert-comment (msg)
  "�u���b�N�R�����g��}��"
  (interactive "*sComment: ")
  (let ((line (concat comment-start
		      (make-array
		       (- 79 (+ (length comment-start)
				(length comment-end))) :element-type 'character :initial-element #\-)
		      comment-end)))
    (insert (concat line "\n"))
    (insert (concat comment-start msg comment-end "\n"))
    (insert (concat line "\n"))))

(defun google-search (from to)
  "���[�W������google�Ō���"
  (interactive "r")
  (if (= from to)
      (progn
	(message "�P���I�����Ă�������")
	(return-from google-search nil)))
  (long-operation
    (let ((str (si:www-url-encode (buffer-substring from to)))
	  url)
      (setq url (concat "http://www.google.co.jp/search?q=" str))
      (shell-execute url t))))

;rtime.exe�Ŏ��Ԑݒ�
(defun run-rtime()
  "rtime.exe�Ŏ��Ԑݒ�"
  (interactive)
  (call-process (merge-pathnames "bin/rtime.exe" (si:system-root)) :wait t)
  (multiple-value-bind (sec min hour date mon year) (get-decoded-time)
    (message (format nil "set time: ~A/~A/~A ~2,'0D:~2,'0D:~2,'0D" year mon date hour min sec))))

;ML����p�N����. �T�u�f�B���N�g�����������邩�ǂ���������
(defun xgrep (pattern directory files subdirp)
  "grep-dialog��cui��  subdirp��'nil'�ȊO�Ȃ�T�u�f�B���N�g����"
  (interactive "sgrep: \nDdirectory: \nsfiles: \nssubdir? " :history0  'search :default2 "*.*" :default3 "nil")
  (require "grepd")
  (let ((ed::*grep-case-fold-search* nil)
	(ed::*grep-regexp-search* t)
	(ed::*grep-subdir* (not (string= subdirp "nil")))
	(ed::*grep-name-only* nil))
    (ed::scan-files pattern (split-string files #\; t " ") directory)))

(defun insert-now-date()
  "�����̓��t��YYYY/MM/DD�`����poin�ʒu�ɑ}��"
  (interactive)
  (multiple-value-bind (sec min hour date mon year) (get-decoded-time)
    (insert (format nil "~D/~2,'0D/~2,'0D" year mon date))))

(defun kill-line-backward()
  "�J�[�\���ʒu����s���܂ł��폜����"
  (interactive)
  (let ((end (point)) start)
    (goto-bol)
    (setq start (point))
    (kill-region start end)))

(defun indent-this-buffer()
  "���J���Ă���o�b�t�@���C���f���g���܂���"
  (interactive)
  (long-operation
    (save-excursion
      (indent-region (point-min) (point-max)))))

(defun number-to-string (number)
  "�����𕶎���ɕϊ� �����Ƃ����̂Ȃ��́H"
  (format nil "~D" number))

(defun max-count-len (n d s e)
  (let ((len 0)
	last-number)
    (setf last-number (+ n (* d (1- (count-line-region s e)))))
    (if (< n last-number)
	(length (number-to-string last-number))
      (length (number-to-string n)))))

(defun insert-number-to-bol(start end)
  "���[�W�����Ԃ̊e�s���ɔԍ���U���Ă���"
  (interactive "*r")
  (let* ((num (read-integer "Init Number: "))
	 (diff (read-integer "Diff: "))
	 (width (read-integer "Width: "))
	 (number-size (max width (max-count-len num diff start end))))
    (each-line-in-region
     #'(lambda (s e)
	 (goto-bol)
	 (insert (format nil (concat "~" (number-to-string number-size)  ",'0D: ") num))
	 (message "~D" num)
	 (setf num (+ num diff)))
     start end)))

(defun count-line-region(begin end)
  "���[�W�����Ԃ̍s����Ԃ�"
  (if (> begin end)
      (rotatef begin end))
  (let ((count 0))
    (save-excursion
      (save-restriction
	(narrow-to-region begin end)
	(goto-char (point-min))
	(loop
	  (setf count (1+ count))
	  (unless (forward-line)
	    (return-from count-line-region count)))))))

(defun countl (begin end)
  "���[�W�����Ԃ̍s�����X�e�[�^�X�o�[�ɕ\��"
  (interactive "r")
  (let ((count (count-line-region begin end)))
    (cond
     ((= count 1) (message "~D�s����." count))
     (t (message "~D�s���炢����." count)))))

(defun each-line-in-region (func s e)
  "�ʒus �ƈʒue �ň͂܂��͈͂ɂ���s(s���܂ލs,e���܂ލs ���܂�)�̊e�s��bol�ʒu��eol�ʒu���֐�func�ɓn��"
  (save-excursion
    (save-restriction
      (if (> s e)
	  (rotatef s e))
      ;�ʒus������s�̐擪��s���Đݒ�
      (setf s (progn (goto-char s) (goto-bol) (point)))
      ;�ʒue������s�̍s����e���Đݒ�
      (setf e (progn (goto-char e) (goto-eol) (point)))
      (narrow-to-region s e)
      (goto-char (point-min))
      (let ((head nil)
	    (tail nil))
	(loop
	  (goto-bol)
	  (setf head (point))
	  (setf tail (progn (goto-eol) (point)))
	  (funcall func head tail)
	  (unless (forward-line)
	    (return)))))))

(defun insert-string-to-bol (start end)
  "���[�W�������̊e�s�̐擪�Ɏw�肳�ꂽ������}�����邷�łɐ擪�ɂ��̕���������Ή������Ȃ�."
  (interactive "*r")
  (let ((str))
    (if (string= "" (setf str (read-string "Insert String:"
					   :default (if comment-start comment-start ""))))
	(progn
	  (message "Canceled")
	  (return-from insert-string-to-bol)))
    (each-line-in-region
     #'(lambda (s e)
	 (let ((this-line (buffer-substring s e)))
	   (if (not (string-match (concat "^" str) this-line))
	       (progn
		 (delete-region s e)
		 (insert (concat str this-line)))))) start end)))

(defun delete-string-from-bol (start end)
  "���[�W�������̊e�s�̐擪����w�肳�ꂽ�������폜����"
  (interactive "*r")
  (let ((str))
    (if (string= "" (setf str (read-string "Delete String:"
					   :default (if comment-start comment-start ""))))
	(progn
	  (message "Canceled")
	  (return-from delete-string-from-bol)))
    (each-line-in-region
     #'(lambda (s e)
	 (let ((this-line (buffer-substring s e)))
	   (if (string-match (concat "^" str) this-line)
	       (progn
		 (setf this-line (substring this-line (match-end 0) nil))
		 (delete-region s e)
		 (insert this-line))))) start end)))

(defun occur (pattern)
  "���݂̃o�b�t�@�݂̂�ΏۂƂ���grep"
  (interactive "sPattern: "
    :default0 *last-grep-regexp*
    :history0 'search)
  (let ((reg (compile-regexp pattern t))
	(scanning-buffer (selected-buffer))
	(count 0)
	(prev-beg -1)
	beg end)
    (with-output-to-temp-buffer ("*occur-out*" t)
      (save-excursion
	(set-buffer scanning-buffer)
	(goto-char (point-min))
	(while (scan-buffer reg :tail t)
	  (setq count (1+ count))
	  (setq beg (save-excursion (progn (goto-char (match-beginning 0)) (goto-bol) (point)))
		end (save-excursion (progn (goto-char (match-end 0)) (goto-eol) (point))))
	  (and (not (= prev-beg beg))
	       (format t "<~A>:~D:~A~%"
		       (buffer-name (selected-buffer))
		       (current-line-number)
		       (buffer-substring beg end)))
	  (setq prev-beg beg))))
    (message (format nil "~D matched." count))))

(defun my-filer-run-console ()
  (interactive)
  (let ((ed::*launch-app-directory* (filer-get-directory)))
    (run-console)))

(defun set-buffer-eol-code-cmd (eol-code)
  (interactive
      (let* ((c2s `(("cr"  . ,*eol-cr*)
		    ("lf"   . ,*eol-lf*)
		    ("crlf" . ,*eol-crlf*)))
	     (eol  (completing-read "eol-type: "
				    '("crlf" "lf" "cr")
				    :must-match t
				    :case-fold t)))
	(list (cdr (assoc eol c2s :test #'string=)))))
  (set-buffer-eol-code eol-code))

(defun my-filer-get-path ()
  (interactive)
  (let ((files (or (filer-get-mark-files)
		   (let ((f (filer-get-current-file)))
		     (and f (list f)))))
	(str ""))
    (when files
      (dolist (f files)
	(setq str (concat str (if (/= (length str) 0) "\n" "") f)))
      (copy-to-clipboard str)
      (message "�t�@�C�������R�s�[���܂���"))))

(defun match-files (dir pat &optional &key regexp)
  "�f�B���N�g��dir�Ƀt�@�C��pat������Ȃ炻������X�g�ɂ��ĕԂ��B
regexp��nil�łȂ��Ƃ��Apat�𐳋K�\���Ƃ݂Ȃ��Ă���Ƀ}�b�`����t�@�C�������ׂăt���p�X�ŕԂ�"
  (if (and regexp
	   (not (regexpp pat)))
      (setq pat (compile-regexp pat :case-fold)))
  (if regexp
      (mapcar (lambda (match-file)
		(merge-pathnames match-file dir))
	      (find-all (lambda (file)
			  (string-match pat file))
			(directory dir :file-only t)))
    (let ((fullpath (merge-pathnames pat dir)))
      (if (file-exist-p fullpath)
	  (list fullpath)))))

(defun env-path-list ()
  "���ϐ�PATH�̊e�l�����X�g�ɂ��ĕԂ�"
  (cons "." (split-string (si:getenv "PATH") ";")))

(defun which (pat &optional &key (regexp nil) (all nil))
  "unix��which�݂����Ȃ�."
  (let ((result (mapcan (lambda (dir)
			  (match-files dir pat :regexp regexp))
			(env-path-list))))
    (if all
	result
      (car result))))

(require "util/zenkaku-hankaku-util")
(add-hook '*post-startup-hook*
	  #'(lambda()
	      (add-menu-item *app-popup-menu* nil "�I��͈͂�S�p�����p"
			     'zenkaku-to-hankaku-selection nil)
	      (add-menu-item *app-popup-menu* nil "�I��͈͂𔼊p���S�p"
			     'hankaku-to-zenkaku-selection nil)))
