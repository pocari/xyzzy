;;; -*- Mode: Lisp-Interaction -*-

(provide "timeutil")

(export
 '(get-file-universal-time
   get-file-time
   set-file-access-time
   set-file-create-time
   set-file-modify-time
   get-file-access-time
   get-file-create-time
   get-file-modify-time
   with-file-universal-time
   get-universal-time-from-FILETIME
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "foreign")
  (require "wip/winapi")
  )


(c:define-c-struct FILETIME
  (winapi:DWORD dwLowDateTime)
  (winapi:DWORD dwHighDateTime))

(c:define-c-struct SYSTEMTIME
  (winapi:WORD wYear)
  (winapi:WORD wMonth)
  (winapi:WORD wDayOfWeek)
  (winapi:WORD wDay)
  (winapi:WORD wHour)
  (winapi:WORD wMinute)
  (winapi:WORD wSecond)
  (winapi:WORD wMilliseconds))

(c:*define-dll-entry winapi:LONG GetFileTime (winapi:HANDLE (FILETIME *) (FILETIME *) (FILETIME *)) "kernel32" "GetFileTime")
(c:*define-dll-entry winapi:LONG SetFileTime (winapi:HANDLE (FILETIME *) (FILETIME *) (FILETIME *)) "kernel32" "SetFileTime")
(c:*define-dll-entry c:void GetSystemTime ((SYSTEMTIME *)) "kernel32" "GetSystemTime")
(c:*define-dll-entry winapi:HANDLE CreateFile (winapi:LPCSTR winapi:DWORD winapi:DWORD winapi:LPCSTR winapi:DWORD winapi:DWORD winapi:HANDLE) "kernel32" "CreateFileA")
(c:*define-dll-entry winapi:LONG CloseHandle (winapi:HANDLE) "kernel32" "CloseHandle")
(c:*define-dll-entry winapi:LONG SystemTimeToFileTime    ((SYSTEMTIME *) (FILETIME *)) "kernel32" "SystemTimeToFileTime")
(c:*define-dll-entry winapi:LONG FileTimeToSystemTime    ((FILETIME *) (SYSTEMTIME *)) "kernel32" "FileTimeToSystemTime")
(c:*define-dll-entry winapi:LONG FileTimeToLocalFileTime ((FILETIME *) (FILETIME *)) "kernel32" "FileTimeToLocalFileTime")
(c:*define-dll-entry winapi:LONG LocalFileTimeToFileTime ((FILETIME *) (FILETIME *)) "kernel32" "LocalFileTimeToFileTime")

(c:*define GENERIC_READ #x80000000)
(c:*define GENERIC_WRITE #x40000000)
(c:*define OPEN_EXISTING 3)
(c:*define OPEN_ALWAYS 4)
(c:*define FILE_ATTRIBUTE_NORMAL #x80)

(defmacro with-open-file-handle ((handle filename access-mode disposition) &body body)
  `(let ((,handle (CreateFile (si:make-string-chunk (map-slash-to-backslash ,filename))
			      ,access-mode 0 0 ,disposition FILE_ATTRIBUTE_NORMAL 0)))
     (unwind-protect
	 (multiple-value-prog1
	  (progn ,@body))
       (CloseHandle ,handle))))

(defmacro with-open-read-only-file-handle ((handle filename) &body body)
  `(with-open-file-handle (,handle ,filename GENERIC_READ OPEN_EXISTING)
     ,@body))

(defmacro with-open-writable-file-handle ((handle filename) &body body)
  `(with-open-file-handle (,handle ,filename GENERIC_WRITE OPEN_EXISTING)
     ,@body))

(setf (get 'with-open-file-handle 'ed::lisp-indent-hook) 1)
(setf (get 'with-open-read-only-file-handle 'ed::lisp-indent-hook) 1)
(setf (get 'with-open-writable-file-handle 'ed::lisp-indent-hook) 1)

(defun convert-system-time (year month dayofweek day hour sec millsec)
  (let ((systime (make-SYSTEMTIME)))
    (setf (SYSTEMTIME-wYear systime) year)
    (setf (SYSTEMTIME-wMonth systime) month)
    (setf (SYSTEMTIME-wDayOfWeek systime) dayofweek)
    (setf (SYSTEMTIME-wDay systime) day)
    (setf (SYSTEMTIME-wHour systime) hour)
    (setf (SYSTEMTIME-wSecond systime) sec)
    (setf (SYSTEMTIME-wMilliseconds systime) millsec)
    systime))

(defun system-time-to-universal-time (systime)
  (encode-universal-time
   (SYSTEMTIME-wSecond systime)
   (SYSTEMTIME-wMinute systime)
   (SYSTEMTIME-wHour systime)
   (SYSTEMTIME-wDay systime)
   (SYSTEMTIME-wMonth systime)
   (SYSTEMTIME-wYear systime)
   -9))

(defun universal-time-to-system-time (universal-time)
  (let ((systime (make-SYSTEMTIME)))
    (multiple-value-bind (sec min hour day mon year)
	(decode-universal-time universal-time)
      (setf (SYSTEMTIME-wSecond systime) sec)
      (setf (SYSTEMTIME-wMinute systime) min)
      (setf (SYSTEMTIME-wHour systime) hour)
      (setf (SYSTEMTIME-wDay systime) day)
      (setf (SYSTEMTIME-wMonth systime) mon)
      (setf (SYSTEMTIME-wYear systime) year)
      (setf (SYSTEMTIME-wDayOfWeek systime) 0)
      (setf (SYSTEMTIME-wMilliseconds systime) 0))
    systime))

(defun file-time-to-local-file-time (file-time)
  (let ((local-file-time (make-FILETIME)))
    (FileTimeToLocalFileTime file-time local-file-time)
    local-file-time))

(defun local-file-time-to-system-time (local-file-time)
  (let ((system-time (make-SYSTEMTIME)))
    (FileTimeToSystemTime local-file-time system-time)
    system-time))

(defun file-time-to-system-time (file-time)
  (local-file-time-to-system-time (file-time-to-local-file-time file-time)))

(defun system-time-to-local-file-time (systime)
  (let ((local-file-time (make-FILETIME)))
    (SystemTimeToFileTime systime local-file-time)
    local-file-time))

(defun local-file-time-to-file-time (local-file-time)
  (let ((file-time (make-FILETIME)))
    (LocalFileTimeToFileTime local-file-time file-time)
    file-time))

(defun file-time-to-universal-time (file-time)
  (system-time-to-universal-time
   (local-file-time-to-system-time
    (file-time-to-local-file-time file-time))))

(defun universal-time-to-file-time (universal-time)
  (local-file-time-to-file-time
   (system-time-to-local-file-time
    (universal-time-to-system-time universal-time))))

(defun get-universal-time-from-FILETIME (filename)
  (let ((tmp1 (make-FILETIME))
	(tmp2 (make-FILETIME))
	(tmp3 (make-FILETIME)))
    (with-open-read-only-file-handle (h filename)
      (GetFileTime h tmp1 tmp2 tmp3)
      (values (file-time-to-universal-time tmp1)
	      (file-time-to-universal-time tmp2)
	      (file-time-to-universal-time tmp3)))))

(defun set-file-time (filename &key (create nil) (access nil) (modify nil))
  (with-open-writable-file-handle (h filename)
    (SetFileTime
     h
     (if create (universal-time-to-file-time create) 0)
     (if access (universal-time-to-file-time access) 0)
     (if modify  (universal-time-to-file-time modify) 0))))

(defun dump-filetime (ftime)
  (format t
	  "dwHighDateTime: ~A~%dwLowDateTime: ~A~%"
	  (FILETIME-dwHighDateTime ftime) (FILETIME-dwLowDateTime ftime)))

(defun dump-systemtime (stime)
  (format t "~A年~A月~A日(~[日~;月~;火~;水~;木~;金~;土~]) ~A時~A分~A秒"
	  (SYSTEMTIME-wYear stime)
	  (SYSTEMTIME-wMonth stime)
	  (SYSTEMTIME-wDay stime)
	  (SYSTEMTIME-wDayOfWeek stime)
	  (SYSTEMTIME-wHour stime)
	  (SYSTEMTIME-wMinute stime)
	  (SYSTEMTIME-wSecond stime)))

;--------------------------------------------------
; APIs
;--------------------------------------------------
(defun get-file-universal-time (filename)
  "filenameの作成日時、アクセス日付、更新日時のユニバーサル形式をキーcreate, access modifyの連想リストとして返す"
  (multiple-value-bind (create access modify)
      (get-universal-time-from-FILETIME filename)
    `((create . ,create)
      (access . ,access)
      (modify . ,modify))))

(defun get-file-time (filename)
  "fileの作成日時、アクセス日付、更新日時を「日時形式」のリストを返す"
  (mapcar (lambda (as)
	    `(,(car as) ,(multiple-value-list (decode-universal-time (cdr as)))))
	  (get-file-universal-time filename)))

(defmacro with-file-universal-time (file (create access modify) &body body)
  "fileの作成日時、アクセス日付、更新日時のユニバーサル形式をそれぞれcreate access modifyにバインドしてbodyを実行する。"
  `(multiple-value-bind (,create ,access ,modify)
       (get-universal-time-from-FILETIME ,file)
     (progn
       ,@body)))
(setf (get 'with-file-universal-time 'ed::lisp-indent-hook) 1)

(defun set-file-access-time (file time)
  "fileの最終アクセス日付をtime(ユニバーサル形式)に変更する"
  (set-file-time file :access time))

(defun get-file-access-time (file)
  (with-file-universal-time file (c a m) a))

(defsetf get-file-access-time (file) (time)
  `(progn
     (set-file-access-time ,file ,time)
     ,time))

(defun set-file-create-time (file time)
  "fileの作成日時をtime(ユニバーサル形式)に変更する"
  (set-file-time file :create time))

(defun get-file-create-time (file)
  (with-file-universal-time file (c a m) c))

(defsetf get-file-create-time (file) (time)
  `(progn
     (set-file-create-time ,file ,time)
     ,time))

(defun set-file-modify-time (file time)
  "fileの最終更新日時をtime(ユニバーサル形式)に変更する。set-file-write-timeと一緒。"
  (set-file-time file :modify time))

(defun get-file-modify-time (file)
  (with-file-universal-time file (c a m) m))

(defsetf get-file-modify-time (file) (time)
  `(progn
     (set-file-modify-time ,file ,time)
     ,time))

#|

(with-file-universal-time "d:/dev/tmp/20111018/l/test.dat" (c a m)
  (format t "access:~A create:~A modify:~A~%"
	  (format-date-string "%Y/%m/%d %H:%M:%S" a)
	  (format-date-string "%Y/%m/%d %H:%M:%S" c)
	  (format-date-string "%Y/%m/%d %H:%M:%S" m)))

(set-file-create-time "d:/dev/tmp/20111018/l/test.dat" (parse-date-string "2011/10/21 12:43:22"))
(setf (get-file-create-time "d:/dev/tmp/20111018/l/test.dat") (parse-date-string "2011/10/22 12:43:22"))

|#
