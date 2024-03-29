;;; bootstrap.el --- hexo-renderer-orgmode's emacs init file.

;;; Commentary:

;;; Code:

;;;; Public variables

(defvar hexo-renderer-org-cachedir ".cache.d"
  "Cache directory to save generated result and Emacs packages to increase startup speed.")

(defvar hexo-renderer-org-theme ""
  "User's theme to use, it's recommand to use Emacs's builtin theme here.")

(defvar hexo-renderer-org-common-block "#+OPTIONS: html-postamble:nil num:nil toc:nil ^:nil"
  "Common `org-mode' settings in string, like #+OPTIONS: html-postamble:nil num:nil toc:nil ^:nil .")

(defvar hexo-renderer-org-debug t
  "Enable debug message or not.")

(defvar hexo-renderer-org-emacs-offlinedir ".offline.d")

(setq debug-on-error t)

(defconst m/filepath (file-name-directory (or load-file-name (buffer-file-name))))

(defconst m/os
  (let ((os (symbol-name system-type)))
    (cond ((string= os "darwin") 'macos)
          ((string-prefix-p "gnu" os) 'linux))))

(defvar m/offline
  (and (not (string= "" hexo-renderer-org-emacs-offlinedir))
       (file-exists-p hexo-renderer-org-emacs-offlinedir)
       (file-directory-p hexo-renderer-org-emacs-offlinedir)))

(defconst m/cache.d
  (let ((cachedir (cond ((and (not (string= "" hexo-renderer-org-cachedir))
			      (file-name-absolute-p  hexo-renderer-org-cachedir)) hexo-renderer-org-cachedir)
			(t (expand-file-name ".cache.d" m/filepath)))))
    (cond ((not (file-directory-p cachedir)) (make-directory cachedir))
	  (t cachedir))))

(setq user-emacs-directory (expand-file-name "emacs.d" m/cache.d))

(add-to-list 'load-path user-emacs-directory)

(unless hexo-renderer-org-debug
  (setq inhibit-message t))

(setq gc-cons-threshold 100000000)
;; Ignore all directory-local variables in `.dir-locals.el', whick make Emacs stucks there.
(setq enable-dir-local-variables nil)

(setq make-backup-files nil)
					; stop to create the lock files
					; lock files is used to prevent concurrent edit of a file
(setq create-lockfiles nil)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (version= "26.2" emacs-version)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)
;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(if m/offline
    (setq package-archives `(("gnu" . ,(expand-file-name "gnu" hexo-renderer-org-emacs-offlinedir))
			     ("melpa" . ,(expand-file-name "melpa" hexo-renderer-org-emacs-offlinedir))
			     ("org" . ,(expand-file-name "org" hexo-renderer-org-emacs-offlinedir))))
  (let* ((no-ssl (and (memq system-type '(windows-nt m11s-dos))
		      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    (message "remote")
    (setq package-archives `(("gnu"   . ,(concat proto "://elpa.gnu.org/packages/"))
			     ("melpa" . ,(concat proto "://melpa.org/packages/"))
			     ("org" . ,(concat proto "://orgmode.org/elpa/"))))))

(setq load-prefer-newer t)
(package-initialize)
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

(unless (package-installed-p 'auto-compile)
  (package-refresh-contents)
  (package-install 'auto-compile))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(use-package exec-path-from-shell
  :config
  (when (memq m/os '(macos linux))
    (exec-path-from-shell-initialize)))

(use-package cnfonts
  :ensure t
  :if (display-graphic-p)
  :init (setq cnfonts-verbose nil)
  :config
  (setq cnfonts-use-face-font-rescale t)
  (cnfonts-enable))

(use-package org
  :pin gnu
  :custom
  (org-src-fontify-natively t)
  (org-export-allow-bind-keywords t)
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts '{})
  (org-export-coding-system 'utf-8)
  :config
  (unless (and (boundp 'org-ditaa-jar-path)
	       (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa.jar")
	  (url "https://github.com/stathissideris/ditaa/releases/download/v0.11.0/ditaa-0.11.0-standalone.jar"))
      (setq org-ditaa-jar-path (expand-file-name jar-name m/cache.d))
      (unless (file-exists-p org-ditaa-jar-path)
	(url-copy-file url org-ditaa-jar-path))))

  (unless (and (boundp 'org-plantuml-jar-path)
	       (file-exists-p org-plantuml-jar-path))
    (let ((jar-name "plantuml.jar")
	  (url "https://downloads.sourceforge.net/project/plantuml/1.2020.2/plantuml.1.2020.2.jar"))
      (setq org-plantuml-jar-path (expand-file-name jar-name m/cache.d))
      (unless (file-exists-p org-plantuml-jar-path)
	(url-copy-file url org-plantuml-jar-path))))
  ;; Allow use #+BIND: in org-mode
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (dot . t)
     (ditaa . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (shell . t)
     (sql . t)
     (sqlite . t))))

(use-package ox-hexo
  :load-path m/filepath
  :commands (org-hexo-export-as-html))

;;;; Debugger

(defun hexo-org-renderer-oops (msg)
  "OOPS: something error, let's show the MSG and kill EMACS :(."
  (princ msg)
  (kill-emacs))

;; The emacs daemon SHOULD die when error occurs.
(run-with-idle-timer
 1 t (lambda ()
       ;; When *Backtrace* exist, which means error occured, set `*statue*' to false and write value to `debug-file' then exit.
       (when (get-buffer "*Backtrace*")
         (with-current-buffer "*Backtrace*"
           (hexo-org-renderer-oops (buffer-string))))
       ;; Sometimes, there's another error "End of file during parsing:", this error may not trow Error to emacs but just display on *Messages* buffer.
       (with-current-buffer "*Messages*"
         (goto-char (point-min))
         (while (re-search-forward "End of file during parsing" nil t)
           (hexo-org-renderer-oops (buffer-string))))
       ))


(defun hexo-renderer-org-insert-options (s)
  "Insert common option and settings S to current `org-mode' document.
The string S will be prepent at beginning of file."
  (save-excursion
    (goto-char (point-min))
    (newline-and-indent)
    (insert s)
    (newline-and-indent)))

(defun hexo-render (args)
  "ARGS is a plist which contain following properities:

ARGS:
 (
 :file         \"File path to render\"
 :output-file  \"Output file which redner by org-hexo\"
 )"
  (let* ((file         (or (plist-get args :file)             ""))
	 (output-file  (or (plist-get args :output-file)      ""))
	 (exebuf (find-file-noselect file)))
    (with-current-buffer exebuf
      (ignore-errors (make-directory (expand-file-name (file-name-base (buffer-file-name exebuf)) (file-name-directory (buffer-file-name exebuf)))))
      (org-babel-execute-buffer)
      (with-temp-buffer
	(insert-buffer-substring exebuf)
	(setq org-hexo-export-src-file (buffer-file-name exebuf))
	(hexo-renderer-org-insert-options hexo-renderer-org-common-block)
	(org-hexo-export-as-html)
	(write-region (point-min) (point-max) output-file)
	(kill-buffer))
      (set-buffer-modified-p nil)
      (kill-buffer exebuf))))


;; Emacs is Ready!!!
(message "Emacs is READY!!!!!")

(provide 'bootstrap)
;;; bootstrap.el ends here
