;;; bootstrap.el --- hexo-renderer-orgmode's emacs init file.

;;; Commentary:

;;; Code:

(setq debug-on-error t)

(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(defconst m/os
  (let ((os (symbol-name system-type)))
    (cond ((string= os "darwin") 'macos)
          ((string-prefix-p "gnu" os) 'linux)
          ((or (string-prefix-p "ms" os) (string-prefix-p "windows" os)) 'windows))))


(setq gc-cons-threshold 100000000)

(defvar m/root (file-name-directory (or load-file-name (buffer-file-name))))

(require 'package)
;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         m/root)))
  (setq package-user-dir versioned-package-dir))


(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (if no-ssl
      (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			       ("melpa" . "http://elpa.emacs-china.org/melpa/")
			       ("org" . "http://elpa.emacs-china.org/org/")))
    (unless no-ssl
      (setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
			       ("melpa" . "https://elpa.emacs-china.org/melpa/")
			       ("org" . "https://elpa.emacs-china.org/org/"))))))
;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (version= "26.2" emacs-version)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(add-to-list 'load-path m/root)

;; Ignore all directory-local variables in `.dir-locals.el', whick make Emacs stucks there.
(setq enable-dir-local-variables nil)
;; Allow use #+BIND: in org-mode
(setq org-export-allow-bind-keywords t)

;;;; Public variables
(defvar hexo-renderer-org-cachedir "./hexo-org-cache"
  "Cache directory to save generated result and Emacs packages to increase startup speed.")

(defvar hexo-renderer-org-user-config ""
  "User's personal init.el to install extra packages or customize their own `org-mode' exporter.")

(defvar hexo-renderer-org-theme ""
  "User's theme to use, it's recommand to use Emacs's builtin theme here.")

(defvar hexo-renderer-org-common-block "#+OPTIONS: html-postamble:nil num:nil toc:nil ^:nil"
  "Common `org-mode' settings in string, like #+OPTIONS: html-postamble:nil num:nil toc:nil ^:nil .")

(defvar hexo-renderer-org-htmlize "false"
  "Enable use Emacs's htmlize package to renderer code block or not.")

;;;; Private variables
(defvar hexo-renderer-org--debug-file "./hexo-org-renderer.log"
  "YOU SHOULD NOT SETUP THIS VARIABLE.")

(defvar hexo-renderer-org--use-htmlize nil
  "YOU SHOULD NOT SETUP THIS VARIABLE.
This variable is keeped incase org-hexo not loaded.")


(use-package org
  :ensure org-plus-contrib)
(use-package ox-html
  :ensure nil)
(use-package htmlize
  :if hexo-renderer-org--use-htmlize)
(use-package ox-hexo
  :load-path m/root
  :commands (org-hexo-export-as-html))

;;;; Debugger

(defun hexo-org-renderer-oops (msg)
  "OOPS: something error, let's show the MSG and kill EMACS :(."
  (use-package json)                       ; built-in
  (let ((oops '(:success :json-false)))
    (plist-put oops :message msg)
    ;; Convert to JSON format and write to `*deebug-file*'
    (with-temp-buffer
      (insert (json-encode oops))
      (write-region (point-min) (point-max) hexo-renderer-org--debug-file))
    ;; bye-bye emacs
    (kill-emacs)))

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
  (let ((file         (or (plist-get args :file)             ""))
        (output-file  (or (plist-get args :output-file)      "")))
    ;; Export file content by ox-hexo.el
    (with-temp-buffer
      (insert-file-contents file)
      (hexo-renderer-org-insert-options hexo-renderer-org-common-block)
      (org-hexo-export-as-html)
      (write-region (point-min) (point-max) output-file)
      (kill-buffer))))

;; Emacs is Ready!!!
(message "Emacs is READY!!!!!")

(provide 'bootstrap)
;;; bootstrap.el ends here
