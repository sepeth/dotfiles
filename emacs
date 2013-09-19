(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar package-list
  '(rainbow-mode
    monokai-theme))

(defun package-list-installed-p ()
  (cl-loop for p in package-list
	   when (not (package-installed-p p)) do (cl-return nil)
	   finally (cl-return t)))

(unless (package-list-installed-p)
  (package-refresh-contents)
  (dolist (p package-list)
    (or (package-installed-p p)
	(package-install p))))

(load-theme 'monokai t)
