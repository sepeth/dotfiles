(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(if window-system
    (set-frame-size (selected-frame) 100 55))

(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(setq vc-follow-symlinks t)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl-lib)
(defvar package-list
  '(rainbow-mode
    monokai-theme
    slime
    exec-path-from-shell))

(defun list-uninstalled-packages ()
  (cl-remove-if #'package-installed-p package-list))

(let ((uninstalled-packages (list-uninstalled-packages)))
  (when uninstalled-packages
    (package-refresh-contents)
    (mapcar #'package-install uninstalled-packages)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(load-theme 'monokai t)

(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-banner))
