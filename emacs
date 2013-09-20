(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless window-system
  (menu-bar-mode -1))

(when (memq window-system '(mac ns))
  (set-frame-font "Monaco")
  (set-frame-size (selected-frame) 100 55))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(setq vc-follow-symlinks t)
(setq ring-bell-function 'ignore)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl-lib)
(defvar package-list
  '(rainbow-mode
    molokai-theme
    slime
    exec-path-from-shell
    evil))

(defun list-uninstalled-packages ()
  (cl-remove-if #'package-installed-p package-list))

(let ((uninstalled-packages (list-uninstalled-packages)))
  (when uninstalled-packages
    (package-refresh-contents)
    (mapcar #'package-install uninstalled-packages)))

(load-theme 'molokai t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-repl slime-banner))
