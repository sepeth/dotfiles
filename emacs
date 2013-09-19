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
    monokai-theme
    slime))

(defun list-uninstalled-packages ()
  (cl-remove-if #'package-installed-p package-list))

(let ((uninstalled-packages (list-uninstalled-packages)))
  (when uninstalled-packages
    (package-refresh-contents)
    (mapcar #'package-install uninstalled-packages)))

(load-theme 'monokai t)