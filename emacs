;; -*- mode: emacs-lisp -*-
(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless window-system
  (menu-bar-mode -1))

(when (memq window-system '(mac ns))
  (set-frame-font "Monaco")
  (set-frame-size (selected-frame) 100 55))

(setq inhibit-startup-message t
      initial-scratch-message nil
      mac-option-modifier nil
      mac-command-modifier 'meta
      vc-follow-symlinks t
      ring-bell-function 'ignore)

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-.") 'kill-region)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-S-f") 'scroll-up-command)
(global-set-key (kbd "C-S-b") 'scroll-down-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "RET") 'newline-and-indent)

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
    evil
    helm
    key-chord))

(defun list-uninstalled-packages ()
  (cl-remove-if #'package-installed-p package-list))

(let ((uninstalled-packages (list-uninstalled-packages)))
  (when uninstalled-packages
    (package-refresh-contents)
    (mapcar #'package-install uninstalled-packages)))

(load-theme 'molokai t)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(evil-mode)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

(setq inferior-lisp-program "sbcl"
      common-lisp-hyperspec-root (expand-file-name "~/Documents/HyperSpec/"))

(setq slime-lisp-implementations
      '((sbcl    ("sbcl"))
	(ccl     ("ccl"))
	(allegro ("alisp"))
	(clisp   ("clisp"))))

(slime-setup
 '(slime-repl
   slime-banner
   slime-editing-commands
   slime-fuzzy
   slime-presentations))
