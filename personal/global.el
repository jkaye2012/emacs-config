;; "Global" configuration.
;; These are packages that many of my personal configurations will required.
;; By loading them before the personalized configs, I won't have to worry about
;; deferring everywhere.

;; Initialize package system

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(unless (require 'use-package nil 'noerror)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Emacs-wide defaults

(setq backup-directory-alist '(("." . "~/.emacs-saves")))
(recentf-mode t)
(setf recentf-max-saved-items 50)
(run-at-time nil (* 5 60)
             (lambda ()
               (let ((inhibit-message t))
                 (recentf-save-list))))
(setq-default recent-save-file "~/.emacs.d/recentf")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(font-lock-add-keywords 'prog-mode
  '(("TODO" . font-lock-warning-face)))
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(setq compilation-scroll-output t)
(setq custom-file (expand-file-name "custom.el" user-base-dir))
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Global packages

(use-package evil
  :init
  (setq-default evil-want-C-i-jump nil)
  (setq evil-want-keybinding nil)
  :config
  (setq evil-disable-insert-state-bindings t)
  (global-set-key (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "<tab>") 'evil-indent-line)
  (define-key evil-motion-state-map (kbd ",") nil)
  (evil-mode 1))

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.125)
  (which-key-mode))

(use-package general
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  (defun my/find-config ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

  (general-define-key
   :states '(normal)
    "C-j" '(evil-paste-pop :wk "Paste previous")
    "C-k" '(evil-paste-pop-next :wk "Paste next"))

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(counsel-M-x :wk "Execute command")
   "'" '(eshell :wk "Eshell")
   "\\" '(my/find-config :wk "Edit config")
   "/" '(swiper :wk "Interactive search")

   "b" '(nil :wk "Buffer")
   "bb" '(ivy-switch-buffer :wk "list")
   "bd" '(evil-delete-buffer :wk "delete")
   "bk" '(kill-this-buffer :wk "kill")
   "bp" '(my/switch-to-previous-buffer :wk "previous")
   "br" '(counsel-recentf :wk "list")

   "f" '(nil :wk "File")
   "ff" '(counsel-find-file :wk "find")

   "h"  '(nil :wk "Help")
   "ha" '(counsel-apropos :wk "apropos")
   "hd" '(nil :wk "Describe")
   "hdv" '(counsel-describe-variable :wk "variable")
   "hdf" '(counsel-describe-function :wk "function")
   "hdk" '(describe-key :wk "key")

   "i" '(counsel-imenu :wk "menu")

   "W" '(venv-workon :wk "Choose virtualenv")
   ))

(use-package hydra
  :config
  (defhydra hydra-window-select ()
    "Select window"
    ("." nil "exit")
    ("h" evil-window-left "left")
    ("j" evil-window-down "down")
    ("k" evil-window-up "up")
    ("l" evil-window-right "right"))

  (defhydra hydra-sexpr ()
    "Sexpr"
    ("s" sp-forward-slurp-sexp "slurp forward")
    ("b" sp-forward-barf-sexp "slurp backward")
    ("W" sp-unwrap-sexp "unwrap"))

  (defhydra hydra-tide-reference ()
    "Navigate references"
    ("n" tide-find-next-reference "next")
    ("p" tide-find-previous-reference "previous"))
  )

(use-package counsel
  :bind (:map ivy-minibuffer-map
	 ("C-u" . ivy-scroll-down-command)
	 ("C-d" . ivy-scroll-up-command)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-immediate-done)

	 :map counsel-find-file-map
	 ("C-h" . counsel-up-directory))

  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
        ivy-use-virtual-buffers t
	ivy-height 20)
  (ivy-mode 1)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
				(t      . ivy--regex-fuzzy))))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))
