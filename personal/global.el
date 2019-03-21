;; "Global" packages.
;; These are packages that many of my personal configurations will required.
;; By loading them before the personalized configs, I won't have to worry about
;; deferring everywhere.

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
   )

  (general-define-key
   :states '(normal)
   :keymaps 'emacs-lisp-mode-map
   :prefix ","
   "e" '(nil :wk "Evaluate")
   "eb" '(eval-buffer :wk "buffer")
   "ee" '(eval-defun :wk "defun")))

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
    ("W" sp-unwrap-sexp "unwrap"))

  (defhydra hydra-tide-reference ()
    "Navigate references"
    ("n" tide-find-next-reference "next")
    ("p" tide-find-previous-reference "previous"))
  )