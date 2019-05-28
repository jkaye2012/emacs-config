
(use-package ace-window
  :after (general)
  :config
  (setq aw-keys '(?q ?w ?e ?r))
  (setq aw-ignore-current t)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "w" '(nil :wk "Window")
   "wh" '(evil-window-left :wk "left")
   "wj" '(evil-window-down :wk "down")
   "wk" '(evil-window-up :wk "up")
   "wl" '(evil-window-right :wk "right")
   "wg" '(ace-window :wk "ace")
   "wd" '(delete-window :wk "delete")
   "wm" '(delete-other-windows :wk "maximize")
   "ws" '(hydra-window-select/body :wk "select")
   "w/" '(evil-window-vsplit :wk "split vertically")
   "w-" '(evil-window-split :wk "split horizontally")
   ))

(use-package evil-visualstar
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package evil-matchit
  :hook (prog-mode . evil-matchit-mode))

(general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "m" '(nil :wk "Bookmark")
   "mc" '(counsel-bookmark :wk "counsel")
   "mj" '(bookmark-jump :wk "jump")
   "ms" '(bookmark-set :wk "set"))

(use-package projectile
  :hook (after-init . projectile-mode))

(use-package counsel-projectile
  :after (counsel)
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "p" '(nil :wk "In project")
    "pb" '(projectile-switch-to-buffer :wk "switch buffer")
    "pf" '(counsel-projectile-find-file :wk "find file")
    "pk" '(projectile-kill-buffers :wk "kill buffers")
    "pr" '(projectile-replace :wk "replace")
    "p/" '(counsel-projectile-ag :wk "search")))

(use-package avy
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "j"  '(nil :wk "Jump to")
   "jc" '(avy-goto-char :wk "char")
   "jw" '(avy-goto-word-1 :wk "word")
   "jl" '(avy-goto-line :wk "line")))
