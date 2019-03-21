
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

(general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "m" '(nil :wk "Bookmark")
   "mc" '(counsel-bookmark :wk "counsel")
   "mj" '(bookmark-jump :wk "jump")
   "ms" '(bookmark-set :wk "set")
   )
