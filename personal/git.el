(use-package magit
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "g" '(nil :wk "Git")
   "gb" '(magit-blame :wk "blame")
   "gs" '(magit-status :wk "status")
   "gd" '(magit-diff :wk "diff")))

(use-package magit-todos
  :hook (after-init . magit-todos-mode)
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "gt" '(magit-todos-list :wk "todos")))
