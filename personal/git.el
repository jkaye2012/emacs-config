(use-package magit
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "g" '(nil :wk "Git")
   "gb" '(magit-blame-addition :wk "blame")
   "gs" '(magit-status :wk "status")
   "gd" '(magit-diff :wk "diff")))

(use-package evil-magit)
