
(use-package google-this
  :config
  (general-define-key
     :states '(normal visual insert emacs)
     :prefix "SPC"
     :non-normal-prefix "M-SPC"
      "s" '(nil :wk "Search")
      "sg" '(google-this :wk "at point")
      "ss" '(google-this-search :wk "string")
      "sw" '(google-this-word :wk "word")
      "sr" '(google-this-region :wk "region"))
  )

(use-package flx)

(use-package smex)
