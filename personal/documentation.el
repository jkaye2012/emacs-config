
(use-package zeal-at-point
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
    "d" '(nil :wk "Documentation")
    "dd" '(zeal-at-point :wk "at point")
    "ds" '(zeal-at-point-search :wk "search")))
