
(use-package rtags
  :after company
;  :hook ((c++-mode . rtags-start-process-unless-running))
  :config

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix ","
   :non-normal-prefix "M-,"
    "r" '(nil :wk "Rtags")
    "r!" '(rtags-restart-process :wk "restart process")
    "r-" '(rtags-location-stack-back :wk "jump back")
    "r+" '(rtags-location-stack-forward :wk "jump forward")
    "rf" '(rtags-find-symbol-at-point :wk "find symbol")
    "rh" '(rtags-get-include-file-for-symbol :wk "insert include")
    "ri" '(rtags-imenu :wk "imenu")
    "rn" '(rtags-rename-symbol :wk "rename symbol")
    "rr" '(rtags-find-references-at-point :wk "find references")
    "rs" '(rtags-symbol-info :wk "symbol info")
    "rt" '(rtags-symbol-type :wk "symbol type")
    "rv" '(rtags-find-virtuals-at-point :wk "find virtuals")
    ))

(use-package ivy-rtags
  :after rtags
  :config
  (setq rtags-display-result-backend 'ivy))
