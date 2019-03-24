
(use-package haskell-mode
  :config
  (require 'haskell-doc))

(use-package intero
  :config
  (intero-global-mode 1)
  (general-define-key
   :states '(normal)
   :keymaps '(intero-mode-map)
   :prefix ","
    "r" '(intero-restart :wk "restart")
    ))
