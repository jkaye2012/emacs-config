
(use-package markdown-mode
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'markdown-mode-map
   :prefix ","
    "p" '(markdown-live-preview-mode :wk "live preview"))
  )
