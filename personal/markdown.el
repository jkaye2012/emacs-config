
(use-package markdown-mode
  :hook (markdown-mode . flyspell-mode)

  :config
  (general-define-key
   :states '(normal)
   :keymaps 'markdown-mode-map
   :prefix ","
    "p" '(markdown-live-preview-mode :wk "live preview"))
  )
