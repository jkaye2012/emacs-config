
(use-package lsp-haskell
  :ensure t
  :hook ((haskell-mode . lsp))
  :config
  (setq lsp-haskell-process-path-hie "/usr/local/bin/haskell-language-server-wrapper"))
