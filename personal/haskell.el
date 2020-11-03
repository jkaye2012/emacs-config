
(general-define-key
 :keymaps '(haskell-mode-map)
 :states '(normal)
 :prefix ","
  "c" '(compile :wk "Compile"))

(use-package lsp-haskell
  :ensure t
  :hook ((haskell-mode . lsp)
         (haskell-mode . lsp-ui-doc-mode))
  :config
  (setq lsp-haskell-process-path-hie "/usr/local/bin/haskell-language-server-wrapper"))
