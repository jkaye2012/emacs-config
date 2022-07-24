(use-package rustic
  :config
  (general-define-key
   :keymaps '(rustic-mode-map)
   :states '(normal)
   :prefix ","
   "c" '(nil :wk "cargo")
   "cc" '(lsp-rust-analyzer-open-cargo-toml :wk "Cargo.toml")
   "cr" '(lsp-rust-analyzer-related-tests :wk "Related tests")
   "d" '(nil :wk "docs")
   "dd" '(lsp-rust-analyzer-open-external-docs :wk "Open externally")
   )
  )
