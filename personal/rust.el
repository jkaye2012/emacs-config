
(use-package rust-mode
  :hook ((rust-mode . lsp)
         (rust-mode . (lambda () (setq-local my/lsp-should-format-buffer nil))))
  
  :config
  (setq rust-format-on-save t)
  (general-define-key
   :states '(normal)
   :keymaps 'rust-mode-map
   :prefix ","
   "b" '(rust-compile :wk "build")
   "f" '(rust-format-buffer :wk "format")
   "l" '(rust-run-clippy :wk "lint")
   "p" '(nil :wk "Playpen")
   "pb" '(rust-playpen-buffer :wk "buffer")
   "pr" '(rust-playpen-region :wk "region")
   "r" '(rust-run :wk "run")
   "t" '(rust-test :wk "test")
   "ee" '(eval-defun :wk "defun")))

(use-package cargo
  :after (rust-mode)
  :hook ((rust-mode . cargo-minor-mode))
  :config

  (general-define-key
   :states '(normal)
   :keymaps 'rust-mode-map
   :prefix ","
    "c" '(nil :wk "Cargo")
    "ca" '(cargo-process-add :wk "add")
    "cb" '(cargo-process-bench :wk "benchmark")
    "cc" '(cargo-process-clean :wk "clean")
    "cd" '(cargo-process-doc :wk "doc")
    "cg" '(cargo-process-upgrade :wk "upgrade")
    "ci" '(cargo-process-init :wk "init")
    "cn" '(cargo-process-new :wk "new")
    "co" '(cargo-process-doc :wk "open doc")
    "cr" '(cargo-process-rm :wk "remove")
    "cs" '(cargo-process-search :wk "search")
    "cu" '(cargo-process-update :wk "update")
    "t" '(nil :wk "Test")
    "tc" '(cargo-process-current-test :wk "current")
    "tf" '(cargo-process-current-file-tests :wk "file")
    "tt" '(cargo-process-test :wk "all")
    ))
