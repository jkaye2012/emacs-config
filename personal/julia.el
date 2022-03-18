(use-package lsp-julia
  :hook (julia-mode . lsp)

  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.7"))

(require 'lsp-julia)
