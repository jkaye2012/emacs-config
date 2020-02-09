
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (lsp-mode . lsp-ui-mode))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (general-define-key
   :states '(normal)
   :keymaps '(lsp-mode-map)
   :prefix "SPC"
   "l" '(nil :wk "LSP")
   "l=" '(lsp-format-buffer :wk "format")
   "la" '(lsp-ui-find-workspace-symbol :wk "find symbol")
   "ld" '(lsp-find-declaration :wk "find declaration")
   "lf" '(lsp-find-definition :wk "find definition")
   "lh" '(lsp- :wk "find definition")
   "li" '(lsp-find-implementation :wk "find impls")
   "ln" '(lsp-rename :wk "rename")
   "lr" '(lsp-find-references :wk "find references")
   "lt" '(lsp-find-type-definition :wk "find typedef")
   "lp" '(nil :wk "Peek")
   "lpf" '(lsp-ui-peek-find-definitions :wk "definition")
   "lpi" '(lsp-ui-peek-find-implementation :wk "impl")
   "lpr" '(lsp-ui-peek-find-references :wk "references")
   "lps" '(lsp-ui-peek-find-workspace-symbol :wk "symbol")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev))

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
