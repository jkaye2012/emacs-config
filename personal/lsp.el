(setq my/lsp-should-format-buffer t)

(defun my/lsp-format-buffer ()
  (interactive)
  (when (and (boundp 'lsp-mode) lsp-mode my/lsp-should-format-buffer)
    (lsp-format-buffer)))

(use-package lsp-mode
  :after (company)
  :hook ((c++-mode . lsp)
         (lsp-mode . lsp-ui-mode)
         (before-save . my/lsp-format-buffer))

  :bind (:map company-active-map
         ("C-j". company-select-next)
         ("C-k". company-select-previous)
         ("RET". company-complete-selection))
 
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-xref nil)
  (setq lsp-clients-clangd-args '("-j=2" "--log=info" "--background-index" "--clang-tidy" "--header-insertion=iwyu" "--pch-storage=disk"))

  (general-define-key
   :states '(normal)
   :keymaps 'lsp-mode-map
   :prefix "SPC"
    "l" '(nil :wk "LSP")
    "l=" '(lsp-format-buffer :wk "format")
    "la" '(lsp-ui-find-workspace-symbol :wk "find symbol")
    "lc" '(lsp-ui-doc-mode :wk "toggle docs")
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
    "lps" '(lsp-ui-peek-find-workspace-symbol :wk "symbol")
    "ls" '(nil :wk "Server")
    "lsr" '(lsp-workspace-restart :wk "restart")))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (define-key lsp-ui-peek-mode-map (kbd "C-j") 'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "C-k") 'lsp-ui-peek--select-prev))

(use-package company-lsp)

(use-package lsp-ivy)

; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
