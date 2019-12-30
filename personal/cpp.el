
(use-package cmake-mode)

(use-package cquery
  :config
  (setq cquery-executable "/usr/local/bin/cquery")
  (setq cquery-project-roots '("/home/jkaye/git/Harbor/Laser"))
  (setq xref-prompt-for-identifier '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references))

  (general-define-key
   :states '(normal)
   :keymaps '(c++-mode-map)
   :prefix ","
    "g" '(xref-find-definitions :wk "find definition")
    "h" '(projectile-find-other-file :wk "header/source jump")
    "i" '(lsp-goto-implementation :wk "find implementation")
    "e" '(nil :wk "Error")
    "en" '(flymake-goto-next-error :wk "next")
    "ep" '(flymake-goto-prev-error :wk "previous")
    "p" '(nil :wk "Peek")
    "pb" '(lsp-ui-peek-jump-backward :wk "backward")
    "pf" '(lsp-ui-peek-jump-forward :wk "forward")
    "ps" '(lsp-ui-peek-find-workspace-symbol :wk "symbol")
    "r" '(xref-find-references :wk "find references")
    )

  :init
  (add-hook 'lsp-mode-hook '(lambda ()
                              (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)))
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))

(use-package ivy-xref
  :ensure t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(defun my/c++-indentation ()
  (c-set-offset 'access-label -1)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro-cont 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-intro 1)
  (c-set-offset 'case-label '+)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my/c++-indentation)
