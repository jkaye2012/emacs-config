;; Ubiquitous programming tools
(use-package iedit)

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package rainbow-delimiters
  :config
  (add-hook 'smartparens-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :config
  (add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode-hook #'(lambda () (setq flycheck-checker 'javascript-jshint)))
  (add-hook 'js2-mode-hook 'flycheck-mode)

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "e" '(nil :wk "Errors")
   "el" '(flycheck-list-errors :wk "list")
   "ep" '(flycheck-previous-error :wk "previous")
   "en" '(flycheck-next-error :wk "next")))
