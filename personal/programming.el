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
