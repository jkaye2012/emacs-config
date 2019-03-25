
(use-package nim-mode
  :hook ((nim-mode . nimsuggest-mode))
  :config
  (add-hook 'nimsuggest-mode-hook 'company-mode)
  (add-hook 'nimsuggest-mode-hook 'flycheck-mode))
