
(use-package cmake-mode)

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :after irony
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :after irony
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :after irony
  :hook (irony-mode . irony-eldoc))
