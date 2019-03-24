
(use-package salt-mode
  :config
  (add-hook 'salt-mode-hook
            (lambda ()
              (flyspell-mode 1))))
