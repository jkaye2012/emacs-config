
(use-package all-the-icons
  :config
  ; (all-the-icons-install-fonts t)
  )

(use-package doom-themes
  :after (all-the-icons)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode))
