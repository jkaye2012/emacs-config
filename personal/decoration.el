
(use-package all-the-icons
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :after (all-the-icons)
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-org-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode))

(if (find-font (font-spec :name "Source Code Pro"))
    (set-frame-font "Source Code Pro 11" nil t)
  (message "Source Code Pro not found; consider installing it?"))
