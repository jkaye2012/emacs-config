
(use-package all-the-icons
  :config
  (when (and window-system (not (find-font (font-spec :name "all-the-icons"))))
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :after (all-the-icons)
  :config
  (load-theme 'doom-acario-light t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode))

(use-package sublime-themes
  :after (all-the-icons))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(cond ((find-font (font-spec :name "Source Code Pro"))
       (set-frame-font "Source Code Pro 11" nil t))
      ((find-font (font-spec :name "Fira Code"))
       (set-frame-font "Fira Code 11" nil t))
      (t (message "No fonts found; consider installing one?")))
