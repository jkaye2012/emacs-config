

(use-package nerd-icons
  :config
 (when (and window-system (not (find-font (font-spec :name "Symbols Nerd Font Mono"))))
  (nerd-icons-install-fonts)))

(use-package doom-themes
  :after (nerd-icons)
  :config
  (load-theme 'doom-nord t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode))

(use-package sublime-themes
  :after (nerd-icons))

;; (use-package emojify
;;   :hook (after-init . global-emojify-mode)
;;   :config
;;   (setq emojify-emoji-styles '(unicode github)))

;; (use-package fira-code-mode
;;   :config
;;   (when (not (find-font (font-spec :name "Fira Code Symbol")))
;;     (fira-code-mode-install-fonts t))
;;   (setq fira-code-mode-disabled-ligatures '("x" "-}"))
;;   (global-fira-code-mode)
;;   (fira-code-mode-set-font))

(defun my/set-font ()
  (interactive)
  (cond
   ((find-font (font-spec :name "Fira Code"))
    (set-frame-font "Fira Code 11" nil t))
   ((find-font (font-spec :name "Source Code Pro"))
    (set-frame-font "Source Code Pro 11" nil t))
   (t (message "No fonts found; consider installing one?"))))

(add-hook 'window-setup-hook 'my/set-font)
