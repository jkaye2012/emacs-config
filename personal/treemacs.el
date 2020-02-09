
(use-package treemacs
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)

  (general-define-key
   :states '(normal)
   :prefix "SPC"
    "T" '(nil :wk "Directory tree")
    "Tt" '(treemacs-select-window :wk "focus")
    "Tx" '(treemacs :wk "toggle")))

(use-package treemacs-evil
  :requires treemacs
  :config
  (define-key treemacs-mode-map (kbd ",") #'other-window))

(use-package treemacs-projectile
  :requires treemacs)

(use-package treemacs-magit
  :requires (treemacs magit))

(use-package treemacs-icons-dired
  :requires (treemacs)
  :config (treemacs-icons-dired-mode))
