
(use-package evil-org
  :ensure t
  :after org
  :config
  (setq org-todo-keywords '((sequence "TODO(t!)" "IN PROGRESS(i!)" "DONE(d@)")))
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq org-agenda-skip-scheduled-if-done t)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps '(org-mode-map)
   "<tab>" '(org-cycle)
  )

  (general-define-key
   :states '(normal)
   :keymaps '(org-mode-map)
    "t" 'org-shiftright
   )

  (general-define-key
   :states '(normal)
   :keymaps '(org-mode-map)
   :prefix ","
    "a" '(org-agenda :wk "agenda")
    "c" '(org-archive :wk "archive subtree")
    "g" '(org-edit-special :wk "edit linked file")
    "i" '(org-insert-heading-after-current :wk "insert heading")
    "n" '(org-next-visible-heading :wk "next heading")
    "p" '(org-previous-visible-heading :wk "previous heading")
    "s" '(org-sort :wk "sort")
    "t" '(nil :wk "Todo")
    "td" '(org-deadline :wk "deadline")
    "tn" '(org-add-note :wk "note")
    "ts" '(org-schedule :wk "schedule")
    "tt" '(org-shiftright :wk "cycle")
    "tT" '(org-shiftleft :wk "cycle backwards")
    "tg" '(org-todo :wk "goto state"))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))