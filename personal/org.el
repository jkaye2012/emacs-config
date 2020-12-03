
(use-package evil-org
  :ensure t
  :after org
  :config
  (setq org-todo-keywords '((sequence "TODO(t!)" "IN PROGRESS(i!)" "DONE(d@)")))
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-files '("~/org"))
  (setq ispell-silently-savep t)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)
              (company-mode -1)))
  (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(org-mode-map)
   "<tab>" '(org-cycle))

  (general-define-key
   :states '(normal)
   :keymaps '(org-mode-map)
    "t" 'org-shiftright)

  (general-define-key
   :states '(insert)
   :keymaps '(org-mode-map)
   "C-i" 'org-insert-item)

  (general-define-key
   :states '(normal)
   :keymaps '(org-mode-map)
   :prefix ","
    "a" '(org-agenda :wk "agenda")
    "c" '(org-archive-subtree :wk "archive subtree")
    "g" '(org-edit-special :wk "edit linked file")
    "i" '(org-insert-subheading :wk "insert subheading")
    "i" '(org-insert-heading-after-current :wk "insert heading")
    "n" '(org-next-visible-heading :wk "next heading")
    "o" '(org-open-at-point :wk "open")
    "p" '(org-previous-visible-heading :wk "previous heading")
    "s" '(org-sort :wk "sort")
    "t" '(nil :wk "Todo")
    "td" '(org-deadline :wk "deadline")
    "tn" '(org-add-note :wk "note")
    "ts" '(org-schedule :wk "schedule")
    "tt" '(org-shiftright :wk "cycle")
    "tT" '(org-shiftleft :wk "cycle backwards")
    "tg" '(org-todo :wk "goto state")
    "w" '(ispell-word :wk "Fix word"))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defun my/org-roam-find-file ()
  (interactive)
  (org-roam-find-file nil nil #'(lambda (completions)
                                  (seq-filter #'(lambda (comp)
                                                  (not (string-match "archived" (car comp))))
                                              completions))))

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org")

  :config
  (general-define-key
   :states '(normal)
   :keymaps '(org-mode-map)
   :prefix ","
   "r" '(nil :wk "Roam")
   "rf" '(my/org-roam-find-file :wk "find file")
   "rg" '(org-roam-graph :wk "graph")
   "ri" '(org-roam-insert :wk "insert")
   "rr" '(org-roam :wk "roam"))
  )
