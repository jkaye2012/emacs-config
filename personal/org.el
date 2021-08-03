(defun org-open-at-point-current-window ()
  (interactive)
  (let ((org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                                (vm-imap . vm-visit-imap-folder-other-frame)
                                (gnus . org-gnus-no-new-news)
                                (file . find-file)
                                (wl . wl-other-frame))))
    (org-open-at-point)))

(defun my/org-open-at-point (&optional arg)
  (interactive "P")
  (if arg
      (org-open-at-point-current-window)
    (org-open-at-point)))

(use-package evil-org
  :ensure t
  :after org
  :config
  (setq org-todo-keywords '((sequence "TODO(t!)" "IN PROGRESS(i!)" "DONE(d@)")))
  (setq org-log-into-drawer t)
  (setq org-log-reschedule 'time)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-files '("~/sync"))
  (setq ispell-silently-savep t)
  (setq org-insert-heading-respect-content t)
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
    "i" '(org-insert-heading-after-current :wk "insert heading")
    "n" '(org-next-visible-heading :wk "next heading")
    "o" '(my/org-open-at-point :wk "open")
    "p" '(org-previous-visible-heading :wk "previous heading")
    "s" '(org-sort :wk "sort")
    "t" '(nil :wk "Todo")
    "td" '(org-deadline :wk "deadline")
    "tn" '(org-add-note :wk "note")
    "ts" '(org-schedule :wk "schedule")
    "tt" '(org-shiftright :wk "cycle")
    "tT" '(org-shiftleft :wk "cycle backwards")
    "tg" '(org-todo :wk "goto state")
    "w" '(ispell-word :wk "Fix word")
    "x" '(org-toggle-checkbox :wk "Toggle checkbox"))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defun my/org-roam-filter-node (node)
  (not (member "archived" (org-roam-node-tags node))))

(defun my/org-roam-find-file ()
  (interactive)
  (org-roam-node-find nil nil #'my/org-roam-filter-node))

(defun my/org-roam-insert ()
  (interactive)
  (org-roam-node-insert #'my/org-roam-filter-node))

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-setup)
  :custom
  (org-roam-directory "~/sync")
  (org-roam-index-file "~/sync/index.org")
  (org-roam-file-exclude-regexp ".debris")

  :config
  (general-define-key
   :states '(normal)
   :prefix "SPC"
   "r" '(nil :wk "Roam")
   "rf" '(my/org-roam-find-file :wk "find file")
   "rg" '(org-roam-graph :wk "graph")
   "ri" '(my/org-roam-insert :wk "insert")
   "rn" '(org-id-get-create :wk "create node")))
