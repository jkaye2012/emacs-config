
(defun protect-eshell-prompt ()
  "Protect Eshell's prompt like Comint's prompts.
E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
  (let ((inhibit-field-text-motion t))
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
		      inhibit-line-move-field-capture t
		      field output
                      read-only t
                      front-sticky (field inhibit-line-move-field-capture)))))

(use-package eshell-prompt-extras
  :config
  (setq eshell-highlight-prompt nil
	eshell-prompt-function 'epe-theme-lambda)

  (defun my/setup-eshell-keys ()
    (local-set-key (kbd "C-j") 'eshell-next-input)
    (local-set-key (kbd "C-k") 'eshell-previous-input))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (add-hook 'eshell-mode-hook 'my/setup-eshell-keys)
  (add-hook 'eshell-after-prompt-hook 'protect-eshell-prompt))

(use-package multi-term
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "t" '(nil :wk "Terminal")
   "tn" '(multi-term-next :wk "next")
   "tp" '(multi-term-prev :wk "next")
   "tt" '(multi-term :wk "new")
   ))
