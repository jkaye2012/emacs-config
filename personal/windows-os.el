
(defun git-bash () (interactive)
  (let ((explicit-shell-file-name "C:/Program Files/git/bin/bash"))
    (call-interactively 'shell)))

(general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "`" '(git-bash :wk "Git bash")
   )
