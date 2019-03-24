
(defconst user-base-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
        ((boundp 'user-init-directory) user-init-directory)
        (t (file-name-as-directory "~/.emacs.d/"))))

(defconst user-init-dir
  (concat user-base-dir "personal"))

(defun load-user-module (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (message "Loading personal module: %s" file)
  (load-file (expand-file-name (concat file ".el") user-init-dir)))
