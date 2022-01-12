
(defconst user-base-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
        ((boundp 'user-init-directory) user-init-directory)
        (t (file-name-as-directory "~/.emacs.d/"))))

(defconst user-init-dir
  (concat user-base-dir "personal"))

(defconst user-themes-dir
  (concat user-base-dir "themes"))

(defun load-user-module (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (message "Loading personal module: %s" file)
  (load-file (expand-file-name (concat file ".el") user-init-dir)))

(defun load-themes ()
  (interactive)
  "Load all themes provided with local sources"
  (mapc #'(lambda (f) (load-file f))
        (directory-files user-themes-dir t ".el$")))
