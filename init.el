;; Modularization

(package-initialize)

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

;; Globals must be loaded before anything else; this include package system initialization
(load-user-module "global")

;; OS-specific configuration
(when (string= system-type "darwin")
  (load-user-module "osx"))
(when (string= operating-system-release "4.19.4-02480-gd44d301822f0") ; TODO: there should be a better way
  (load-user-module "pixelbook"))

(load-user-module "navigation")
(load-user-module "utility")
(load-user-module "org")
(load-user-module "programming")
(load-user-module "git")
(load-user-module "elisp")
(load-user-module "python")
(load-user-module "haskell")
(load-user-module "cpp")
(load-user-module "web")
(load-user-module "dotnet")
(load-user-module "rust")
(load-user-module "salt")
(load-user-module "shell")

;; Load decorations after everything else is finished
(load-user-module "decoration")

;; Move customizations to their own file
(setq custom-file (expand-file-name "custom.el" user-base-dir))
(load custom-file)
