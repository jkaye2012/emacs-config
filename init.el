;;; Modularization

(package-initialize)
(load "~/.emacs.d/modularization.el")

;; Globals must be loaded before anything else; this include package system initialization
(load-user-module "global")

;;; OS-specific configuration

(when (string= system-type "darwin")
  (load-user-module "osx"))
(when (string= system-name "penguin") ; TODO: there should be a better way
  (load-user-module "pixelbook"))

;;; Individual user modules

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

;; Load decorations and customizations after everything else is finished
(load-user-module "decoration")
(load custom-file)
