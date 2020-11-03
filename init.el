;; Modularization

(package-initialize)
(load "~/.emacs.d/modularization.el")

;; Globals must be loaded before anything else; this include package system initialization
(load-user-module "global")

;;; OS-specific configuration

(when (string= (system-name) "colwksdev001.belvederetrading.com")
  (load-user-module "centos"))
(when (string= system-type "gnu/linux")
  (load-user-module "centos"))
(when (string= system-type "darwin")
  (load-user-module "osx"))
(when (string= (system-name) "penguin")
  (load-user-module "pixelbook"))
(when (string= system-type  "windows-nt")
  (load-user-module "windows-os"))

;;; Individual user modules

(load-user-module "navigation")
(load-user-module "undo")
(load-user-module "utility")
(load-user-module "org")
(load-user-module "programming")
(load-user-module "git")
(load-user-module "elisp")
(load-user-module "cpp")
(load-user-module "python")
(load-user-module "groovy")
(load-user-module "haskell")
(load-user-module "web")
(load-user-module "dotnet")
(load-user-module "rust")
(load-user-module "nim")
(load-user-module "salt")
(load-user-module "shell")
(load-user-module "typescript")
(load-user-module "markdown")
(load-user-module "craft-mode")
(load-user-module "spotify")
(load-user-module "treemacs")
(load-user-module "midnight")
(load-user-module "zeroc")

;; Load decorations and customizations after everything else is finished
(load-themes)
(load-user-module "decoration")
(load custom-file)
