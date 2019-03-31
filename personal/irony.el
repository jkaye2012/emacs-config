
(use-package irony
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))

  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix ","
   :non-normal-prefix "M-,"
    "h" '(ff-find-other-file :wk "switch header/impl")
    ))

(use-package company-irony
  :after irony
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :after irony
  :hook ((flycheck-mode . flycheck-irony-setup)))

(use-package irony-eldoc
  :after irony
  :hook (irony-mode . irony-eldoc))

(load-user-module "rtags")
(load-user-module "cmake")
