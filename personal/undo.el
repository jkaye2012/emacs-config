
(use-package undo-tree
  :hook (prog-mode . undo-tree-mode)
  :config

  (general-define-key
   :states '(normal)
    "u" '(undo-tree-undo :wk "undo")
    "C-r" '(undo-tree-redo :wk "redo"))

  (general-define-key
   :states '(normal)
   :prefix "SPC"
    "u" '(nil :wk "Undo")
    "ur" '(undo-tree-restore-state-from-register :wk "restore")
    "us" '(undo-tree-save-state-to-register :wk "save")
    "uv" '(undo-tree-visualize :wk "visualize"))

  (general-define-key
   :states '(normal)
   :keymaps '(undo-tree-visualizer-mode-map)
    "d" '(undo-tree-visualizer-toggle-diff :wk "diffs")
    "h" '(undo-tree-visualize-switch-branch-left :wk "left branch")
    "l" '(undo-tree-visualize-switch-branch-right :wk "right branch")
    "t" '(undo-tree-visualizer-toggle-timestamps :wk "timestamps")
    "n" '(undo-tree-visualize-redo-to-x :wk "next branch")
    "u" '(undo-tree-visualize-undo-to-x :wk "last branch")))
