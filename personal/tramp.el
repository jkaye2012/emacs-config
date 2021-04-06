
(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='~/.ssh/tramp.%%r@%%h:%%p' -o ControlPersist=no")
