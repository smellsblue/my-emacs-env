(load (concat (file-name-directory load-file-name) "emacs_utils/load-all.el"))
(load (concat (file-name-directory load-file-name) "emacs-bash-completion/bash-completion.el"))
(load (concat (file-name-directory load-file-name) "keyboard-mappings.el")
(setq inhibit-splash-screen t)
(setq rspec-shell-name "*shell*")
(set-default-keyboard-mapping "mvs")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)
