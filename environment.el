(if (boundp 'mvs-after-loads) (funcall mvs-after-loads))
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)
(setq coffee-tab-width 4)
(setq default-tab-width 4)
(setq inhibit-splash-screen t)
(setq rspec-shell-name "*shell*")
(setq tab-width 4)
(setq sass-indent-offset 4)
(setq-default fill-column 80)
(setenv "PAGER" "cat")
(column-number-mode)
(global-linum-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-hook 'shell-mode-hook
          (lambda ()
            (setq tab-width 8)
            (shell-up-down-minor-mode)
            (ansi-color-for-comint-mode-on)))
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)
(if (boundp 'mvs-before-set-keyboard-mapping) (funcall mvs-before-set-keyboard-mapping))
(set-default-keyboard-mapping "mvs")
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-default-font "Droid Sans Mono 12")
(setq backup-directory-alist `(("." . "~/.saves")))
(rvm-use-default)
