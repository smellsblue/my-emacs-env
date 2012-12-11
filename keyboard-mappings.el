(define-keyboard-mappings "mvs"
  '(("Buffers"
     ("C-b" switch-to-buffer)
     ("C-w" kill-this-buffer))

    ("Windows"
     ("C-<tab>" other-window)
     ("<escape> <tab>" other-window)
     ("M-<tab>" other-window)
     ("M-TAB" other-window)
     ("<backtab>" i-previous-window)
     ("M-1" delete-other-windows)
     ("<escape> 1" delete-other-windows)
     ("M-2" split-window-vertically)
     ("<escape> 2" split-window-vertically)
     ("M-3" split-window-horizontally)
     ("<escape> 3" split-window-horizontally)
     ("M-0" delete-window)
     ("<escape> 0" delete-window))

    ("Copy/Paste"
     ("C-c" kill-ring-save)
     ("C-x" kill-region)
     ("C-v" yank)
     ("M-v" yank-pop)
     ("C-z" undo))

    ("Find"
     ("C-f" isearch-forward)
     ("M-f" isearch-backward))

    ("Rectangle"
     ("C-r r" string-rectangle)
     ("C-r x" kill-rectangle)
     ("C-r v" yank-rectangle))

    ("Save/Load"
     ("C-o" find-file)
     ("C-s" save-buffer)
     ("M-s" write-file))

    ("Jump"
     ("C-j c" jump-to-rails-controller)
     ("C-j h" jump-to-rails-helper)
     ("C-j m" jump-to-rails-model)
     ("C-j s" jump-to-rails-spec)
     ("C-j t" jump-to-rails-test)
     ("C-j v" jump-to-rails-view))

    ("Lisp Eval"
     ("M-e" eval-last-sexp))

    ("Exit"
     ("C-q" save-buffers-kill-terminal))

    ("Suspend"
     ("M-z" suspend-frame))

    ("Movement"
     ("C-<down>" scroll-up)
     ("C-<up>" scroll-down))

    ("Interrupt"
     ("s-c" comint-interrupt-subjob)
     ("<f9> c" comint-interrupt-subjob)))

  ;; On load
  (lambda ()
    (defun i-previous-window ()
      "Interactive version of previous-window"
      (interactive)
      (select-window (previous-window (selected-window))))
    (setq old-isearch-mode-map-hashmap (make-hash-table :test 'equal))
    (puthash "C-f" (lookup-key isearch-mode-map (kbd "C-f")) old-isearch-mode-map-hashmap)
    (puthash "M-f" (lookup-key isearch-mode-map (kbd "M-f")) old-isearch-mode-map-hashmap)
    (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-backward))

  ;; On unload
  (lambda ()
    (define-key isearch-mode-map (kbd "C-f") (gethash "C-f" old-isearch-mode-map-hashmap))
    (define-key isearch-mode-map (kbd "M-f") (gethash "M-f" old-isearch-mode-map-hashmap))
    (define-key keyboard-mappings-keymap (kbd "C-r") nil)
    (define-key keyboard-mappings-keymap (kbd "C-j") nil)))
