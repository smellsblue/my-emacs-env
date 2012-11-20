(define-keyboard-mappings "mvs"
  '(("Buffers"
     ("s-`" switch-to-buffer)
     ("<f9> `" switch-to-buffer)
     ("C-w" kill-this-buffer))

    ("Windows"
     ("C-<tab>" other-window)
     ("<f9> TAB" other-window)
     ("<backtab>" i-previous-window)
     ("s-1" delete-other-windows)
     ("<f9> 1" delete-other-windows)
     ("s-2" split-window-vertically)
     ("<f9> 2" split-window-vertically)
     ("s-3" split-window-horizontally)
     ("<f9> 3" split-window-horizontally)
     ("s-0" delete-window)
     ("<f9> 0" delete-window))

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
     ("s-r" string-rectangle)
     ("<f9> r" string-rectangle)
     ("s-x" kill-rectangle)
     ("<f9> x" kill-rectangle)
     ("s-v " yank-rectangle)
     ("<f9> v " yank-rectangle))

    ("Save/Load"
     ("C-o" find-file)
     ("C-s" save-buffer)
     ("M-s" write-file))

    ("Jump"
     ("s-j c" jump-to-rails-controller)
     ("<f9> j c" jump-to-rails-controller)
     ("s-j h" jump-to-rails-helper)
     ("<f9> j h" jump-to-rails-helper)
     ("s-j m" jump-to-rails-model)
     ("<f9> j m" jump-to-rails-model)
     ("s-j s" jump-to-rails-spec)
     ("<f9> j s" jump-to-rails-spec)
     ("s-j t" jump-to-rails-test)
     ("<f9> j t" jump-to-rails-test)
     ("s-j v" jump-to-rails-view)
     ("<f9> j v" jump-to-rails-view))


    ("Lisp Eval"
     ("s-e" eval-last-sexp)
     ("<f9> e" eval-last-sexp))

    ("Exit"
     ("s-q" save-buffers-kill-terminal)
     ("<f9> q" save-buffers-kill-terminal))

    ("Suspend"
     ("s-z" suspend-frame)
     ("<f9> z" suspend-frame))

    ("Interrupt"
     ("s-c" comint-interrupt-subjob)
     ("<f9> c" comint-interrupt-subjob)))

  ;; On load
  (lambda ()
    (defun i-previous-window ()
      "Interactive version of previous-window"
      (interactive)
      (select-window (previous-window (selected-window)))))

  ;; On unload
  nil)
