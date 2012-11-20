(define-keyboard-mappings "mvs"
  '(;; Buffers
    ("s-`" switch-to-buffer)
    ("C-w" kill-this-buffer)

    ;; Windows
    ("C-<tab>" other-window)
    ("C-S-<iso-lefttab>" i-previous-window)
    ("s-1" delete-other-windows)
    ("s-2" split-window-vertically)
    ("s-3" split-window-horizontally)
    ("s-0" delete-window)

    ;; Copy/Paste
    ("C-c" kill-ring-save)
    ("C-x" kill-region)
    ("C-v" yank)
    ("C-z" undo)

    ;; Find
    ("C-f" isearch-forward)
    ("M-f" isearch-backward)

    ;; Rectangle
    ("s-r" string-rectangle)
    ("s-x" kill-rectangle)
    ("s-v " yank-rectangle)

    ;; Save/Load
    ("C-o" find-file)
    ("C-s" save-buffer)
    ("M-s" write-file)

    ;; Jump
    ("s-j c" jump-to-rails-controller)
    ("s-j h" jump-to-rails-helper)
    ("s-j m" jump-to-rails-model)
    ("s-j s" jump-to-rails-spec)
    ("s-j t" jump-to-rails-test)
    ("s-j v" jump-to-rails-view)

    ;; Lisp Eval
    ("s-e" eval-last-sexp)

    ;; Exit
    ("s-q" save-buffers-kill-terminal)

    ;; Suspend
    ("s-z" suspend-frame))

  ;; On load
  (lambda ()
    (defun i-previous-window ()
      "Interactive version of previous-window"
      (interactive)
      (select-window (previous-window (selected-window)))))

  ;; On unload
  nil)
