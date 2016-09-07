;; From http://stackoverflow.com/a/20015336/122
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; add this hook as buffer local, so it runs once per window.
            (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)))
