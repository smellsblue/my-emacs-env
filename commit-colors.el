;;; commit-colors.el: Thanks to Sriram Thaiyar
;;;

;; Add colors if invoked with "git diff -v".
(defface commit-colors-line-added
  '((t (:foreground "green")))
  "")

(defface commit-colors-line-removed
  '((t (:foreground "red")))
  "")

(defvar commit-colors-line-added 'commit-colors-line-added)
(defvar commit-colors-line-removed 'commit-colors-line-removed)

(when (featurep 'magit)
  (setq commit-colors-line-added 'magit-diff-del)
  (setq commit-colors-line-removed 'magit-diff-add))

(defvar commit-colors-keywords
  '(("^[*] [^:]+:" . font-lock-function-name-face)
    ("^\\+.*$" . commit-colors-line-added)
    ("^-.*$" . commit-colors-line-removed)))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . commit-colors-mode))

(defun commit-colors-mode-turn-on ()
  (commit-colors-mode 1))

(define-minor-mode commit-colors-mode " CMT" nil
  :lighter " CMT"
  (font-lock-add-keywords nil commit-colors-keywords))

;; Magit hooks
(add-hook 'magit-log-edit-mode-hook 'commit-colors-mode-turn-on)

(provide 'commit-colors)
