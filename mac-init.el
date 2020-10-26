(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
(defcustom mvs-mac-font "Menlo-18"
  "The default global Mac font"
  :type 'string
  :group 'mvs)
(defcustom mvs-mac-initialize-window-size t
  "Whether or not to initialize window size on a mac"
  :type 'boolean
  :group 'mvs)
(setq mvs-font mvs-mac-font)

;;; Nice size for the default window
(if mvs-mac-initialize-window-size
    (progn
      (defun get-default-height ()
        (/ (- (display-pixel-height) 120)
           (frame-char-height)))

      (add-to-list 'default-frame-alist '(width . 159))
      (add-to-list 'default-frame-alist (cons 'height (get-default-height)))))
