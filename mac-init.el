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
(defcustom mvs-mac-window-width 159
  "When initializing window size on a mac, use this width"
  :type 'integer
  :group 'mvs)
(setq mvs-font mvs-mac-font)

;;; Nice size for the default window
(if mvs-mac-initialize-window-size
    (progn
      (add-to-list 'default-frame-alist '(left . 0))
      (add-to-list 'default-frame-alist (cons 'width mvs-mac-window-width))
      (add-to-list 'default-frame-alist '(fullscreen . fullheight))))

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
