(when (display-graphic-p)
  (add-to-list 'load-path (concat (file-name-directory load-file-name) "color-theme-6.6.0"))
  (require 'color-theme)
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (load (concat (file-name-directory load-file-name) "color-theme-molokai/color-theme-molokai.el"))
       (color-theme-molokai))))
