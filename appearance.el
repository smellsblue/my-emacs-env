(when (display-graphic-p)
  (when (locate-library "color-theme")
    (load (concat (file-name-directory load-file-name) "color-theme-molokai/color-theme-molokai.el"))
    (color-theme-molokai)))
