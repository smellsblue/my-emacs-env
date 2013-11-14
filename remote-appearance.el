(when (display-graphic-p)
  (when (locate-library "color-theme")
    (remote-load "https://raw.github.com/alloy-d/color-theme-molokai/color-theme-molokai.el")
    (color-theme-molokai)))
