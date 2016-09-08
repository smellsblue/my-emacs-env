(setq yas-snippet-dirs (concat (file-name-directory load-file-name) "snippets/"))
(yas-global-mode 1)
(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)

(defmacro def-bind-yasnippet (binding template)
  "Define a function to insert a particular yasnippet"
  `(progn
     (defun ,(intern (concat "expand-yasnippet-" template)) ()
       (concat "Expand the " ,template " yasnippet")
       (interactive)
       ;; Thanks to: http://emacs.stackexchange.com/a/13901
       (yas-expand-snippet (yas-lookup-snippet ,template)))
     (global-unset-key ,binding)
     (global-set-key ,binding (intern (concat "expand-yasnippet-" ,template)))))

(def-bind-yasnippet "\C-xyi" "if")
(def-bind-yasnippet "\C-xyc" "class")
(def-bind-yasnippet "\C-xyC" "Class")
(def-bind-yasnippet "\C-xym" "method")
(def-bind-yasnippet "\C-xyM" "Method")
