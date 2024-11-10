(defmacro def-jump-to-project-file (type)
  "Define an interactive function to jump to a particular type for the current project, after determining the project type"
  `(progn
     (defun ,(intern (concat "jump-to-project-" type)) ()
       ,(concat "Open a " type " file for the current project")
       (interactive)
       (cond ((is-rails-project) (jump-to-project-file "rails" ,type))
             ((is-java-project) (jump-to-project-file "java" ,type))
             (t (message "Cannot determine project type!"))))))

(defmacro bind-and-def-jump-to-project-file (binding type)
  "Define and bind an interactive function to jump to a particular type for the current project, after determining the project type"
  `(progn
     (def-jump-to-project-file ,type)
     (keymap-global-set ,binding (intern (concat "jump-to-project-" ,type)))))

(defmacro def-jump-to-file (fn-name help root-dir current-item retrieve-files-message retrieve-files load-file)
  "Define a function to jump to a particular file based on a root directory function, a way to retrieve current context, a way to load known files, and how to load that specific file"
  `(progn
     (defun ,(intern fn-name) ()
       ,help
       (interactive)
       (if ,root-dir
           (let* ((default-item ,current-item)
                  (item (completing-read (concat ,retrieve-files-message " (default " default-item "): ") ,retrieve-files nil 'confirm)))
             (find-file (funcall ,load-file (if (equal item "") default-item item))))
         (message "Cannot find project root!")))))

(bind-and-def-jump-to-project-file "C-x j c" "controller")
(bind-and-def-jump-to-project-file "C-x j h" "helper")
(bind-and-def-jump-to-project-file "C-x j m" "model")
(bind-and-def-jump-to-project-file "C-x j s" "spec")
(bind-and-def-jump-to-project-file "C-x j t" "test")
(bind-and-def-jump-to-project-file "C-x j v" "view")
(bind-and-def-jump-to-project-file "C-x j g" "config")

(defun jump-to-project-file (project-type file-type)
  "Invoke the interactive jump-to-rails-TYPE function, if it exists"
  (interactive)
  (let* ((fn-name (intern (concat "jump-to-" project-type "-" file-type))))
    (if (fboundp fn-name)
        (funcall fn-name)
      (message (concat "Projects for " project-type " do not support files of type " file-type)))))

(defun recursive-directory-files (root-fn path regex &optional max-depth)
  "Like a recursive version of directory-files, but use a function to determine the base path"
  (compact (mapcar
            (lambda (x)
              (if (and x (string-match regex x)) x))
            (flatten (recursive-directory-expand-files "" (funcall root-fn path) max-depth)))))

(defun recursive-directory-expand-files (prefix path max-depth)
  "Recursive helper function for recursive-directory-files"
  (let ((path-dir (file-name-as-directory path)))
    (mapcar (lambda (x)
              (if (or (equal x ".") (equal x ".."))
                  nil
                (let ((path-x (concat path-dir x)))
                  (if (file-directory-p path-x)
                      (if (or (not max-depth) (> max-depth 0))
                          (recursive-directory-expand-files (file-name-as-directory (concat prefix x)) path-x (if max-depth (- max-depth 1))))
                    (concat prefix x)))))
            (if (file-exists-p path-dir)
                (directory-files path-dir nil nil t)))))

(defun has-parent-directory (testing against)
  "Determine if the first argument has the parent directory being the against argument"
  (let ((testing-dir (file-name-as-directory (file-truename testing)))
        (against-dir (file-name-as-directory (file-truename against))))
    (cond ((equal testing-dir against-dir) t)
          ((equal testing-dir "/") nil)
          (t (has-parent-directory (concat testing-dir "..") against)))))

;; Adapted from http://stackoverflow.com/questions/969067/name-of-this-function-in-built-in-emacs-lisp-library
(defun flatten (x)
  "Flatten a list, so all sub-list items become simple items in the array, so ((1 2) 3 (4 5)) becomes (1 2 3 4 5)"
  (cond ((null x) nil)
        ((listp x) (let (value)
                     (dolist (elt x value)
                       (setq value (append value (flatten elt))))))
        (t (list x))))

;; From http://stackoverflow.com/questions/3967320/lisp-function-to-remove-nils
(defun compact (x)
  "Compact a list, so all nils are removed, so (nil 1 2 3 nil 4) becomes (1 2 3 4)"
  (if (listp x)
      (mapcar #'compact
              (remove nil x))
    x))

(defun string-inside (string before after)
  "Retrieve the string iside the before and after strings"
  (let ((latter (chomp-starts-with string before)))
    (if latter (chomp-ends-with latter after))))

(defun chomp-starts-with (string value)
  "Determine if the string starts with the value and chomp it if so, else nil"
  (cond ((and (>= (length string) (length value))
              (string-equal (substring string 0 (length value)) value))
         (substring string (length value)))
        (t nil)))

(defun chomp-ends-with (string value)
  "Determine if the string ends with the value and chomp it if so, else nil"
  (let ((endlength (- (length string) (length value))))
    (cond ((and (>= (length string) (length value))
                (equal value (substring string endlength)))
           (substring string 0 endlength))
          (t nil))))
