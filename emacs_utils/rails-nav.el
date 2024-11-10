(defmacro def-jump-to-rails (type retrieve-files)
  "Define a jump-to-rails- function to jump to a particular rails file"
  `(def-jump-to-file
     ,(concat "jump-to-rails-" type)
     (concat "Get the correct rails " ,type " for the current file")
     (get-rails-root)
     (get-rails-default-item ,type (get-rails-type) (get-rails-item) (get-rails-action))
     (concat "Rails " ,type " to load")
     ,retrieve-files
     (lambda (x) (get-rails-full-item-path ,type x))))

(def-jump-to-rails "controller"
  (mapcar
   (lambda (x) (chomp-ends-with x "_controller.rb"))
   (rails-directory-files "app/controllers" ".*_controller\\.rb$")))
(def-jump-to-rails "helper"
  (mapcar
   (lambda (x) (chomp-ends-with x "_helper.rb"))
   (rails-directory-files "app/helpers" ".*_helper\\.rb$")))
(def-jump-to-rails "model"
  (mapcar
   (lambda (x) (chomp-ends-with x ".rb"))
   (rails-directory-files "app/models" ".*\\.rb$")))
(def-jump-to-rails "spec"
  (mapcar
   (lambda (x) (chomp-ends-with x "_spec.rb"))
   (rails-directory-files "spec" ".*_spec\\.rb$")))
(def-jump-to-rails "test"
  (mapcar
   (lambda (x) (chomp-ends-with x "_test.rb"))
   (rails-directory-files "test" ".*_test\\.rb$")))
(def-jump-to-rails "view"
  (mapcar
   (lambda (x) (replace-regexp-in-string "^\\(.+\\)/\\(.+?\\)\\.html\\.erb$" "\\1#\\2" x))
   (rails-directory-files "app/views" ".+/.+?\\.html\\.erb$")))
(def-jump-to-rails "config"
  (mapcar
   (lambda (x) (chomp-ends-with x ""))
   (rails-directory-files "config" ".*\\.*$")))

(defun rails-directory-files (path regex)
  "Like a recursive version of directory-files, but for rails directories"
  (recursive-directory-files 'get-rails-path path regex))

(defun get-rails-default-item (for-type maybe-type item action)
  "Get the default item for the given type, given the current item/type/action"
  (let* ((type (if maybe-type maybe-type "model"))
         (non-test-type (cond ((equal type "test") "model")
                              ((equal type "spec") "model")
                              (t type)))
         (test-suffix (cond ((equal type "controller") "_controller")
                            ((equal type "helper") "_helper"))))
    (cond ((equal for-type "controller") item)
          ((equal for-type "helper") item)
          ((equal for-type "model") item)
          ((equal for-type "spec") (concat (toggle-plural non-test-type) "/" item test-suffix))
          ((equal for-type "test") (concat (toggle-plural non-test-type) "/" item test-suffix))
          ((equal for-type "view") (concat item action)))))

(defun get-rails-type (&optional file-name-or-current)
  "Get the rails item type from the given file name or current buffer file name"
  (let ((file-name (file-truename (or file-name-or-current (buffer-file-name) "."))))
    (cond ((is-rails-controller file-name) "controller")
          ((is-rails-helper file-name) "helper")
          ((is-rails-model file-name) "model")
          ((is-rails-spec file-name) "spec")
          ((is-rails-test file-name) "test")
          ((is-rails-view file-name) "view")
          ((is-rails-config file-name) "config")
          (t nil))))

(defun get-rails-item (&optional file-name-or-current)
  "Get the rails item from the given file name or current buffer file name"
  (let ((file-name (file-truename (or file-name-or-current (buffer-file-name) ".")))
        (using-current-buffer (not file-name-or-current))
        (file-type (get-rails-type file-name-or-current)))
    (cond ((equal file-type "controller") (string-inside file-name (get-rails-path "app/controllers/") "_controller.rb"))
          ((equal file-type "helper") (string-inside file-name (get-rails-path "app/helpers/") "_helper.rb"))
          ((equal file-type "model") (string-inside file-name (get-rails-path "app/models/") ".rb"))
          ((equal file-type "spec") (get-rails-test-item file-name (get-rails-path "spec/") "_spec.rb"))
          ((equal file-type "test") (get-rails-test-item file-name (get-rails-path "test/") "_test.rb"))
          ((equal file-type "view") (string-inside file-name (get-rails-path "app/views/") (concat "/" (file-name-nondirectory file-name))))
          (t "application"))))

(defun get-rails-test-item (file-name before after)
  "Get the item for a test type by checking for controller/helper/model/view tests"
  (or (string-inside file-name (concat before "controllers/") (concat "_controller" after))
      (string-inside file-name (concat before "helpers/") (concat "_helper" after))
      (string-inside file-name (concat before "models/") after)
      (string-inside file-name (concat before "views/") after)
      "application"))

(defun get-rails-action (&optional file-name-or-current)
  "Get the rails action from the given file name or current buffer file name, return with leading #"
  (let ((file-name (file-truename (or file-name-or-current (buffer-file-name) ".")))
        (using-current-buffer (not file-name-or-current))
        (file-type (get-rails-type file-name-or-current)))
    (cond ((equal file-type "controller") (or (get-rails-controller-action using-current-buffer) "#index"))
          ((equal file-type "helper") "#index")
          ((equal file-type "model") "#index")
          ((equal file-type "spec") "#index")
          ((equal file-type "test") "#index")
          ((equal file-type "view") (get-rails-view-action file-name))
          (t "#index"))))

(defun get-rails-controller-action (detect)
  "Try to detect an action from the current buffer, return with leading #, but only if detect is true"
  (if detect
      (let ((result (save-excursion (if (re-search-backward "def\\s-+\\([a-zA-Z_]+\\)" nil t) (match-string 1)))))
        (if result (concat "#" result)))))

(defun get-rails-view-action (file-name)
  "Get the rails action from the given view file (including a leading #)"
  (concat "#" (nth 0 (split-string (file-name-nondirectory file-name) "\\."))))

(defun is-rails-controller (file-name)
  "Determine if the given file-name is a rails controller"
  (has-parent-directory file-name (get-rails-path "app/controllers")))

(defun is-rails-helper (file-name)
  "Determine if the given file-name is a rails helper"
  (has-parent-directory file-name (get-rails-path "app/helpers")))

(defun is-rails-model (file-name)
  "Determine if the given file-name is a rails model"
  (has-parent-directory file-name (get-rails-path "app/models")))

(defun is-rails-spec (file-name)
  "Determine if the given file-name is a rails spec"
  (has-parent-directory file-name (get-rails-path "spec")))

(defun is-rails-test (file-name)
  "Determine if the given file-name is a rails test"
  (has-parent-directory file-name (get-rails-path "test")))

(defun is-rails-view (file-name)
  "Determine if the given file-name is a rails view"
  (has-parent-directory file-name (get-rails-path "app/views")))

(defun is-rails-config (file-name)
  "Determine if the given file-name is a rails config"
  (has-parent-directory file-name (get-rails-path "config")))

(defun get-rails-full-item-path (type full-item)
  "Get an item path relative to the current rails root"
  (let* ((item (nth 0 (split-rails-item full-item)))
         (maybe-action (nth 1 (split-rails-item full-item)))
         (action (or maybe-action "index"))
         (non-toggled-path (get-rails-item-path type item action))
         (toggled-path (get-rails-item-path type (toggle-plural item) action)))
    (if (and (not (file-exists-p non-toggled-path)) (file-exists-p toggled-path))
        toggled-path
      non-toggled-path)))

(defun get-rails-item-path (type item action)
  "Get a rails item path from the item and action"
  (cond ((equal type "controller") (get-rails-path (concat "app/controllers/" item "_controller.rb")))
        ((equal type "helper") (get-rails-path (concat "app/helpers/" item "_helper.rb")))
        ((equal type "model") (get-rails-path (concat "app/models/" item ".rb")))
        ((equal type "spec") (get-rails-path (concat "spec/" item "_spec.rb")))
        ((equal type "test") (get-rails-path (concat "test/" item "_test.rb")))
        ((equal type "view") (get-rails-path (concat "app/views/" item "/" action ".html.erb")))
        ((equal type "config") (get-rails-path (concat "config/" item)))))

(defun toggle-plural (item)
  "Toggle the plural state of the given item"
  (let ((chomped (chomp-ends-with item "s")))
    (if chomped
        chomped
      (concat item "s"))))

(defun split-rails-item (full-item)
  "Split a rails item like 'account#index' into '('acount' 'index')"
  (split-string full-item "#"))

(defun is-rails-project (&optional path-or-current)
  "Determine if the current or given path is a rails project"
  (let* ((path (or path-or-current "."))
         (from-path (locate-rails-root path)))
    (if from-path from-path)))

(defun get-rails-path (path)
  "Get a path relative to the current rails root"
  (concat (get-rails-root) path))

(defun get-rails-root (&optional path-or-current)
  "Find rails root from the current or given directory, or nil if it is not detected, and fall back to default-rails-root global variable as possible Rails location"
  (let* ((path (or path-or-current "."))
         (from-path (locate-rails-root path)))
    (if from-path
        from-path
      (if (boundp 'default-rails-root) (locate-rails-root default-rails-root)))))

(defun locate-rails-root (path)
  "Find rails root relative to the given directory, or nil if it is not detected"
  (if path
      (let ((path-dir (file-name-as-directory path)))
        (cond ((is-rails-root path-dir) (file-truename path-dir))
              ((equal (file-truename path-dir) "/") nil)
              (t (locate-rails-root (concat path-dir "..")))))))

(defun is-rails-root (path)
  "Determine if a given path is a rails root"
  (let ((path-dir (file-name-as-directory path)))
    (or (file-exists-p (concat path-dir "script/rails"))
        (and (file-exists-p (concat path-dir "script/server"))
             (file-directory-p (concat path-dir "app")))
        (and (file-exists-p (concat path-dir "config/application.rb"))
             (file-directory-p (concat path-dir "app"))))))
