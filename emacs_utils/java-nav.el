(defmacro def-jump-to-java (type retrieve-files)
  "Define a jump-to-java- function to jump to a particular java file"
  `(def-jump-to-file
     ,(concat "jump-to-java-" type)
     (concat "Get the correct java " ,type " for the current file")
     (get-java-root)
     nil
     (concat "Java " ,type " to load")
     ,retrieve-files
     (lambda (x) (get-java-full-item-path ,type x))))

(def-jump-to-java "model"
  (mapcar
   (lambda (x) (chomp-ends-with x ".java"))
   (java-main-directory-files ".*\\.java$")))
(def-jump-to-java "spec"
  (mapcar
   (lambda (x) (chomp-ends-with x ".java"))
   (java-test-directory-files ".*\\.java$")))
(def-jump-to-java "test"
  (mapcar
   (lambda (x) (chomp-ends-with x ".java"))
   (java-test-directory-files ".*\\.java$")))

(defun java-main-directory-files (regex)
  "Like a recursive version of directory-files, but for java main src directories"
  (recursive-directory-files 'get-java-main-path nil regex))

(defun java-test-directory-files (regex)
  "Like a recursive version of directory-files, but for java test srcdirectories"
  (recursive-directory-files 'get-java-test-path nil regex))

(defun is-java-project (&optional path-or-current)
  "Determine if the current or given path is a java project"
  (get-java-root path-or-current))

(defun get-java-root (&optional path-or-current)
  "Find java root from the current or given directory, or nil if it is not detected"
  (let* ((path (or path-or-current "."))
         (from-path (locate-java-root path)))
    (if from-path from-path)))

(defun get-java-main-root (&optional path-or-current)
  "Get the main src root of the given or current java project, including any empty packages like com/company/project/"
  (let* ((path (get-java-root path-or-current)))
    (if path (traverse-java-empty-packages (file-name-as-directory (concat path "src/main/java"))))))

(defun get-java-test-root (&optional path-or-current)
  "Get the test src root of the given or current java project, including any empty packages like com/company/project/"
  (let* ((path (get-java-root path-or-current)))
    (if path (traverse-java-empty-packages (file-name-as-directory (concat path "src/test/java"))))))

(defun traverse-java-empty-packages (path)
  "recursively traverse the given directory until all single empty directories are traversed, to move past empty packages like com/company/project/"
  (let* ((all-contents (directory-files path t directory-files-no-dot-files-regexp t))
         (first-subdir (car all-contents)))
    (if (and (= (length all-contents) 1)
             (file-directory-p first-subdir))
        (traverse-java-empty-packages (file-name-as-directory first-subdir))
      path)))

(defun locate-java-root (path)
  "Find java root relative to the given directory, or nil if it is not detected"
  (if path
      (let ((path-dir (file-name-as-directory path)))
        (cond ((is-java-root path-dir) (file-truename path-dir))
              ((equal (file-truename path-dir) "/") nil)
              (t (locate-java-root (concat path-dir "..")))))))

(defun is-java-root (path)
  "Determine if a given path is a java root"
  (let ((path-dir (file-name-as-directory path)))
    (and (file-directory-p (concat path-dir "src/main/java"))
         (file-directory-p (concat path-dir "src/test/java")))))

(defun get-java-full-item-path (type item)
  "Get an item path relative to the current java root"
  (cond ((equal type "model") (get-java-main-path (concat item ".java")))
        ((equal type "spec") (get-java-test-path (concat item ".java")))
        ((equal type "test") (get-java-test-path (concat item ".java")))))

(defun get-java-main-path (path)
  "Get the main src path of this java project, including any empty packages like com/company/project"
  (concat (get-java-main-root) path))

(defun get-java-test-path (path)
  "Get the test src path of this java project, including any empty packages like com/company/project"
  (concat (get-java-test-root) path))
