# Remote Load

To load this environment remotely, simply execute the following from within emacs:

    (let* ((url "https://raw.github.com/mikestone/my-emacs-env/master/remote-load.el")
    	(remote-buffer (url-retrieve-synchronously url)))
      (switch-to-buffer remote-buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (delete-region (point-min) (1+ (point)))
      (eval-buffer)
      (kill-buffer remote-buffer))
