;;; config.el --- Personal configuration for init.el and emacs.el.
;;; Commentary:

;;; As the generated message should have said, copy this to `config.el` and
;;; personalize that copy.

;;; For recent Emacs consider saving these in some auth-source accessed place.

;;; Code:

(defvar my-login "your-username" "Borrowers will want to change this.")
(defvar my-name "Your Full name")   ; when user-full-name isn't set

;; Avoid custom stomping on init.el
(let* ((f (expand-file-name
           "~/.config/emacs-custom.el"  ; Before changing this read C-h f custom-file
           )))
  (cond ((file-readable-p f)
         (setq custom-file f)
         (load custom-file)
         (message "Loaded custom definitions from %s" f))))

;; satisfy flycheck
(eval-when-compile (require 'url-vars)
                   (defvar mastodon-instance-url) ; mastodon.el
                   (defvar mastodon-active-user)
                   (defvar mastodon-client--token-file)
                   )

;; Personal settings
(setq mail-host-address (system-name)
      user-mail-address "your_email@eaxmple.com"
      mastodon-instance-url "https://your-mastodon-instance"
      mastodon-active-user "your-mastodon-user-name"
      mastodon-client--token-file (expand-file-name "/path/to/mastodon.plstore")
      )

;; Additional directories for load-path
(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

;; Set up so that backups, etc., all end up in one place
(defvar my-backup-dir (expand-file-name "~/Downloads/EmacsBackups")
  "Where backups, lock files, etc. go.")
(unless (file-directory-p my-backup-dir)
  (make-directory my-backup-dir t)
  (message "Created %s" my-backup-dir))

;; Once copied to config.el you may want to remove these next comments.
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;; flycheck: user is supposed to copy this to config.el
(provide 'config)
;;; config.el ends here
