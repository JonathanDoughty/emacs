;;; config.el --- Personal configuration
;;; Commentary:

;;; Personal informatiom that you don't want in init.el or emacs.el.  As the generated message
;;; should havce said, you should copy this to `config.el` and personalize that copy.

;;; These maybe belong in some auth-source accessed place.

;;; Code:

(defvar my-login "your-username" "Borrowers will want to change this.")
(defvar my-name "Your Full name")   ; when user-full-name isn't set

;; satisfy flycheck
(eval-when-compile (require 'url-vars)
                   (defvar mastodon-instance-url) ; mastodon.el
                   (defvar mastodon-active-user)
                   )

;; email settings
(setq mail-host-address (system-name)
      user-mail-address "your_email@eaxmple.com"
      mastodon-instance-url "https://mastodon.social"
      mastodon-active-user "mastodon-user-name")

(let* ((home (concat "~" init-file-user "/")) ; when; on multi-user systems, --user was possible.
       (elib (concat home ".emacs.d/")))
  (defvar user-emacs-directory (expand-file-name elib)
    "Where all this appears to live."
    ))

(defvar my-backup-dir (expand-file-name "~/Downloads/EmacsBackups")
  "Where backups, lock files, etc. go.")
(unless (file-directory-p my-backup-dir)
  (make-directory my-backup-dir t)
  (message "Created %s" my-backup-dir))

(provide 'config)
;;; config.el ends here
