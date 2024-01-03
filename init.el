;; init.el --- Emacs init file
;;; Commentary:
;;; Look for and load user's config file of personalizations followed by the
;;; byte-compiled full set of initializations.

;;; Code:
;(setq debug-on-error t)
(defvar do-init t "Whether to load byte-compiled full initialization file.")
(defvar config-file "config.el" "Configure personalizations.")
(defvar full-init-file "emacs.el" "What would generally be in \='user-init-file\='.")
(defvar user-warnings "*Warnings*" "Buffer for user messages.")
(defvar my-login)  ; Should be set in config-file"

(defun issue-warning (new-buffer text &rest args)
  "Let user know of issues by formatting TEXT, optionally in NEW-BUFFER with ARGS."
  (if new-buffer
      (progn
        (and new-buffer (buffer-live-p user-warnings)
	     (kill-buffer user-warnings))
        (switch-to-buffer-other-window user-warnings)
        (insert (format "\nHi %s,\n\n" (user-full-name)))))
    (switch-to-buffer-other-window user-warnings)
    (insert (apply #'format text args)))

(defun load-config (user-config generic-config)
  "Load user specific configuration file USER-CONFIG.
if not found load GENERIC-CONFIG and issue warning."
  (if (file-exists-p user-config)
      (load user-config t)
    (message "loading fallback %s" generic-config)
    (load generic-config)
    (issue-warning t "
%s was used to configure Emacs for you.
Please copy it to %s
and customize its content to your specifications.
"
	           generic-config user-config)))

(defun borrower-warning (user-login full-init)
  "Let USER-LOGIN borrower know that FULL-INIT isn't exactly standard."
  (if (not (string-match user-login (downcase (user-login-name))))
      (issue-warning nil "
You have discovered Jonathan's Emacs initialization files. I would
suggest NOT using this as your model. You may find some things
of interest in %s which this loads.
"
               (concat (file-name-directory user-init-file) full-init))))

(defun setup-load-path (elisp-path)
  "Add ELISP-PATH directory tree containing to \='load-path\='."
  ;; Setup so load-path can be set by `emacs -u [my-login]`, which used to
  ;; make sense back in days when multi-user systems were more open.
  (let ((default-directory (expand-file-name "lisp" elisp-path)))
    (push default-directory load-path)
    (normal-top-level-add-subdirs-to-load-path)) ;; Uses default-directory internally
  )

(defun update-and-load-full-init (init-file compiled-init)
  "If out of date byte-compile INIT-FILE to COMPILED-INIT/."
  ;; Historically, for speedier start up, I byte-compile most of what
  ;; would normally be in a user-init-file from the larger init-file.

  (if (and (file-newer-than-file-p init-file compiled-init)
           (string= (user-login-name) my-login))
      (progn
        (issue-warning nil "%s is not up to date, byte-compiling %s..." init-file compiled-init)
        (sit-for 1)
        (if (byte-compile-file init-file)
            (issue-warning nil "byte compiled %s" init-file)
          (issue-warning nil "error byte compiling %s" init-file)
          )))
  (cond
   (do-init
    (if (file-readable-p compiled-init)
        (and (load-file compiled-init) (message "ready"))
      (message "no byte compiled %s in %s" init-file compiled-init)
      (load-file init-file)))))

(defun config-and-load (config-path generic-config)
  "Initialize by loading CONFIG-PATH (or GENERIC-CONFIG)."
  (load-config config-path generic-config)
  (borrower-warning my-login full-init-file)

  (let* ((elib (file-name-directory user-init-file))
         (el   (concat elib full-init-file))
         (elc  (concat el "c")))

    (setup-load-path elib)
    (update-and-load-full-init el elc)
    (cd "~"))

  (if (buffer-live-p user-warnings)
      (switch-to-buffer-other-window user-warnings)))

(let* ((config-path (concat (file-name-directory user-init-file) config-file))
       (generic-config (replace-regexp-in-string "\.el" "-generic.el" config-path)))
  (config-and-load config-path generic-config))


(provide 'init)
;; Any executable lisp after this should be in custom.el
;;; init.el ends here
