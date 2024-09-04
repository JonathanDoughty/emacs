;;; .emacs.el --- Emacs initializations
;;; Commentary:

(defconst jwd/warning "
Warning: Many years of cruft here.

Some from using different flavors of emacs on assorted platforms over the decades.
More from adapting to what made various projects easier over that time.
Too much from yak shaving.
")

;;; Code:
(eval-when-compile
  (if (not (require 'config "config.el" t))
      (message "config not found, have you personalized config-generic.el?")))

(defvar my-login-name (downcase (user-login-name))
  "Borrowers may want to  change this."
  )

;; Flavors of Emacs
(defconst running-emacs-for-macos (numberp (string-match "darwin.*NS appkit" (emacs-version)))
  "The \='pure\=' GNU EmacsForMacOS native GUI support version on Macs.")
(defconst running-emacs-mac-port (numberp (string-match "darwin.*Carbon" (emacs-version)))
  "YAMAMOTO's Emacs Mac Port version on Macs.")
(defconst running-emacs-mac (numberp (string-match "darwin" (emacs-version))))
(defconst running-aquamacs (featurep 'aquamacs)
  "Aquamacs due to 64-bit issues on an ancient Mac.")
(defconst running-xemacs (featurep 'xemacs)
  "XEmacs was my preferred flavor on X11.")
(defconst running-fsf
  (not (or running-xemacs running-aquamacs running-emacs-mac-port))
  "I started with canonical Emacs and still come back regularly.")

;; Version specific settings

(when (>= emacs-major-version 26)
  (setq confirm-kill-processes nil))

(when (>= emacs-major-version 29)
  (setq mouse-drag-and-drop-region-cross-program t)
  (setq mouse-drag-copy-region t)
  (setq tty-select-active-regions t)
  (setq show-paren-context-when-offscreen t)
  )

;; Flavors of OS
(defconst is-linux (string-match "linux" (symbol-name system-type)))
(defconst is-macos (string-match-p "darwin" (symbol-name system-type)))
(defconst is-unix (or (string-match "n.x" (symbol-name system-type))
                      is-macos) "A unix-like system, Solaris maybe?")
(defconst is-windows (string-match "windows" (symbol-name system-type))
  "Using Windows is exceedingly rare for me.")

;; Be modern with packages; though use-package will be a critical dependency
;; prior to emacs 29 and requires some bootstrapping
(when (> emacs-major-version 23)
  ;; enable packages and extras
  (require 'package)
  (add-to-list 'package-archives
               ;; be sure default elpa's signature has been updated too
               '("melpa-stable" . "https://stable.melpa.org/packages/")
               ;;  http only & https expired
               ;; '("marmalade" . "http://marmalade-repo.org/packages/")
	       )
  )

;; Avoid some free variable warnings used a lot on byte-compile
(eval-when-compile
  (require 'cc-vars))

(if (< (string-to-number emacs-version) 29.1)
  (eval-after-load 'gnutls
    (progn
      '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem")))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (eval-when-compile
      (unless (bound-and-true-p package--initialized)
        (package-initialize) ;; be sure load-path includes package directories during compilation
        )
      ))
  )
(require 'use-package)
(require 'bind-key)  ;; because some use-package uses :bind

;; Adjust defaults
(unless (getenv "LANG")
  (setenv "LANG" "en_US.UTF-8"))
(set-locale-environment)

;; Enable emacs to find executables that shells do
(defvar local-bin
  (let ((dir "/opt/homebrew/bin"))
    ;; ToDo grr, make this work with a list of candidates
    ;;(dolist (dir '("/opt/homebrew/bin" "/usr/local/bin"))
    (when (file-directory-p dir) dir)
    )
  "Where locally installed executables are most likely to be found."
  )
;; My bash initialization conflicts with normal shell solutions
(if (boundp 'local-bin)
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":" local-bin))
      (add-to-list 'exec-path local-bin)
      (message "%s added to PATH, exec-path" local-bin))
  (message "WTF, no local-bin?"))

(setq-default fill-column 95)           ; 70 is so last century

;; Formerly useful to let me control all top level hooks should I really mess up.
;; Rarely used now, perhaps better done with advice.
(defconst jwd/add-hooks t "Set to nil will skip all \='add-hook\=' while chaos is fixed.")
(defconst jwd/verbose-add-hook nil "Set to t may help me find where I have goofed.")
(defun jwd/add-hook (hook fn &optional append)
  "When jwd/add-hooks is non-nil install FN to HOOK.
Be verbose if jwd/verbose-add-hook is non-nil."
  (interactive)
  (when jwd/add-hooks (add-hook hook fn append))
  (when jwd/verbose-add-hook
    (message "%s %s to hook %s %s"
             (if jwd/add-hooks "added" "skipped adding")
             fn hook append))
  t)

(defvar jwd/backup-dir
  (progn
    (defvar my-login) ;; see config*.el
    (cond (is-macos ;; Keep backups where I can autoclean them periodically
           (defvar my-backup-dir) ;; see config*.el
           (let ((dir my-backup-dir))
             (unless (file-exists-p dir)
               (make-directory dir))
             (expand-file-name dir)))
          ((string-match my-login my-login-name)
           ;; avoid problems if using this init file as some other user
           (expand-file-name
            (concat
             (if (boundp 'user-init-directory) user-init-directory
               (if (boundp 'user-emacs-directory) user-emacs-directory
                 "~/")) "backups/" )))
          (t "~/Downloads")))
  "Where to save backups on this machine." )

;; Deposit backups, auto-saves, and other crud in a single, machine local directory.
;; Define this before custom is loaded.
;; Consider https://github.com/emacscollective/no-littering As an alternative
(eval-when-compile
  (require 'message)
  (let* ((default (and (boundp 'jwd/backup-dir) (symbol-value 'jwd/backup-dir)))
         (dir (or default (expand-file-name "~/Downloads/EmacsStuff"))))
    (unless (file-directory-p dir)
      (make-directory dir))
    (setq backup-directory-alist `((".*" . ,dir))
          auto-save-file-name-transforms
          `((".*" ,(concat dir "/") t))
          auto-save-list-file-prefix (concat dir "/.saves-")
          delete-auto-save-files nil      ; keep a safety net
          create-lockfiles t ; lock files confuse filesystem watchers
          lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat dir "/\\1") t))
          message-auto-save-directory dir))
  )

;; Before a buffer is reverted make a final auto-save
(jwd/add-hook 'before-revert-hook
              (lambda ()
                (write-region (point-min) (point-max) (make-auto-save-file-name))))

;; Avoid custom stomping on init.el
(let* ((f (expand-file-name "custom.el" user-emacs-directory)))
  (cond ((file-readable-p f)
         (setq custom-file f)
         (load custom-file))))

;;; Function definitions

;; companions to bash aliases
(defun jwd/ppath (dir)
  "Add DIR to the front of Emacs' environment's path."
  (interactive "DDirectory: ")
  (if (file-directory-p dir)
      (setenv "PATH" (concat (expand-file-name dir)
                             path-separator (getenv "PATH")))))

(defun jwd/rpath (dir)
  "Remove DIR from the environment's path."
  (interactive "DDirectory: ")
  (setq dir (expand-file-name dir))
  (if (file-directory-p dir)
      (let* ((p (getenv "PATH"))
             (s (string-match (concat dir ":") p)))
        (if s
            (setq p (replace-match "" t t p))
          (setq s (string-match (concat ":" dir) p))
          (setq p (replace-match "" t t p)))
        (setenv "PATH" p))))

;; quote neatly
(defun jwd/delete-leading-whitespace (&optional prefix)
  "Delete leading whitespace from lines in region, optionally add a PREFIX.
Adapted from prefix-region"
  (interactive "PPrefix string: ")
  (let ((count (count-lines (mark) (point))))
    (goto-char (min (mark) (point)))
    (while (> count 0)
      (setq count (1- count))
      (beginning-of-line 1)
      (delete-horizontal-space)
      (insert prefix)
      (end-of-line 1)
      (when (< (point) (point-max))
        (forward-char 1)))))

;; http://www.emacswiki.org/emacs/DeletingWhitespace
(defun whack-whitespace (arg)
  "Delete all white space from current point to the next word.
With prefix ARG delete across newlines as well.  The only danger
in this is that you don't have to actually be at the end of a
word to make it work.  It skips over to the next whitespace and
then whacks it all to the next word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun jwd/inhibit-delete-trailing-whitespace ()
  "This might get you out of a jam.
Not sure this works reliably, possibly useful in cases where something in the
\='save-buffer\=' / ws-butler chain insists on removing ^M."
  (interactive)
  (cond ((local-variable-p 'write-file-hooks)
         (remove-hook 'write-file-hooks 'delete-trailing-whitespace t))))

;; Key bindings related to the above display/whitespace manipulation
(define-key global-map (kbd "C-c l") 'jwd/delete-leading-whitespace)
(define-key global-map (kbd "C-c C-SPC") 'whack-whitespace)
(define-key global-map (kbd "C-c SPC") 'delete-trailing-whitespace)
(define-key global-map (kbd "C-c o") 'just-one-space) ; default M-SPC is macOS SpotLight

;; Where/how I like my buffers displayed
;; Warning: further yak-shaving means a trip down the display-buffer-alist rabbit hole
(defun jwd/list-buffers (&optional arg)
  "It seems I am always immediately switching to the buffer list anyway.
Pass ARG along to \='list-buffers\='."
  (interactive "P")
  (list-buffers arg)
  (pop-to-buffer "*Buffer List*"
                 '((display-buffer-reuse-window
                    display-buffer-same-window
                    display-buffer-below-selected
                    display-buffer-at-bottom)
                   (window-height . fit-window-to-buffer)))
  )
(define-key ctl-x-map  (kbd "C-b") 'jwd/list-buffers)

;; originally from netnews, modified to go both ways
(defun jwd/dos-unix (&optional unix-dos)
  "Convert DOS line ending chars to the correct, Unix style.
With prefix arg UNIX-DOS, go the other way."
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (if unix-dos
        (while (search-forward "\n" nil t) (replace-match "\r\n"))
      (while (search-forward "\r" nil t) (replace-match "")))))

;; Package integration

(defun jwd/package-reminder ()
  "Remind me of a repeated memory failure when managing packages."
  (interactive)
  (message "U - mark-upgrades; /m - list-upgrades; x - execute upgrades")
  (pop-to-buffer "*Messages*" t)
  (goto-char (point-max)))
(jwd/add-hook 'package--post-download-archives-hook
          #'jwd/package-reminder 'append)

(use-package auth-source
  :disabled
  :requires config ;; defines authinfo credential path
  :init
  (when (memq window-system '(mac ns))
    (setq auth-sources '(macos-keychain-generic
                         macos-keychain-internet
                         :source authinfo-source)
          auth-source-debug t ;; WIP
          )))

(use-package caddyfile-mode
  ;;:disabled
  :ensure t
  :mode (("Caddyfile.*\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package csv-mode                   ; more than the built-in ses-mode
  :ensure t
  :hook (csv-mode-mode . csv-align-mode))

(use-package company                    ; complete anything
  :disabled                             ; cause of EmacsForMacOS hangs/crashes?
  :ensure t
  :config
  (company-tng-mode)                    ; tab through completions
  (global-company-mode t))

(use-package dired
  :disabled
  :config
  ;; much more useful to me than the default dired-copy-filename-as-kill
  (define-key dired-mode-map  (kbd "w") 'wdired-change-to-wdired-mode)
  ;; From: sof@dcs.glasgow.ac.uk (Sigbjorn Finne)
  (defun dired-sort ()
    "Dired sort hook to list directories first."
    (interactive)
    (save-excursion
      (let (buffer-read-only)
	(forward-line 2);; beyond dir. header
	(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (set-buffer-modified-p nil)
    (add-hook 'dired-after-readin-hook 'dired-sort)))

;; use all IDE indentation standard
(use-package editorconfig
  ;;:disabled
  :ensure t
  :commands                             ; inform flycheck
  #'editorconfig-get-properties-from-exec
  :config
  (editorconfig-mode 1)
  (set-variable 'editorconfig-get-properties-function
                ;; from external program, never internal fallback
                #'editorconfig-get-properties-from-exec)
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :bind
  ("C-C e" . editorconfig-display-current-properties)
  )

;;; On the fly syntax checks
(use-package flycheck
  ;;:disabled
  :ensure t
  :init (setq sentence-end-double-space nil)
  :config (global-flycheck-mode 1)
  )

;;; Help alternative with more context: https://github.com/Wilfred/helpful
(use-package helpful
  ;;:disabled
  :ensure t
  :bind                                 ; replace defaults
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-c C-d" . helpful-at-point)
  )

;;; Spell checking
(use-package ispell
  ;;:disabled
  :ensure t
  :bind
  (("H-i" . ispell-word)
   ([(meta shift i)] . ispell-word))
  :config
  (setq ispell-use-framepop-p nil)
  (setq ispell-choices-win-default-height 3) ; default 2 gets truncated
  ;; (setq ispell-personal-dictionary
  ;;       (expand-file-name "aspell-dictionary" user-emacs-directory)) ; holdover
  (eval-when-compile (defvar ispell-program-name))

  ;; adapted from https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs/
  (cond
   ;; try hunspell first; if hunspell can't be found try aspell
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
          ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

    ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
    ;; If it's nil, Emacs tries to automatically set up the dictionaries.
    (when (boundp 'ispell-hunspell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))
    (message "Set up hunspell for spell-checking"))

   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
    (message "Set up aspell for spell-checking"))

   ((executable-find "ispell")
    (setq ispell-program-name "ispell")
    (message "Set up ispell for spell-checking"))

   (t
    (message "Couldn't find hunspell, aspell, or ispell executables; spell checks will fail"))))

;; just
(use-package just-mode
  ;;:disabled
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '(".*[Jj]ustfile.*" . just-mode) t)
  )

;; Mode-line cleanup
(use-package minions
  :disabled
  :ensure t
  :config (minions-mode 1))

;; Emacs debugging
(use-package realgud
  :disabled                             ; unless I'm actually debugging eith gdb
  :ensure t
  :bind
  ((["C-c d"] . realgud:bashdb)))

;; Paren matching
(use-package smartparens
  :disabled
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ; add `smartparens-mode` to these hooks
  :commands sp-pair                         ; inform flycheck
  :config
  (progn
    ;; See https://emacs.stackexchange.com/a/29620/5146
    ;; and https://github.com/Fuco1/smartparens/wiki/Permissions
    ;; could try:
    (sp-pair "`" nil :when '(sp-point-before-same-p))
    (sp-pair "\"" nil :when '(sp-point-before-same-p))
    )
  ;; load default config
  (require 'smartparens-config))
;; Related, via https://www.emacswiki.org/emacs/NavigatingParentheses#h5o-4
(defun forward-or-backward-sexp (&optional arg)
  "Go to the ARGth matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))
(define-key global-map (kbd "C-c %") 'forward-or-backward-sexp) ;; a crutch left over from vi days

;;; Org mode - the hit by a bus possibility makes me not change
(use-package org
  :disabled                             ; unless I've gone back to trying org
  :config
  (defun jwd/org-mode-hook ()
    "An attempt to move off TiddlyWiki."
    (setq org-return-follows-link t)
    (setq org-tab-follows-link t)
    )
  :hook jwd/org-mode-hook
  :bind (
         :map org-mode-map
         ("C-C l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         )
  )

;; Integrate TiddlyWiki tiddler and config editing
(use-package tiddler-mode
  ;;:disabled
  :mode ("\\.tid\\'" . tiddler-mode)
  :config
  (add-to-list 'auto-mode-alist '("tiddlywiki\\.info$" . js-mode) t)
  )

;; Inverse of fill
(use-package unfill
  ;;:disabled
  :ensure t
  :bind (([remap fill-paragraph] . unfill-toggle)
         ([(meta Q)] . unfill-paragraph))
  :config
  ;; add * bullet characters so lists are not collapsed by fill-paragraph
  (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")
  )

;; Generic language support
(use-package lsp
  :disabled                             ; should I be using eglot?
  :ensure t
  :config
  (setq lsp-keymap-prefix "s-l"))
;;(jwd/add-hook 'XXX-mode-hook #'lsp) ;; as needed, where XXX is python, java, ...

;; java-mode
(eval-when-compile
  (defvar java-mode-map)
  ;;(require 'subr-x)
  )
(defun jwd/java-mode-hook ()
  "Setup for (now rare) editting java source on various platforms."
  (interactive)
  ;; over-ride c-up-conditional
  (define-key java-mode-map  (kbd "C-c C-u") 'jwd/dos-unix)
  (cond (is-unix
         (jwd/ppath "/opt/local/java/current/bin")
         (jwd/ppath "/usr/local/java/current/bin")
         (jwd/ppath "/usr/local/java/bin"))
        (is-macos
         (jwd/ppath                     ; add current macOS's java_home bin to PATH
          (concat (string-trim(shell-command-to-string "/usr/libexec/java_home")) "/bin")))
        (is-windows
         (jwd/ppath "C:\\Java\\current\\bin")
         (setenv "MAKE_MODE" "UNIX")    ; from sample init
         ))
  (cond ((display-graphic-p)
         (set (make-local-variable 'font-lock-maximum-decoration) t)
         )))
(jwd/add-hook 'java-mode-hook 'jwd/java-mode-hook)
(jwd/add-hook 'java-mode-hook #'lsp)

;; shell-mode
(eval-when-compile (require 'shell))
(when is-macos                          ; use homebrew's current bash
  (let ((bash-path (expand-file-name "bash" local-bin)))
    (defvar sh-shell-file bash-path)
    (setq explicit-shell-file-name bash-path))
  )
(defun jwd/shell-mode-hook ()
  "Convenience settings for \='shell-mode\='."
  (define-key shell-mode-map (kbd "C-C C-.") 'shell-dirtrack-toggle)
  (define-key shell-mode-map (kbd "C-c .") 'shell-resync-dirs)
  ;;(copy-face 'default 'comint-input-face) ; avoid color customizations
  (setq
   tab-width 8
   truncate-lines nil
   shell-pushd-regexp "pd\\|pd[ \.\$/a-zA-Z0-9]*"
   shell-popd-regexp  "pop\\|pop[ +0-9]*"
   ;; shell-prompt-pattern "^[^#$%>]*[#$%>] *"
   shell-prompt-pattern (concat "^" (user-login-name) "@[^#$%>]*[#$%>] *")
   comint-buffer-maximum-size 4096      ; in lines
   comint-prompt-regexp shell-prompt-pattern
   comint-password-prompt-regexp
   "^\\(\\([^@]+@[^']+'s \\|[Oo]ld \\|[Nn]ew \\)?[Pp]assword\\|pass *phrase\\):\\s *\\'"
   comint-input-ring-size 99
   )
  (cond (running-fsf
         (define-key shell-mode-map [(meta p)] ; normally comint-previous-input
                     'comint-previous-matching-input-from-input))) ; matches Xemacs
  )
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(jwd/add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(jwd/add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(jwd/add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(when (not is-unix)
  (jwd/add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))
(jwd/add-hook 'shell-mode-hook 'jwd/shell-mode-hook)

;; Find the buffer with the shell
(defun jwd/find-shell (&optional shell-only)
  "Find end of shell buffer or create one by splitting the current window.
If shell is already displayed in current frame or SHELL-ONLY,
delete other windows in frame. Stop displaying shell in all
other windows; then go to end of buffer and return the buffer."
  (interactive "p")
  (let* ((shellbuf (get-buffer "*shell*")))
    (if (or (eq (window-buffer) shellbuf) shell-only)
        (delete-other-windows)
      (if (eq 1 (count-windows))
          (split-window-vertically))
      (if (not (eq (window-buffer) shellbuf))
          (other-window 1)))
    ;; undisplay shell in other windows (on other devices)
    (and shellbuf
         (> (length (get-buffer-window-list shellbuf nil t)) 0)
         (replace-buffer-in-windows shellbuf))
    (setq shellbuf (shell))
    (goto-char (point-max))
    (recenter -2)
    (get-buffer shellbuf)))

;; yak-shave: refactor the above so it handles both shell and term buffers
(defun jwd/find-term (&optional term-only)
  "Find end of terminal buffer or create one by splitting the current window.
Or not if TERM-ONLY."
  (interactive)
  (let* ((termbuf (get-buffer "*ansi-term*")))
    (if (or (eq (window-buffer) termbuf) term-only)
        (delete-other-windows)
      (if (eq 1 (count-windows))
          (split-window-vertically))
      (if (not (eq (window-buffer) termbuf))
          (other-window 1)))
    ;; undisplay term in other windows (on other devices)
    (and termbuf
         (> (length (get-buffer-window-list termbuf nil t)) 0)
         (replace-buffer-in-windows termbuf))
    (if termbuf (switch-to-buffer termbuf) (ansi-term shell-file-name))
    (goto-char (1- (point-max)))
    (recenter -2)
    (get-buffer termbuf)))

(defun jwd/query-replace-regexp () ;; https://emacs.stackexchange.com/a/47159/5146
  "Avoid 'Match data clobbered by buffer modification hooks' issues."
  (interactive)
  (let ((after-change-functions nil)) ; empty when executing this func
    (call-interactively 'query-replace-regexp)))

;; Mode hooks

(defun jwd/after-find-hook ()
  "I generally don't want to edit found functions."
  (read-only-mode))
(jwd/add-hook 'find-function-after-hook 'jwd/after-find-hook)

(defun jwd/text-mode-hook  ()
  "For \='text-mode\=' I remain old-school."
  (and is-unix
       (flyspell-mode +1))
  (setq fill-column 78)
  (message "Text mode hook turning on auto-fill")
  (turn-on-auto-fill))
(jwd/add-hook 'text-mode-hook 'jwd/text-mode-hook)

(eval-when-compile (require 'perl-mode))
(defun jwd/perl-mode-hook  ()
  "Override cperl's high-handed re-definitions."
  ;;(define-key cperl-mode-map [(meta backspace)] 'backward-kill-word)
  ;;(define-key cperl-mode-map [(control h) v] 'describe-variable)
  ;;(define-key cperl-mode-map [(control c) f] nil)
  )
(jwd/add-hook 'cperl-mode-hook 'jwd/perl-mode-hook)

(use-package lua-mode
  ;;:disabled
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  :hook #'lsp)

;; git
(use-package magit
  :disabled
  :bind
  (("H-g" . magit-status))
  )

(use-package markdown-mode
  ;;:disabled
  :mode "\\.md\\'"
  :config
  (define-key markdown-mode-map (kbd "C-C d")
              (lambda ()
                "Insert date for hugo metadata"
                (interactive)
                (delete-region (point) (line-end-position))
                ;; was 2021-04-24T11:18:00-04:00
                ;; now 2021-06-22T22:23:37-0400
                ;; which format-time-string claims is "full ISO 8601 format"
                (insert (format-time-string "%FT%T%z"))))
  ;;(remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link)
  (custom-set-variables '(markdown-command "pandoc"))
  :hook turn-on-auto-fill
  )

(use-package mastodon
  :disabled
  :requires config ;; defines mastodon credentials
  :ensure t)

;; process display
(use-package proced
  :disabled
  :custom
  (proced-auto-update-flag t)
  (proced-enable-color-flag t)
  (proced-auto-update-interval 3)
  )

;; Windows holdover
(use-package powershell-mode
  :disabled                             ; reminder if I ever go back here
  :mode "\\.ps1\\'"
  :interpreter "powershell")

;; Clipboard interaction
(use-package simpleclip
  ;;:disabled
  :ensure t
  :functions simpleclip-mode
  :config
  (simpleclip-mode 1)
  (when (memq window-system '(mac ns))                    ;; maybe should be is-mac?
    (define-key global-map [(super c)] 'simpleclip-copy)  ;; was 'kill-ring-save
    (define-key global-map [(super v)] 'simpleclip-paste) ;; was 'yank
    (define-key global-map [(super x)] 'simpleclip-cut) ;; was 'kill-region
    ))

;; Typsecript
(use-package tide
  :disabled
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; HTML
(use-package web-mode
  :disabled
  :mode "\\.html?\\'"
  :config
  (defvar web-mode-engines-alist '(("go" . "\\.html\\'")))
  (defvar web-mode-enable-engine-detection t)
  :hook
  (defvar web-mode-enable-current-column-highlight t)
  )

;; window configuration: C-c {<left>,<right>}
(use-package winner
  :disabled
  :ensure t)

;; trim whitespace
(use-package ws-butler
  :ensure t
  :hook (on-first-buffer . ws-butler-global-mode)
  ;; maybe just text-mode and prog-mode ?
  )

(use-package yaml-mode
  :disabled
  :mode "\\.yml\\'"
  :interpreter ("yaml" . yaml-mode))

;;; Some derived modes I've gen-ed up; others in lisp archive

(define-derived-mode hvi-mode text-mode "HVI"
  "Mode for editing OCR-ed HVI text files."
  (setq case-fold-search nil)
  (set (make-local-variable 'tab-always-indent) nil)
  (set (make-local-variable 'indent-tabs-mode) t)  ; allow insert of real tabs
  (highlight-changes-mode 1)
  (whitespace-mode 1)
  (highlight-regexp "^==+" "hi-yellow")
  ;; (highlight-regexp "[\t]+" "hi-salmon")
  (highlight-regexp "^[a-z0-9.]+" "hi-aquamarine")
  (highlight-regexp "\$[0-9.]+" "hi-pink")
  (define-key hvi-mode-map (kbd "<tab>") "	") ; a real tab for TAB
  )
(jwd/add-hook 'hvi-mode-hook
          (lambda ()
            "Override my \='text-mode\=' hook, which turns on auto-fill-mode."
            (message "HVI mode turning off auto-fill")
            (turn-off-auto-fill)))

;; kill ring
(autoload 'browse-kill-ring "browse-kill-ring" t)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;;; Set some other variables that are the same for all systems.
(setq-default
 default-case-fold-search t             ; ignore case on searches
 inhibit-startup-screen t               ; I've seen it
 mail-signature t
 mail-self-blind nil                    ; switch to Bcc:
 make-backup-files t                    ; save previous in file~
 message-from-style 'angles
 passwd-invert-frame-when-keyboard-grabbed nil
 require-final-newline t                ; sigh, some applications need it
 )

;;; Various additions to auto mode

(add-to-list 'auto-mode-alist '("README$" . text-mode) t)
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode) t)
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode) t)
(add-to-list 'auto-mode-alist '("\\.php$"  . html-mode) t)    ; php-mode
(add-to-list 'auto-mode-alist '(".envrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '(".gitconfig.*\\'" . conf-mode))

;;; Default values for buffer local variables
(setq-default indent-tabs-mode nil)     ; never insert tabs

;;; Some key bindings I've grown to depend on
;;; Overrides:
(define-key global-map  (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map  (kbd "C-r") 'isearch-backward-regexp)
;;; User keys:
(define-key global-map  (kbd "C-c z") 'jwd/find-shell)
(define-key global-map  (kbd "C-c b") 'bury-buffer)
;; Ctrl-x overrides:
(define-key ctl-x-map   (kbd "C-r") 'jwd/query-replace-regexp)
(define-key ctl-x-map   (kbd "C-q") 'view-mode) ;; replace the less useful read-only-mode
(define-key ctl-x-map   (kbd "c") 'goto-char)
(define-key ctl-x-map   (kbd ":") 'goto-line)
(define-key ctl-x-map   (kbd "?") 'what-line)
(define-key global-map [(control meta tab)] 'completion-at-point)

;; Misc
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)   ; almost never intentional
(fset 'yes-or-no-p 'y-or-n-p)           ; single char answers preferred

;;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun jwd/unfill-region (&optional region)
  "Convert a multi-line REGION into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map [(meta shift q)] 'jwd/unfill-region)

(defun jwd/activate-mail-or-compose (&optional arg)
  "Switch to *unsent mail* buffer if it exists; with prefix ARG compose new mail."
  (interactive "P")
  (if arg
      (compose-mail)
    (let ((mail-buffer (get-buffer "*unsent mail*")))
      (if mail-buffer
          (switch-to-buffer mail-buffer)
        (compose-mail)))))
(define-key ctl-x-map  (kbd "m") 'jwd/activate-mail-or-compose)

(defun jwd/message-unfill ()
  "Unfill paragraphs of a message buffer, then select and copy the whole message.
Sort of the inverse of message-fill-yanked-message.  Because I will typically
use a message buffer to compose something for another app."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (message-goto-body) (point-max)))
  (message-goto-body)
  (set-mark-command nil)
  (message-goto-signature)
  (setq deactivate-mark nil)
  (and (featurep 'simpleclip) (simpleclip-copy (point) (mark))))
(require 'message)
(define-key message-mode-map [(meta shift q)] 'jwd/message-unfill)

(defun jwd/align-region ()
  "Via mastodon: align lines of text into columns ...
this confusing monstrosity is what you want 99% of the time"
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\s-" 1 1 t)
  )

;; Python
(define-coding-system-alias 'UTF-8 'utf-8) ;; IntelliJ (?) uses upper

(defvar xml-font-lock-keywords
  (list '("<!-- .* -->" . font-lock-comment-face)
        '("\".*\"" . font-lock-doc-string-face)
        '("<[^>]+>" . font-lock-keyword-face)
        ))

;; scripts in general
(jwd/add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Advise annoyances

(defadvice kill-buffer (around confirm-some-buffers activate)
  "Kill some buffers only upon confirmation."
  (interactive "bKill buffer: ")
  (if (member (ad-get-arg 0) '("*shell*" "*Group*"))
      (if (y-or-n-p (format "Really kill `%s'? " (ad-get-arg 0)))
          ad-do-it)
    ad-do-it))
;; why doesn't get-file-buffer deal gracefully with nil?
;; The cause of lots of "Wrong type argument: stringp, nil"s?
(defadvice get-file-buffer (before non-nil-get-file-buffer-arg activate)
  "Make sure 'get-buffer' doesn't get a nil."
  (or (ad-get-arg 0) (ad-set-arg 0 "")))

;; advise html-mode (psgml-html) about some things I'd rather have in a
;; set up hook but which html-mode wants to over-ride.
(defadvice html-mode (after jwd/html-setup activate)
  "Kludge to get around psgml's re-doing keys and variables after sgml-mode-hook has been called."
  (interactive)
  ;; 'cause overriding html-helper-default-insert-timestamp doesn't
  (setq  html-helper-timestamp-hook 'jwd/html-helper-timestamp
         sgml-indent-step 1
         sgml-indent-data t)
  ;; fix overrides of others
  (define-key html-mode-map [C-cC-z] 'jwd/find-shell)
  )

;; via https://www.reddit.com/r/emacs/comments/555upt/i_love_the_new_ms_mw_binding_but/
(defun jwd/eww-search (orig-fun &rest args)
  "Deal with eww bug: insure there is a query before calling ORIG-FUN with ARGS."
  (if (region-active-p) (apply orig-fun args)
    (eww (read-string "Search web for: "))))
(and (functionp 'advice-add) ;; TODO Covert this back to defadvice for older emacs
     (advice-add 'eww-search-words :around #'jwd/eww-search))

;;; replacements for functions with now built-in equivalents
(defalias 'wc-region 'count-words)
(defalias 'face-at-point 'describe-face)

(use-package f
  :ensure t
  )
(eval-when-compile
  (declare-function f-root? "f")
  (declare-function f-same? "f"))
;;; Adapted from https://github.com/DarwinAwardWinner/dotemacs#user-content-fix-default-directory
(defun jwd/resync-directories ()
  "Update \='default-directory\=' to ~ in buffers such as *shell*, *scratch*, ..."
  (interactive)
  (let ((startup-dir default-directory))
    (unless (f-same? default-directory "~")
      (dolist (buf (buffer-list))
        (ignore-errors
          (with-current-buffer buf
            (when (and (null (buffer-file-name buf))
                       (not (bound-and-true-p dired-directory))
                       (or (f-same? default-directory startup-dir)
                           (f-root? default-directory)))
              (message "Changing default dir from %s to ~/ in %s"
                       default-directory (buffer-name buf))
              (cd "~"))))))))

;; UI set up
(defun jwd/gui ()
  "Set up my most frequently used GUI features."
  (interactive)
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-dark)
    )
  (setq-default
   select-enable-clipboard t
   mouse-drag-copy-region nil
   select-enable-primary nil
   yank-pop-change-selection t
   )
  (setq mouse-yank-at-point t)          ; rather than at click position
  (which-function-mode)
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))
  (setq-default split-width-threshold nil) ;; dislike horizontal splits
  (message "Welcome %s - Emacs GUI initialized" (user-login-name)))

(defun jwd/window-setup-hook ()
  "Invoke my GUI customizations if there is a window environment."
  (when (display-graphic-p) (jwd/gui)))
(jwd/add-hook 'emacs-startup-hook 'jwd/window-setup-hook)

;;; Emacs version specific initializations
(defun jwd/emacs-mac-keys ()
  "Setup my preferred Emacs key modifiers and bindings on Macs."
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)
  (cond
   (running-emacs-mac
    (setq mac-command-modifier 'super)    ; make command key do Super
    (setq mac-option-modifier 'meta)      ; and use option key for Meta
    (setq mac-function-modifier 'hyper)   ; (currently unused) H-
    ;; Put standard Mac operations on Command key
    (define-key global-map [(super a)] 'mark-whole-buffer)
    (define-key global-map [(super l)] 'goto-line)
    (define-key global-map [(super q)] 'save-buffers-kill-emacs)
    (define-key global-map [(super m)] 'iconify-frame)
    (define-key global-map [(super s)] 'save-buffer)
    (define-key global-map [(super w)] (lambda () (interactive) (delete-frame)))
    (define-key global-map [(super z)] 'undo)
    ;; Others on keypad keys when available
    (define-key global-map [home] 'move-beginning-of-line)
    (define-key global-map [end] 'move-end-of-line)
    (define-key global-map [(meta down)] 'scroll-up-command)
    (define-key global-map [(meta up)] 'scroll-down-command)
    (cond                               ; slight differences between macos versions
     (running-emacs-mac-port
      (message "running-emacs-mac-port")
      (defvar mac-frame-tabbing nil)    ; I prefer separate frames to Mac tabs
      ;; (define-key global-map (kbd "s-`") 'other-frame) ;; emacs-mac-port uses handle-switch-frame
      )
     (running-emacs-for-macos
      (message "running-emacs-for-macos")
      (define-key global-map [(super "`")] 'other-frame)
      )
     (t
      (message "Running neither Emacs for Mac nor railwaycat; did not set up mac keys")
      )))
   ((<= emacs-major-version 23) ;; historical: preferred at EmacsWiki/EmacsForMacOS
    (setq mac-option-modifier 'alt)
    (setq mac-command-modifier 'meta)
    ;; I don't use these old versions any longer
    )
   ))

(defun jwd/mac-init ()
  "Set up my preferred key bindings and other Mac specific capabilities."
  (interactive)
  ;; (cua-mode) ;; needed on mac with the following?
  (jwd/emacs-mac-keys)
  ;; http://emacs.stackexchange.com/a/10963/5146
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (setq interprogram-paste-function 'copy-from-osx)
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq delete-by-moving-to-trash t) ;; https://www.emacswiki.org/emacs/SystemTrash
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash."
    (call-process (executable-find "trash")
		  nil 0 nil
		  file))
  (defvar locate-command "mdfind")
  (message "Completed inits for macOS")
  )

(defun jwd/emacs-inits ()
  "Perform all my startup initializations."
  (interactive)
  (auto-compression-mode t)
  (when (display-graphic-p)
    (set-scroll-bar-mode 'right))
  (global-auto-revert-mode 1)      ; before-revert-hook will save a copy of externally reverted
  (transient-mark-mode t)          ; default emacs?
  (cond (is-macos
         (jwd/mac-init)
         (setq command-line-default-directory "~")
         )
        (t (message "no emacs-inits for non macos")))
  (message "Completed jwd/emacs-inits")
  )
(jwd/add-hook 'after-init-hook 'jwd/emacs-inits t)

(eval-when-compile                      ; hush byte-compile warnings
  (require 'server)
  (declare-function server-running-p "server"))
(defun jwd/server-start ()
  "Enable access from emacsclient."
  (unless (server-running-p)
    ;; back in the day this was desirable
    ;;(setq server-socket-dir (format "/tmp/emacs_socket_for_%s" (user-login-name)))
    (server-start)
    (message "server started")))
(add-hook 'after-init-hook 'jwd/server-start t)

(defun jwd/envrc ()
  "Make buffer-local environment match current file."
  ;; https://github.com/purcell/envrc
  ;; "It's probably wise to do this late in your startup sequence."
  (use-package envrc ;; a better direnv / (direnv-mode)
    ;;:disabled
    :ensure t
    :config
    (envrc-global-mode)))
(jwd/add-hook 'after-init-hook 'jwd/envrc t)

;;; end initializations:
(defun jwd/initializations ()
  "Perform final initializations."
  (setq-default initial-buffer-choice
                (lambda ()
                  "Instead of startup screen find shell buffer and do other cleanup."
                  (let ((shell-buf (jwd/find-shell)))
                    (delete-other-windows)
                    (jwd/resync-directories)
                    (get-buffer shell-buf)
                    )))
  (if (> 0 (length (getenv "LANG"))) ;; avoid Gnome-isms
      (setenv "LANG" "C"))
  (message "Initialized for host named %s" (system-name))
  )

;; Kick things off for me and others
(cond ((not (string-match my-login my-login-name))
       ;; let borrowers know what they are in for
       (let ((buf "*Warnings*"))
         (if (buffer-live-p buf) (kill-buffer buf))
         (switch-to-buffer-other-window buf)
         (insert jwd/warning)
         (set-buffer-modified-p nil)
         (read-only-mode)
         (goto-char 0)
         (delete-other-windows)))
      (t
       (jwd/initializations)
       (message "%s loaded" (or load-file-name buffer-file-name))))

;;; batch-byte-compile in a subshell to avoid current session interference, using this emacs' path
;;; (shell-command (concat (expand-file-name invocation-name invocation-directory) " -batch -q -f batch-byte-compile " (buffer-file-name)))
(provide 'emacs)
;;; emacs.el ends here
