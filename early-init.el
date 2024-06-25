;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;;; Code:

(when (numberp (string-match "darwin.*NS appkit-DISABLED" (emacs-version)))
  (add-to-list 'initial-frame-alist '(undecorated . t))
  ;;(add-to-list 'initial-frame-alist '(undecorated-round . t)))
  (add-to-list 'initial-frame-alist '(drag-internal-border . 1))
  (add-to-list 'initial-frame-alist '(internal-border-width . 5)))
  
(add-to-list 'initial-frame-alist '(width . 100))
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

(setq default-frame-alist (copy-sequence initial-frame-alist))
;; place the initial one consistently
(add-to-list 'initial-frame-alist '(left . 1))
(add-to-list 'initial-frame-alist '(top . 1))

(message "early-init completed")
(provide 'early-init)
;;; early-init.el ends here
