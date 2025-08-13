;;; Omegamacs initialization file
;;; Copy this file to ~/.emacs.d/init.el
;;; This file loads the main configuration from a configurable directory

;; Load local settings first to get configuration directory
(let ((local-settings-file (expand-file-name "local_settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings-file)
    (load local-settings-file)))

;; Load main configuration from the configured directory
(let ((config-dir (or (and (boundp 'my-emacs-config-dir) my-emacs-config-dir)
                      "~/omegamacs")))  ; Default fallback
  (load-file (expand-file-name "emacs_init.el" config-dir)))
