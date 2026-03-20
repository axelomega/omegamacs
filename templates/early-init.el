;; Local data directory for Emacs (packages, cache, etc.)
;; Set this early for better performance, especially if ${HOME} is on a network filesystem
;; Default: user-emacs-directory (set in defaults.el)
;; Uncomment and modify to use a disk-local location:
;; (setq omegamacs-user-emacs-directory-local "/path/to/local/disk/.emacs.d.local")
;;
;; Using default:
(setq omegamacs-user-emacs-directory-local user-emacs-directory)

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "eln-cache/" omegamacs-user-emacs-directory-local))))
