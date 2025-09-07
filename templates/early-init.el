;; This can be good to set to a disk local location if your ${HOME} is mounted via NFS to speed up certain file operations
;; For example:
;; (setq omegamacs-user-emacs-directory-local "/path/to/local/disk/.emacs.d.local")
(setq omegamacs-user-emacs-directory-local user-emacs-directory)

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "eln-cache/" omegamacs-user-emacs-directory-local))))
