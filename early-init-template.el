;; This can be good to set to a disk local location if your ${HOME} is mounted via NFS to speed up certain file operations
;(setq my-user-emacs-directory-local user-emacs-directory)
(setq my-user-emacs-directory-local "~/.emacs.d.local/")

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "eln-cache/" my-user-emacs-directory-local))))
