(use-package package
  :config
  ;; Add all major package archives
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives
               '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  
  ;; Initialize packages if not already done
  ;; Only refresh if auto update environment variable is set
  (unless package-archive-contents
    (when (getenv "EMACS_PACKAGE_UPDATE_ENABLE")
      (package-refresh-contents)))
  
  ;; Install use-package if not present
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
    
  ;; Performance optimizations
  (setq package-enable-at-startup nil
        package-quickstart t))

;; Auto update packages - only when environment variable is set
(when (getenv "EMACS_PACKAGE_UPDATE_ENABLE")
  (use-package auto-package-update
    :ensure t
    :config
    (setq auto-package-update-delete-old-versions t
          auto-package-update-interval 7)
    (auto-package-update-maybe)))
