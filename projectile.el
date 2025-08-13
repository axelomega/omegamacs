;;; -*- lexical-binding: t -*-

;; Projectile mode
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  ;; Use git ls-files when in git repo, fallback to find for non-git projects
  (setq projectile-generic-command 
        (if my-settings-projectile-generic-command
            my-settings-projectile-generic-command
            "git ls-files -zco --exclude-standard 2>/dev/null || find . -type f -print0"))
  (setq projectile-sort-order 'recently-active)
  
  ;; Use vertico for projectile completion
  (setq projectile-completion-system 'default))

(use-package treemacs-projectile
  :ensure t)
