;;; -*- lexical-binding: t -*-

;; Projectile mode
(use-package projectile
  :ensure t
  :defer t
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

;; Custom function to copy current file path relative to project root to clipboard
(defun my-copy-file-path-from-project-root ()
  "Copy the current file's path relative to project root to clipboard."
  (interactive)
  (when buffer-file-name
    (let* ((project-root (or (projectile-project-root)
                             (vc-root-dir)
                             default-directory))
           (relative-path (file-relative-name buffer-file-name project-root)))
      (kill-new relative-path)
      (if (fboundp 'gui-set-selection)
          (gui-set-selection 'CLIPBOARD relative-path))
      (message "Copied to clipboard: %s" relative-path))))

;; Bind to a convenient key combination
(global-set-key (kbd "C-c f p") 'my-copy-file-path-from-project-root)

(use-package treemacs-projectile
  :ensure t
  :defer t
  :after (treemacs projectile))
