;;; -*- lexical-binding: t -*-

;; Compilation
(use-package compile
  :bind (("C-q" . 'compile))
  :config
  (set-variable 'compilation-max-output-line-length nil)
  ;; Quartus
  ;;(add-to-list 'compilation-error-regexp-alist 'quartus-error)
  ;;(add-to-list 'compilation-error-regexp-alist-alist
  ;;             '(quartus-error
  ;;               "^Error ([0-9]*):.*File:[[:space:]]*\\([[:alnum:]/_.]*\\)[[:space:]]*Line:[[:space:]]*\\([[:digit:]]*\\)"
  ;;               1 2 nil 2))
  ;;
  ;;(add-to-list 'compilation-error-regexp-alist 'quartus-warning)
  ;;(add-to-list 'compilation-error-regexp-alist-alist
  ;;             '(quartus-warning
  ;;               "^Warning ([0-9]*):.*File:[[:space:]]*\\([[:alnum:]/_.]*\\)[[:space:]]*Line:[[:space:]]*\\([[:digit:]]*\\)"
  ;;               1 2 nil 1))
  ;;
  ;;;; Riviera-PRO
  ;;(add-to-list 'compilation-error-regexp-alist 'riviera-error)
  ;;(add-to-list 'compilation-error-regexp-alist-alist
  ;;             '(riviera-error
  ;;               "^ERROR.*\"\\([[:alnum:]./_]+\\)\"[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)$"
  ;;               1 2 3 2))
  ;;
  ;;(add-to-list 'compilation-error-regexp-alist 'riviera-warning)
  ;;(add-to-list 'compilation-error-regexp-alist-alist
  ;;             '(riviera-warning
  ;;               "^WARNING.*\"\\([[:alnum:]./_]+\\)\"[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)$"
  ;;               1 2 3 1))

  ;; Verilator
  (add-to-list 'compilation-error-regexp-alist 'verilator-error)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(verilator-error
                 "^%Error:[[:space:]]*\\([[:alnum:]/_.]+\\):\\([[:digit:]]+\\)$"
                 1 2 nil 2))

  (add-to-list 'compilation-error-regexp-alist 'verilator-warning)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(verilator-warning
                 "^%Warning[[:upper:]-]*:[[:space:]]*\\([[:alnum:]/_.]+\\):\\([[:digit:]]+\\)$"
                 1 2 nil 1))

  ;; Vivado
  (add-to-list 'compilation-error-regexp-alist 'vivado-error)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(vivado-error
                 "^ERROR:.*\\[\\([/_\\.[:alnum:]]*\\):\\([[:digit:]]*\\)$"
                 1 2 nil 2))

  (add-to-list 'compilation-error-regexp-alist 'vivado-warning)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(vivado-warning
                 "^WARNING:.*\\[\\([/_\\.[:alnum:]]*\\):\\([[:digit:]]*\\)$"
                 1 2 nil 1))

  (add-to-list 'compilation-error-regexp-alist 'vivado-info)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(vivado-info
                 "^INFO:.*\\[\\([/_\\.[:alnum:]]*\\):\\([[:digit:]]*\\)$"
                 1 2 nil 0)))

(defun omegamacs--colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook 'omegamacs--colorize-compilation-buffer))

(setq compilation-hidden-output '("^ninja: Entering[^\n]+\n"))

;; Symlink resolution for compilation error navigation
(defun omegamacs--resolve-symlink-in-compilation-error (data)
  "Resolve symlinks in compilation error file paths.
This function is used as advice for `compilation-find-file' to ensure
that when jumping to compilation errors, symlinked file paths are
resolved to their real paths."
  (when (and data (car data))
    (let ((filename (car data)))
      (when (and (stringp filename) (file-exists-p filename))
        (let ((real-path (file-truename filename)))
          (when (not (string= filename real-path))
            (setcar data real-path))))))
  data)

(defun omegamacs--compilation-find-file-resolve-symlinks (orig-fun marker filename directory &rest formats)
  "Advice for `compilation-find-file' to resolve symlinks to real paths.
This ensures that when jumping to compilation errors, Emacs opens the
actual file rather than the symlink, providing consistent file handling."
  (let ((result (apply orig-fun marker filename directory formats)))
    (when (and result (buffer-file-name result))
      (let ((real-path (file-truename (buffer-file-name result))))
        (when (not (string= (buffer-file-name result) real-path))
          ;; If the file we found is a symlink, open the real file instead
          (let ((real-buffer (find-file-noselect real-path)))
            (setq result real-buffer)))))
    result))

;; Apply the advice to resolve symlinks during compilation error navigation
(advice-add 'compilation-find-file :around #'omegamacs--compilation-find-file-resolve-symlinks)

;; Alternative approach: resolve symlinks in next-error navigation
(defun omegamacs--next-error-resolve-symlinks ()
  "Hook to resolve symlinks when navigating to compilation errors.
This ensures consistent file handling when using next-error/previous-error."
  (when (and (buffer-file-name)
             (file-symlink-p (buffer-file-name)))
    (let ((real-path (file-truename (buffer-file-name))))
      (when (not (string= (buffer-file-name) real-path))
        ;; Switch to the real file if current file is a symlink
        (find-file real-path)))))

;; Add hook for next-error navigation
(add-hook 'next-error-hook #'omegamacs--next-error-resolve-symlinks)

;; Configuration to handle symlinks globally in find-file operations
(defun omegamacs--find-file-resolve-symlinks (filename &optional wildcards)
  "Resolve symlinks when opening files through find-file operations.
This provides consistent symlink resolution across all file opening operations."
  (when (and filename (stringp filename) (file-exists-p filename))
    (file-truename filename)))

;; Enhanced compilation error navigation with symlink awareness
(defun omegamacs-goto-next-error-resolve-symlinks ()
  "Navigate to next compilation error and resolve any symlinks."
  (interactive)
  (next-error)
  (omegamacs--next-error-resolve-symlinks))

(defun omegamacs-goto-previous-error-resolve-symlinks ()
  "Navigate to previous compilation error and resolve any symlinks."
  (interactive)
  (previous-error)
  (omegamacs--next-error-resolve-symlinks))

;; Optional keybindings for enhanced error navigation
;; (global-set-key (kbd "M-g n") #'omegamacs-goto-next-error-resolve-symlinks)
;; (global-set-key (kbd "M-g p") #'omegamacs-goto-previous-error-resolve-symlinks)

;; Helper function to determine shell history file
(defun omegamacs--get-shell-history-file ()
  "Get the appropriate shell history file path."
  (if (boundp 'omegamacs-compile-mode-shell-history-file)
      omegamacs-compile-mode-shell-history-file
    (let ((shell (file-name-nondirectory (or (getenv "SHELL") "/bin/bash"))))
      (cond
       ((string-match "zsh" shell) (expand-file-name ".zsh_history" "~"))
       ((string-match "fish" shell) (expand-file-name ".local/share/fish/fish_history" "~"))
       ((string-match "tcsh\\|csh" shell) (expand-file-name ".history" "~"))
       (t (expand-file-name ".bash_history" "~"))))))

;; Read shell history into compile command history
(defun omegamacs--load-shell-history-to-compile ()
  "Load shell history into compilation command history."
  (let ((history-size (if (boundp 'omegamacs-compile-mode-shell-history-size)
                          omegamacs-compile-mode-shell-history-size
                        nil))
        (histfile (omegamacs--get-shell-history-file)))
    (when (and history-size histfile (file-readable-p histfile))
      (with-temp-buffer
        (insert-file-contents histfile)
        (let* ((all-commands (reverse (split-string (buffer-string) "\n" t)))
               (shell-commands (if (zerop history-size)
                                 all-commands
                               (seq-take all-commands history-size))))
          (setq compile-history
                (delete-dups (append shell-commands compile-history))))))))

;; Load bash history when compile command is read
(defun omegamacs--compilation-read-command-with-history (orig-fun &rest args)
   "Advice to load shell history before reading compile command."
  (omegamacs--load-shell-history-to-compile)
  (apply orig-fun args))

(advice-add 'compilation-read-command :around #'omegamacs--compilation-read-command-with-history)
