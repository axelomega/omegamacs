;;; -*- lexical-binding: t -*-

(defvar my--gc-cons-threshold gc-cons-threshold)
(defvar my--gc-cons-percentage gc-cons-percentage)
(defvar my--file-name-handler-alist file-name-handler-alist)

;; Maximize memory during startup for better performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; Enable native compilation if available
(when (featurep 'native-compile)
  (setq native-comp-speed 2)  ; 0=no optimization, 1=light, 2=max optimization
  (setq native-comp-async-report-warnings-errors nil)  ; Reduce noise
  (setq native-comp-jit-compilation t))  ; Enable JIT compilation

;; Restore normal values after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my--gc-cons-threshold
                  gc-cons-percentage my--gc-cons-percentage
                  file-name-handler-alist my--file-name-handler-alist)
            ;; Optional: run gc after startup
            (garbage-collect)))

(defun my-get-fullpath (file-relative-path)
  "Return the full path of FILE-RELATIVE-PATH, relative to the configuration directory."
  (let ((config-dir (or (and (boundp 'my-emacs-config-dir) my-emacs-config-dir)
                        (file-name-directory (or load-file-name buffer-file-name)))))
    (expand-file-name file-relative-path config-dir)))

(defun my-user-emacs-subdirectory-local (subdir)
  "Given a subdirectory name, return the path to the disk local user directory; if the path does not exist, create it."
  (let ((dir (expand-file-name subdir my-user-emacs-directory-local)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

;; Check for minimal config argument
(defvar my-minimal-config (member "--minimal" command-line-args)
  "When non-nil, load only essential configuration.")

;; Remove the argument so it doesn't cause issues
(setq command-line-args (delete "--minimal" command-line-args))

(if my-minimal-config
    (load (my-get-fullpath "minimal"))
  (progn
    ;; Full configuration
    (load (my-get-fullpath "packages"))
    (load (my-get-fullpath "settings"))
    (load (my-get-fullpath "hydra"))
    (load (my-get-fullpath "flycheck"))
    (load (my-get-fullpath "company"))
    (load (my-get-fullpath "completion"))
    (load (my-get-fullpath "frame_buffer_handling"))
    (load (my-get-fullpath "programming"))
    (load (my-get-fullpath "languages/cpp"))
    (load (my-get-fullpath "languages/python"))
    (load (my-get-fullpath "languages/verilog"))
    (load (my-get-fullpath "languages/latex"))
    (load (my-get-fullpath "languages/xml"))
    (load (my-get-fullpath "languages/elisp"))
    (load (my-get-fullpath "compilation"))
    (load (my-get-fullpath "projectile"))
    (load (my-get-fullpath "magit"))
    (load (my-get-fullpath "tramp"))
    (load (my-get-fullpath "development"))
    (load (my-get-fullpath "org"))
    ;; Load Copilot configuration based on user preference
    (when (boundp 'my-copilot-config)
      (cond
       ((eq my-copilot-config 'setup)
        (load (my-get-fullpath "copilot/copilot-setup")))
       ((eq my-copilot-config 'full)
        (load (my-get-fullpath "copilot/copilot")))))
    ))

(setq fill-column 200)

;; Separate custom file for cleaner configuration
    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Convenient reload function
(defun my-reload-config ()
  "Reload the entire Emacs configuration."
  (interactive)
  (load-file (my-get-fullpath "emacs_init.el"))
  (message "Configuration reloaded!"))
