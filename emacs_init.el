;;; -*- lexical-binding: t -*-

(defvar omegamacs--gc-cons-threshold gc-cons-threshold)
(defvar omegamacs--gc-cons-percentage gc-cons-percentage)
(defvar omegamacs--file-name-handler-alist file-name-handler-alist)

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
            (setq gc-cons-threshold omegamacs--gc-cons-threshold
                  gc-cons-percentage omegamacs--gc-cons-percentage
                  file-name-handler-alist omegamacs--file-name-handler-alist)
            ;; Optional: run gc after startup
            (garbage-collect)))

(defun omegamacs-get-fullpath (file-relative-path)
  "Return the full path of FILE-RELATIVE-PATH, relative to the configuration directory."
  (let ((config-dir (or (and (boundp 'omegamacs-emacs-config-dir) omegamacs-emacs-config-dir)
                        (file-name-directory (or load-file-name buffer-file-name)))))
    (expand-file-name file-relative-path config-dir)))

(defun omegamacs-user-emacs-subdirectory-local (subdir)
  "Given a subdirectory name, return the path to the disk local user directory; if the path does not exist, create it."
  (let ((dir (expand-file-name subdir omegamacs-user-emacs-directory-local)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun omegamacs-user-emacs-file-local (file-name &optional subdir)
  "Given name, return the path to the disk local user directory; if the path does not exist, create it."
  (let ((dir (if subdir
                 (omegamacs-user-emacs-subdirectory-local subdir)
               omegamacs-user-emacs-directory-local)))
    (let ((fn (expand-file-name file-name dir)))
      (unless (file-exists-p fn)
        (with-temp-buffer
          (write-file fn)))
      fn)))

(defun omegamacs-user-emacs-subdirectory (subdir)
  "Given a subdirectory name, return the path to the directorey directory; if the path does not exist, create it."
  (let ((dir (expand-file-name subdir user-emacs-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

;; Check for minimal config argument
(defvar omegamacs-minimal-config (member "--minimal" command-line-args)
  "When non-nil, load only essential configuration.")

;; Check for no-defer argument
(defvar omegamacs-enable-lazy-loading (not (member "--no-defer" command-line-args))
  "When non-nil, enable lazy loading with :defer. Set to nil for immediate loading of all packages.")

;; Remove the arguments so they don't cause issues
(setq command-line-args (delete "--minimal" command-line-args))
(setq command-line-args (delete "--no-defer" command-line-args))

;; Show startup mode information
(message "Omegamacs startup: %s mode, lazy loading %s" 
         (if omegamacs-minimal-config "minimal" "full")
         (if omegamacs-enable-lazy-loading "enabled" "disabled"))

(if omegamacs-minimal-config
    (load (omegamacs-get-fullpath "minimal"))
  (progn
    ;; Full configuration
    (load (omegamacs-get-fullpath "packages"))
    (load (omegamacs-get-fullpath "hydra"))
    (load (omegamacs-get-fullpath "settings"))
    (load (omegamacs-get-fullpath "flycheck"))
    (load (omegamacs-get-fullpath "company"))
    (load (omegamacs-get-fullpath "completion"))
    (load (omegamacs-get-fullpath "frame_buffer_handling"))
    (load (omegamacs-get-fullpath "programming"))
    (load (omegamacs-get-fullpath "languages/cpp"))
    (load (omegamacs-get-fullpath "languages/python"))
    (load (omegamacs-get-fullpath "languages/verilog"))
    (load (omegamacs-get-fullpath "languages/latex"))
    (load (omegamacs-get-fullpath "languages/xml"))
    (load (omegamacs-get-fullpath "languages/elisp"))
    (load (omegamacs-get-fullpath "languages/yaml"))
    (load (omegamacs-get-fullpath "compilation"))
    (load (omegamacs-get-fullpath "projectile"))
    (load (omegamacs-get-fullpath "magit"))
    (load (omegamacs-get-fullpath "tramp"))
    (load (omegamacs-get-fullpath "development"))
    (load (omegamacs-get-fullpath "org"))
    ;; Load Copilot configuration based on user preference
    (when (boundp 'omegamacs-copilot-config)
      (cond
       ((eq omegamacs-copilot-config 'setup)
        (load (omegamacs-get-fullpath "copilot/copilot-setup")))
       ((eq omegamacs-copilot-config 'full)
        (load (omegamacs-get-fullpath "copilot/copilot")))))
    ))

(setq fill-column 140)

;; Separate custom file for cleaner configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Convenient reload function
(defun omegamacs-reload-config ()
  "Reload the entire Emacs configuration."
  (interactive)
  (load-file (omegamacs-get-fullpath "emacs_init.el"))
  (message "Configuration reloaded!"))
