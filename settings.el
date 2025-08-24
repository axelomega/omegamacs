;;; -*- lexical-binding: t -*-

(setq visible-bell t)
(setq tab-width 4)

;;No menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "gray15")
(global-set-key (kbd "C-x C-<backspace>") 'delete-trailing-whitespace)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;;Font
;(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-*-*-*-*")
;(add-to-list 'default-frame-alist '(font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-*-*-*-*"))

;; Color settings
(defun my--set-colors ()
  (set-background-color "gray20")
  (set-cursor-color "cyan")
  (set-foreground-color "gray80"))

(defun my--set-frame-colors (frame)
  (select-frame frame)
  (set-background-color "gray20")
  (set-cursor-color "cyan")
  (set-foreground-color "gray80"))

(if (daemonp)
	(add-hook 'after-make-frame-functions #'my--set-frame-colors)
  (my--set-colors))

;;Use space not tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

;;We have wide screens now
(setq fill-column 200)

;;Modern line number showing
(global-set-key (kbd "C-<f5>") 'display-line-numbers-mode)

;;Show the time
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(defface hl-line '((t (:background "Gray10")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t) ; turn it on for all modes by default

;; Zoom text
(defun my-text-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

(global-set-key (kbd "C-+")      #'(lambda nil (interactive) (my-text-zoom 1)))
(global-set-key [C-kp-add]       #'(lambda nil (interactive) (my-text-zoom 1)))
(global-set-key (kbd "C--")      #'(lambda nil (interactive) (my-text-zoom -1)))
(global-set-key [C-kp-subtract]  #'(lambda nil (interactive) (my-text-zoom -1)))

(defhydra hydra-zoom (:color pink :hint nil)
  "
 Zoom: _+_ in   _-_ out   _0_ reset   _q_ quit
"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-set 0))
  ("q" nil :exit t))

(global-set-key (kbd "C-c z") 'hydra-zoom/body)

;;Smerge
(add-hook 'smerge-mode-hook
   #'(lambda ()
       (set-face-background 'smerge-refined-change "dark magenta")))

;; Keep backups in a dedicated folder

(let ((backup-dir (my-user-emacs-subdirectory-local "backups")))
  (setq backup-directory-alist (list (cons "." backup-dir))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        ;; Security improvements
        backup-by-copying-when-linked t
        backup-by-copying-when-mismatch t
        ;; Auto-save improvements
        auto-save-file-name-transforms (list (list ".*" backup-dir t))
        auto-save-timeout 20
        auto-save-interval 200))

;; Brows things in emacs
(setq browse-url-browser-function 'eww-browse-url)

;;; Trim whitespaces
;;(use-package ws-butler
;;  :ensure t
;;  :config
;;  (ws-butler-global-mode 1))

;; History
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Security settings
(setq auth-source-save-behavior nil  ; Don't save auth info automatically
      network-security-level 'high   ; High security for network connections
      gnutls-verify-error t          ; Verify TLS certificates
      gnutls-min-prime-bits 3072)    ; Strong TLS

;; Prevent accidental deletion of important files
(setq delete-by-moving-to-trash t)

;; Disable risky local variables
(setq enable-local-variables :safe
      enable-local-eval nil)

(use-package tab-bar
  :config
  (setq tab-bar-new-tab-choice "*scratch*"
        tab-bar-close-button-show nil
        tab-bar-show 1)
  :bind (("C-x t t" . tab-bar-new-tab)
         ("C-x t k" . tab-bar-close-tab)
         ("C-x t <right>" . tab-bar-switch-to-next-tab)
         ("C-x t <left>" . tab-bar-switch-to-prev-tab)))

;; Hydras will be installed in the .el files that relate to the function of the hydra
(use-package hydra
  :ensure t
  :config
  (defun my-list-hydras ()
    "List all defined hydras (functions ending with /body) in a *Hydras* buffer.
Shows the hydra name, any bound keys, and the docstring (first line)."
    (interactive)
    (let ((buf (get-buffer-create "*Hydras*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert (propertize (make-string 80 ?-) 'face 'font-lock-comment-face) "\n")
        (insert (propertize (format "%-30s %-15s %s\n" "Hydra" "Key(s)" "Docstring") 'face 'font-lock-keyword-face))
        (insert (propertize (make-string 80 ?-) 'face 'font-lock-comment-face) "\n")
        (mapatoms
         (lambda (sym)
           (when (and (fboundp sym)
                      (string-match-p "hydra-.*?/body" (symbol-name sym)))
             (let* ((name (propertize (symbol-name sym) 'face 'font-lock-function-name-face))
                    ;; collect all keybindings for this hydra
                    (keys (propertize (mapconcat #'key-description
                                                 (where-is-internal sym)
                                                 ", ")
                                      'face 'font-lock-string-face))
                    (doc (propertize (or (ignore-errors
                                           (when-let* ((d (documentation sym)))
                                             (car (split-string d "\n"))))
                                         "")
                                     'face 'font-lock-doc-face)))
               (insert (format "%-40s %-20s %s\n"
                               name (or keys "") doc))))))
        (insert "\n")
        (insert (propertize (make-string 80 ?-) 'face 'font-lock-comment-face) "\n")
        (insert (propertize (format "%-40s %s\n" "Hydra" "Docstring") 'face 'font-lock-keyword-face))
        (insert (propertize (make-string 80 ?-) 'face 'font-lock-comment-face))
        (insert "\n")
        (mapatoms
         (lambda (sym)
           (when (and (fboundp sym)
                      (string-match-p "hydra-.*?/body" (symbol-name sym)))
             (let ((name (propertize (symbol-name sym) 'face 'font-lock-function-name-face))
                   (doc (propertize (or (ignore-errors (documentation sym))
                                        "")
                                    'face 'font-lock-doc-face)))
               (insert (format "%-40s %s\n" name doc)))))))
      (pop-to-buffer buf)
      (special-mode))))
