;;; -*- lexical-binding: t -*-

;; Load only essential settings
(load (omegamacs-get-fullpath "theme"))
(load (omegamacs-get-fullpath "settings"))

(windmove-default-keybindings)

(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

;; Enable electric-pair-mode based on omegamacs setting
(when omegamacs-parenthesis-autocomplete-enable
  (electric-pair-mode 1))
(show-paren-mode 1)
(global-auto-revert-mode 1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
