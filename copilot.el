;;; -*- lexical-binding: t -*-
;;; copilot.el --- GitHub Copilot configuration

;; Check Emacs version before setting up Copilot
(if (version< emacs-version "30.0")
    (message "GitHub Copilot configuration requires Emacs 30.0 or later. Current version: %s.
For older Emacs versions, you can configure Copilot with straight.el.
Please see: https://github.com/copilot-emacs/copilot.el/blob/main/README.md
Skipping Copilot setup." emacs-version)
  ;; GitHub Copilot support for Emacs 30+ - minimal configuration
  (use-package copilot
    :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest)
    :ensure t
    :hook (prog-mode . copilot-mode)))
