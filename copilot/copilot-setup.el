;;; -*- lexical-binding: t -*-
;;; copilot-setup.el --- GitHub Copilot configuration
;;; simplest possible settings to get started, once started switch to the more complete setup in copilot.el
;;; https://github.com/copilot-emacs/copilot.el/blob/main/README.md

;; Check Emacs version before setting up Copilot
(if (version< emacs-version "30.0")
    (message "GitHub Copilot configuration requires Emacs 30.0 or later. Current version: %s.
For older Emacs versions, you can configure Copilot with straight.el.
Please see: https://github.com/copilot-emacs/copilot.el/blob/main/README.md
Skipping Copilot setup." emacs-version)
  ;; GitHub Copilot support for Emacs 30+ - minimal configuration
  (use-package copilot
    :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")))
