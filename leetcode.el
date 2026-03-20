;;; -*- lexical-binding: t -*-

;; LeetCode client for Emacs
(use-package leetcode
  :ensure t
  :defer omegamacs-enable-lazy-loading
  :bind ("C-c C-l" . leetcode)
  :config
  ;; Set preferred programming language
  (setq leetcode-prefer-language "cpp")

  ;; Set preferred SQL dialect
  (setq leetcode-prefer-sql "mysql")

  ;; Directory for saving solutions
  (setq leetcode-directory "~/leetcode")

  ;; Enable automatic solution saving
  (setq leetcode-save-solutions t)

  ;; Toggle tag display in problem list
  (setq leetcode-prefer-tag-display t))
