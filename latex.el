;; LaTeX configuration with AUCTeX and lsp-mode

;; AUCTeX - The premier LaTeX mode for Emacs
(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  ;; Basic AUCTeX settings
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-master nil
        TeX-PDF-mode t)
  
  ;; Enable document parsing
  (setq-default TeX-master nil)
  
  ;; Enable source correlation for forward/inverse search
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)
  
  ;; LaTeX-specific settings
  (setq LaTeX-electric-left-right-brace t
        LaTeX-fill-break-at-separators nil)
  
  ;; Auto-fill mode for LaTeX
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  
  ;; Enable folding
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  
  ;; Better indentation
  (setq LaTeX-indent-level 2
        LaTeX-item-indent 0))

;; RefTeX for references, citations, and labels
(use-package reftex
  :ensure t
  :defer t
  :hook (LaTeX-mode . reftex-mode)
  :config
  ;; RefTeX settings
  (setq reftex-plug-into-AUCTeX t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-toc-split-windows-fraction 0.3))

;; Company completion for LaTeX
(use-package company-auctex
  :ensure t
  :after (company auctex)
  :config
  (company-auctex-init))

;; Math input improvements
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode)
  :config
  ;; Use TAB for cdlatex in LaTeX mode
  (setq cdlatex-use-dollar-to-ensure-math t))

;; LSP configuration for LaTeX with texlab (commented out until texlab is installed)
;; Uncomment the following block once you have texlab installed:
;; Install with: cargo install --git https://github.com/latex-lsp/texlab.git
;;
;;(with-eval-after-load 'lsp-mode
;;  ;; Add texlab server for LaTeX
;;  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))
;;  (add-to-list 'lsp-language-id-configuration '(LaTeX-mode . "latex"))
;;  
;;  ;; Register texlab server
;;  (lsp-register-client
;;   (make-lsp-client :new-connection (lsp-stdio-connection "texlab")
;;                    :major-modes '(latex-mode LaTeX-mode)
;;                    :server-id 'texlab))
;;  
;;  ;; Auto-start lsp for LaTeX if texlab is available
;;  (defun my-latex-lsp-ensure ()
;;    "Start lsp for LaTeX only if texlab is available."
;;    (when (executable-find "texlab")
;;      (lsp-deferred)))
;;  
;;  (add-hook 'LaTeX-mode-hook 'my-latex-lsp-ensure)
;;  (add-hook 'latex-mode-hook 'my-latex-lsp-ensure))

;; Preview support
(use-package preview
  :ensure nil  ; Part of AUCTeX
  :after auctex
  :config
  ;; Preview settings
  (setq preview-scale-function 1.2))

;; LaTeX-specific keybindings
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c C-q") 'LaTeX-fill-paragraph)
  (define-key LaTeX-mode-map (kbd "C-c C-e") 'LaTeX-environment)
  (define-key LaTeX-mode-map (kbd "C-c C-s") 'LaTeX-section)
  (define-key LaTeX-mode-map (kbd "C-c C-m") 'TeX-insert-macro))