;;; -*- lexical-binding: t -*-

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
