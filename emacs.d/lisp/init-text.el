(use-package markdown-mode
  :config
  (use-package org-table
    :ensure nil
    :diminish orgtbl-mode
    :hook (markdown-mode . orgtbl-mode)
    :config
    (when (display-graphic-p) (set-face-attribute 'org-table nil :font "Inconsolata 12")))

  (defun cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))
  (maple/add-hook 'markdown-mode-hook
    (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local))
  :bind
  (:map markdown-mode-map
        ([f5] . markdown-toggle-markup-hiding)))

(use-package markdown-preview-mode
  :after (markdown-mode)
  :commands (markdown-preview-mode)
  :load-path "site-lisp/markdown-preview")

(use-package company-english-helper
  :load-path "site-lisp/company-english-helper"
  :functions maple/company-backend
  :commands (company-english-helper-search)
  :init
  (maple/company-backend '(org-mode-hook markdown-mode-hook) 'company-english-helper-search t)
  :setq
  (:mode org-mode
         company-tooltip-align-annotations nil)
  (:mode markdown-mode
         company-tooltip-align-annotations nil))

;; (use-package olivetti
;;   :defer t
;;   :hook (org-mode . olivetti-mode))

(use-package markdown-toc)
(use-package yaml-mode)
(use-package vimrc-mode)
(use-package json-mode)

(provide 'init-text)
