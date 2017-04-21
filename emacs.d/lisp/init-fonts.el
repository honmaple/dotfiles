;;; Character sets


(use-package fontawesome
  :ensure t)

;; Changing font sizes
(use-package default-text-scale
  :ensure t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("<C-wheel-up>" . default-text-scale-increase)
         ("<C-wheel-down>" . default-text-scale-decrease)
         ))

(defun maple/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))


(defun maple/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (maple/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (maple/utf8-locale-p (getenv "LC_ALL"))
      (maple/utf8-locale-p (getenv "LC_CTYPE"))
      (maple/utf8-locale-p (getenv "LANG"))))

(when (or window-system (maple/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

;; (set-language-environment 'Chinese-GB)

(provide 'init-fonts)
