;;; Character sets


(use-package fontawesome
  :demand t
  :load-path "site-lisp/fontawesome")

;; Changing font sizes
(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("<C-wheel-up>" . default-text-scale-increase)
         ("<C-wheel-down>" . default-text-scale-decrease)
         ))


(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if maple-system-is-mswindows 'utf-16-le 'utf-8))
(prefer-coding-system 'utf-8)

;; (set-language-environment 'Chinese-GB)

(provide 'init-fonts)
