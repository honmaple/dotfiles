; (require-package 'powerline)
; (require 'powerline)
; (powerline-vim-theme)
(require-package 'spaceline)
(require-package 'eyebrowse)
(require 'spaceline-config)
(spaceline-emacs-theme)
; (spaceline-spacemacs-theme)
(require 'eyebrowse)
(eyebrowse-mode)
;; Hide jiggle-mode lighter from mode line
; (diminish 'jiggle-mode)
;; Replace abbrev-mode lighter with "Abv"
; (diminish 'flycheck-mode "ⓢ")
; (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)


(provide 'init-modeline)
