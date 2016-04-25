; (require-package 'rst-mode)
(require-package 'ox-rst)
; (setq auto-mode-alist
; (append '(("\\.rst\\'" . rst-mode)
; ("\\.rest\\'" . rst-mode)) auto-mode-alist))

(eval-after-load 'rst-mode
                 (require 'ox-rst)
                 )
(provide 'init-rst)
