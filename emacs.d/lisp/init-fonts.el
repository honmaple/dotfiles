;;; Character sets


;;; Changing font sizes


(require-package 'default-text-scale)
(use-package default-text-scale
  :config
  (progn
    (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
    (global-set-key (kbd "C-M--") 'default-text-scale-decrease)
    )
  )

;; 中英文表格对齐
;; (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12")
;; (set-default-font "Dejavu Sans Mono 10")
;; (if (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (set-fontset-font "fontset-default"
;;                                     'unicode "WenQuanyi Micro Hei Mono 11"))))
;;   (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 11"))
;; (setq face-font-rescale-alist '(("Dejavu Sans Mono" . 1.1) ("WenQuanyi Micro Hei Mono"  . 1.32)))
;; (setq face-font-rescale-alist '(("Dejavu Sans Mono" . 1.15) ("WenQuanyi Micro Hei Mono"  . 1.38)))
;; (setq face-font-rescale-alist '(("WenQuanyi Micro Hei Mono"  . 1.2)))

(provide 'init-fonts)
