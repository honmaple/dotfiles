;;; Character sets

(defcustom sanityinc/force-default-font-for-symbols nil
           "When non-nil, force Emacs to use your default font for symbols."
           :type 'boolean)

(defun sanityinc/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when sanityinc/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'sanityinc/maybe-use-default-font-for-symbols)

;;; Changing font sizes

(require-package 'default-text-scale)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)


(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
  This is helpful for writeroom-mode, in particular."
  (if visual-fill-column-mode
    (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)

;; 中英文表格对齐
;; (set-frame-font "DejaVu Sans Mono 11")
; (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12")
; (set-default-font "Dejavu Sans Mono 11")
; (if (and (fboundp 'daemonp) (daemonp))
  ; (add-hook 'after-make-frame-functions
            ; (lambda (frame)
              ; (with-selected-frame frame
                                   ; (set-fontset-font "fontset-default"
                                                     ; 'unicode "WenQuanyi Micro Hei Mono 12"))))
  ; (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12"))
;; (default-text-scale-increase)
; (setq face-font-rescale-alist '((""Dejavu Sans Mono" . 1.0) ("WenQuanyi Micro Hei Mono"  . 1.23)))


(provide 'init-fonts)
