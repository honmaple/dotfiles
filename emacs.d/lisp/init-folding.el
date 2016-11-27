;;; hideshow

(use-package hideshow
  :diminish hs-minor-mode
  :init (add-hook 'prog-mode-hook #'hs-minor-mode)
  :config
  (progn
    (defvar hs-headline-max-len 30 "*Maximum length of `hs-headline' to display.")
    (defvar hs-overlay-map (make-sparse-keymap) "Keymap for hs minor mode overlay.")

    (defun hs-display-headline ()
      (let* ((len (length hs-headline))
             (headline hs-headline)
             (postfix ""))
        (when (>= len hs-headline-max-len)
          (setq postfix "...")
          (setq headline (substring hs-headline 0 hs-headline-max-len)))
        (if hs-headline (concat headline postfix " ") "")))

    (defun hs-abstract-overlay (ov)
      (let* ((start (overlay-start ov))
             (end (overlay-end ov))
             (str (format "<%d lines>" (count-lines start end))) text)
        (setq text (propertize str 'face 'hs-block-flag-face 'help-echo (buffer-substring (1+ start) end)))
        (overlay-put ov 'display text)
        (overlay-put ov 'pointer 'hand)
        (overlay-put ov 'keymap hs-overlay-map)))

    (setq hs-isearch-open t)
    (setq-default mode-line-format
                  (append '((:eval (hs-display-headline))) mode-line-format))
    (setq hs-set-up-overlay 'hs-abstract-overlay)
    (defadvice goto-line (after expand-after-goto-line activate compile)
      (save-excursion (hs-show-block)))

    (defadvice find-tag (after expand-after-find-tag activate compile)
      (save-excursion (hs-show-block)))

    ))

(provide 'init-folding)
