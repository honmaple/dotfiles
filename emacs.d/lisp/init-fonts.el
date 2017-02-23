;;; Character sets


;;; Changing font sizes

(require-package 'fontawesome)
(require-package 'default-text-scale)

(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("<C-wheel-up>" . default-text-scale-increase)
         ("<C-wheel-down>" . default-text-scale-decrease)
         ))

;; 中英文表格对齐
(defvar emacs-english-font "DejaVu Sans Mono"
  "The font name of English.")
(defvar emacs-cjk-font "WenQuanYi Micro Hei Mono"
  "The font name for CJK.")
(defvar emacs-font-size-pair '(15 . 18)
  "Default font size pair for (english . chinese)")

(defvar emacs-font-size-pair-list
  '(( 5 .  6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
  "This list is used to store matching (englis . chinese) font-size.")
(defun font-exist-p (fontname)
  "Test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun maple/set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."
  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))
;; (add-hook 'ctbl:table-mode-hook
;;           (maple/set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))

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
