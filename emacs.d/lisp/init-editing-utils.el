;; (require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
  blink-cursor-interval 0.4
  bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
  buffers-menu-max-size 30
  case-fold-search t
  column-number-mode t
  delete-selection-mode t
  ediff-split-window-function 'split-window-horizontally
  ediff-window-setup-function 'ediff-setup-windows-plain
  indent-tabs-mode nil
  make-backup-files nil
  mouse-yank-at-point t
  save-interprogram-paste-before-kill t
  scroll-preserve-screen-position 'always
  set-mark-command-repeat-pop t
  tooltip-delay 1.5
  truncate-lines nil
  truncate-partial-width-windows nil)

(global-auto-revert-mode) ;; 修改外部文件自动载入
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)



;;; Newline behaviour

; (global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(when (eval-when-compile (string< "24.3.1" emacs-version))
  ;; https://github.com/purcell/emacs.d/issues/138
  (after-load 'subword
              (diminish 'subword-mode)))



(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))


(require-package 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist
      `(("." . ,(concat maple-cache-directory "undo"))))
; (unless (file-exists-p (concat maple-cache-directory "undo"))
  ; (make-directory (concat maple-cache-directory "undo")))
(diminish 'undo-tree-mode)


(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
(after-load 'highlight-symbol
            (diminish 'highlight-symbol-mode)
            (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
                       "Suppress symbol highlighting while isearching."
                       (unless (or isearch-mode
                                   (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
                         ad-do-it)))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)


;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


(defun sanityinc/open-line-with-reindent (n) ;; 跳转到原来的位置
  "A version of `open-line' which reindents the start and end positions.
  If there is a fill prefix and/or a `left-margin', insert them
  on the new line if the line would have been blank.
  With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
           (cond ((bolp)
                  (if do-left-margin (indent-to (current-left-margin)))
                  (if do-fill-prefix (insert-and-inherit fill-prefix))))
           (forward-line 1)
           (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)


(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r"))
(guide-key-mode 1)
(diminish 'guide-key-mode)


(provide 'init-editing-utils)
