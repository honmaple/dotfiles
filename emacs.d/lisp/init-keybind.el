;;; init-keybind.el --- Keybind configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

;; Author: lin.jiang <xiyang0807@gmail.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Keybind configurations.
;;

;;; Code:

(evil-leader/set-key
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fei" 'maple/open-init-file
  "fek" 'maple/open-keys-file
  "feg" 'maple/open-gtd-file
  "fo" 'maple/open-in-external-app
  "fE" 'maple/sudo-edit
  "fy" 'maple/show-and-copy-buffer-filename
  "fY" 'maple/copy-buffer-filename
  "fCd" 'maple/unix2dos
  "fCu" 'maple/dos2unix
  "fD" 'maple/delete-current-buffer-file
  "fR" 'maple/rename-current-buffer-file
  )

(evil-leader/set-key
  "cc" 'maple/comment-or-uncomment
  "cC" 'maple/copy-and-comment
  "u"  'undo-tree-visualize
  "'" 'shell-pop
  "=" 'maple/indent-buffer
  "se" 'evil-multiedit-match-all
  "ss" 'replace-regexp
  "sd" 'delete-matching-lines
  "sD" 'delete-non-matching-lines
  "sp" 'flyspell-correct-word-generic
  "sh" 'shell-command
  "Sg" 'maple/search-google
  "Sh" 'maple/search-github
  )


(evil-leader/set-key
  "el" 'flycheck-list-errors
  "ec" 'flycheck-clear
  "es" 'flycheck-select-checker
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
  )

;; 字体大小写
(evil-leader/set-key
  "vU" 'upcase-dwim
  "vu" 'downcase-dwim
  )

(evil-leader/set-key
  "h" 'help
  "rc" 'recentf-cleanup
  "rr" 'quickrun
  )

(evil-leader/set-key
  "tg" 'golden-ratio-mode
  "tS" 'flyspell-mode
  "ts" 'flycheck-mode
  "tc" 'company-mode
  "tf" 'fci-mode
  "tn" 'nlinum-mode
  "tr" 'nlinum-relative-toggle
  "tv" 'smooth-scrolling-mode
  "tV" 'visual-line-mode
  "th" 'maple/cycle-themes/body
  )

(evil-leader/set-key
  "jb" 'avy-pop-mark
  "jj" 'evil-avy-goto-char
  "jJ" 'evil-avy-goto-char-2
  "jl" 'evil-avy-goto-line
  "jw" 'evil-avy-goto-word-or-subword-1)

(evil-leader/set-key
  "gi" 'magit-init
  "gs" 'magit-status
  "gb" 'magit-branch
  "gd" 'magit-diff
  "gD" 'magit-branch-delete
  "gc" 'magit-branch-or-checkout
  "gt" 'git-timemachine
  )

(evil-leader/set-key
  ;; "ww" 'split-window-horizontally-instead
  ;; "wo" 'find-file-other-window
  ;; ;; 关闭其他窗口
  "wC"  'delete-other-windows
  "wc"  'delete-window
  "wH"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "w <left>"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "w <down>"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "w <up>"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "w <right>"  'evil-window-right
  "wo"  'other-frame
  "ws"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "ww"  'other-window
  "w="  'balance-windows
  "w0"  'select-window-0
  "w1"  'select-window-1
  "w2"  'select-window-2
  "w3"  'select-window-3
  "w4"  'select-window-4
  "w5"  'select-window-5
  )

(evil-leader/set-key
  "pb"  'projectile-switch-to-buffer
  "pw"  'projectile-ag
  "pd"  'projectile-find-dir
  "pf"  'projectile-find-file
  "pp"  'projectile-switch-project
  "pr"  'projectile-recentf
  "pi"  'projectile-invalidate-cache
  "pc"  'projectile-cleanup-known-projects
  )

(evil-leader/set-key
  "<tab>" 'maple/switch-to-previous-buffer
  "TAB" 'maple/switch-to-previous-buffer
  "bk" 'kill-this-buffer
  "be" 'maple/safe-erase-buffer
  "bh" 'maple/switch-to-scratch-buffer
  "bK" 'maple/kill-other-buffers
  "bm" 'bookmark-set
  "bj" 'bookmark-jump
  "bs" 'bookmark-save
  "bw" 'read-only-mode
  "bp" 'evil-prev-buffer
  "bn" 'evil-next-buffer
  "bR" 'maple/safe-revert-buffer
  "bP"  'maple/copy-clipboard-to-whole-buffer
  "bY"  'maple/copy-whole-buffer-to-clipboard
  "bl"  'blog-admin-start
  "bi"  'imenu-list-minor-mode
  )

(evil-leader/set-key
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen)

(evil-leader/set-key
  "sql" 'sql-sqlite
  "sqp" 'sql-postgres
  "sqm" 'sql-mysql
  )

(evil-leader/set-key
  "oa" 'org-agenda
  "oc" 'org-capture
  "ob" 'org-iswitchb
  "od" 'youdao-dictionary-search-at-point+
  "oD" 'youdao-dictionary-search-at-point
  "op" 'browse-at-remote
  )

(evil-leader/set-key
  "qr" 'restart-emacs
  "qk" 'kill-emacs
  "qc" 'maple/reload-user-init-file
  )

(evil-leader/set-key-for-mode 'web-mode
  "rc" 'web-mode-element-clone
  "rd" 'web-mode-element-vanish
  "rk" 'web-mode-element-kill
  "rr" 'web-mode-element-rename
  "rw" 'web-mode-element-wrap
  "gb" 'web-mode-element-beginning
  "gc" 'web-mode-element-child
  "gp" 'web-mode-element-parent
  ;; "gs" 'web-mode-element-sibling-next
  )

(evil-leader/set-key-for-mode 'python-mode
  "va" 'pyvenv-workon
  "vc" 'pyvenv-deactivate
  "rI" 'py-isort-buffer
  )

(evil-leader/set-key-for-mode 'go-mode
  "ri" 'go-goto-imports
  "rI" 'go-remove-unused-imports
  "ra" 'go-import-add
  "rd" 'godef-describe
  )

(evil-leader/set-key-for-mode 'org-mode
  "oe" 'org-export-dispatch
  "ot" 'org-set-tags
  "o." 'org-time-stamp
  "op" 'org-priority
  "oP" 'org-pomodoro
  "or" 'org-decrypt-entry
  "ow" 'org-refile
  "td" 'org-deadline
  "ts" 'org-schedule
  "tm" 'org-toggle-inline-images
  "im" 'maple/capture-screenshot
  "em" 'maple/org-md-export-to-markdown
  "eh" 'maple/org-html-export-to-html
  )

(maple/define-key evil-normal-state-map
                  (kbd "M-J") 'evil-window-move-very-bottom
                  (kbd "M-K") 'evil-window-move-very-top
                  (kbd "M-L") 'evil-window-move-far-right
                  (kbd "M-H") 'evil-window-move-far-left
                  (kbd "M-j") 'evil-window-down
                  (kbd "M-k") 'evil-window-up
                  (kbd "M-l") 'evil-window-right
                  (kbd "M-h") 'evil-window-left
                  (kbd "H") (kbd "^")
                  (kbd "L") (kbd "$")
                  (kbd "U") 'undo-tree-redo
                  ;; (kbd "RET") 'maple/evil-insert-line-below
                  (kbd "S-<return>") 'maple/evil-insert-line-above)

(maple/define-key evil-insert-state-map
                  (kbd "C-h") (kbd "<left>")
                  (kbd "C-l") (kbd "<right>")
                  (kbd "C-j") (kbd "<down>")
                  (kbd "C-k") (kbd "<up>")
                  (kbd "C-v") 'cua-paste)

(maple/define-key evil-visual-state-map
                  (kbd "H") (kbd "^")
                  (kbd "L") (lambda ()
                              (interactive)
                              (evil-end-of-line))
                  (kbd "<") (lambda ()
                              (interactive)
                              (call-interactively 'evil-shift-left)
                              (evil-normal-state)
                              (evil-visual-restore))
                  (kbd ">") (lambda ()
                              (interactive)
                              (call-interactively 'evil-shift-right)
                              (evil-normal-state)
                              (evil-visual-restore)))

(global-set-key [f6] 'maple/indent-buffer)
(global-set-key [tab] (lambda()
                        (interactive)
                        (if (minibufferp)
                            (minibuffer-complete)
                          (company-indent-or-complete-common))))


;; ivy
(evil-leader/set-key
  "," 'counsel-M-x
  "/" 'swiper ;;当前文件内容
  "ff" 'find-file
  "fF" 'counsel-find
  "fr" 'counsel-recentf
  "fw" 'counsel-ag
  "fW" 'counsel-ag
  "sj" 'counsel-semantic-or-imenu
  "bb" 'ivy-switch-buffer  ;;显示缓冲区(已经打开的文件)
  "ph"  'counsel-projectile ;;在工程内查找
  "pw"  'counsel-projectile-ag)

(provide 'init-keybind)
;;; init-keybind.el ends here
