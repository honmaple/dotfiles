(evil-leader/set-key
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "ff" 'helm-find-files
  "fF" 'helm-find
  "fr" 'helm-recentf
  "fw" 'helm-ag
  "fS" 'evil-write-all
  "fs" 'save-buffer
  "fei" 'maple/open-init-file
  "fo" 'maple/open-in-external-app
  "fE" 'maple/sudo-edit
  "fy" 'maple/show-and-copy-buffer-filename
  "fCd" 'maple/unix2dos
  "fCu" 'maple/dos2unix
  "fD" 'maple/delete-current-buffer-file
  "fR" 'maple/rename-current-buffer-file
  )
(evil-leader/set-key
  "cc" 'comment-or-uncomment-region-or-line
  "/" 'helm-swoop ;;当前文件内容
  "u"  'undo-tree-visualize
  "se" 'evil-mc-make-all-cursors
  "ss" 'replace-regexp
  "sj" 'helm-semantic-or-imenu
  ;; "se" 'mc/mark-all-like-this
  "el" 'maple/toggle-flycheck-error-list
  "ec" 'flycheck-clear
  )

(evil-leader/set-key
  "gi" 'magit-init
  "gs" 'magit-status
  "gb" 'magit-branch
  "gd" 'magit-diff
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
  )

(evil-leader/set-key
  "pb"  'helm-projectile-switch-to-buffer
  "pw"  'helm-projectile-ag
  "pd"  'helm-projectile-find-dir
  "pf"  'helm-projectile-find-file
  "ph"  'helm-projectile ;;在工程内查找
  "pp"  'helm-projectile-switch-project
  "pr"  'helm-projectile-recentf
  "pv"  'projectile-vc
  "sgp" 'helm-projectile-grep
  )

(evil-leader/set-key
  "<tab>" 'maple/switch-to-previous-buffer
  "bb" 'helm-mini  ;;显示缓冲区(已经打开的文件)
  "bd" 'kill-this-buffer
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
  "ol" 'org-new-blog  ;;插入博客地址
  )


(evil-leader/set-key-for-mode 'org-mode
  "od" 'org-deadline
  "oe" 'org-export-dispatch
  "ot" 'org-set-tags
  "o." 'org-time-stamp
  "os" 'org-schedule
  "op" 'org-priority
  "oP" 'org-pomodoro
  "or"  'org-decrypt-entry
  )


(define-key evil-normal-state-map (kbd "M-J") 'evil-window-move-very-bottom)
(define-key evil-normal-state-map (kbd "M-K") 'evil-window-move-very-top)
(define-key evil-normal-state-map (kbd "M-L") 'evil-window-move-far-right)
(define-key evil-normal-state-map (kbd "M-H") 'evil-window-move-far-left)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-insert-state-map (kbd "C-h") (kbd "<left>"))
(define-key evil-insert-state-map (kbd "C-l") (kbd "<right>"))
(define-key evil-normal-state-map (kbd "H") (kbd "^"))
(define-key evil-normal-state-map (kbd "L") (kbd "$"))
(define-key evil-visual-state-map (kbd "H") (kbd "^"))
(define-key evil-visual-state-map (kbd "L") (kbd "$"))
;; (global-set-key (kbd "C-a") 'maple/smart-move-beginning-of-line)
;; (global-set-key (kbd "C-e") 'maple/backward-kill-word-or-region)
(provide 'init-keybind)
