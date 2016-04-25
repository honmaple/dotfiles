(evil-leader/set-key
  "cc" 'comment-or-uncomment-region
  "fj" 'helm-semantic-or-imenu ;;显示函数
  "ff" 'helm-find-files ;;查找文件名 区别helm-find-files指定文件夹
  "fr" 'helm-recentf
  "fs" 'helm-occur ;;当前文件内容
  "fw" 'helm-ag
  "u"  'undo-tree-visualize
  "se" 'evil-mc-make-all-cursors
  ;; "se" 'mc/mark-all-like-this
  "<tab>" 'maple/switch-to-previous-buffer


  "pb"  'helm-projectile-switch-to-buffer
  "pd"  'helm-projectile-find-dir
  "pf"  'helm-projectile-find-file
  "ph"  'helm-projectile ;;在工程内查找
  "pp"  'helm-projectile-switch-project
  "pr"  'helm-projectile-recentf
  "pv"  'projectile-vc
  "sgp" 'helm-projectile-grep

  "el" 'maple/toggle-flycheck-error-list
  "ec" 'flycheck-clear

  "gs" 'magit-status
  "gb" 'magit-branch
  "gd" 'magit-diff
  )

(evil-leader/set-key
  ;; "ww" 'split-window-horizontally-instead
  ;; "wo" 'find-file-other-window
  ;; ;; 关闭其他窗口
  "wC" 'sanityinc/toggle-delete-other-windows
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
  "bb" 'helm-mini  ;;显示缓冲区(已经打开的文件)
  "bl" 'org-new-blog  ;;插入博客地址
  "bk" 'sanityinc/toggle-delete-other-windows

  "bm" 'bookmark-set
  "bj" 'bookmark-jump
  "bs" 'bookmark-save
  )
(evil-leader/set-key
  "lp" 'persp-prev
  "ln" 'persp-next
  "ll" 'persp-switch
  "lL" 'persp-load-state-from-file
  "la" 'persp-add-buffer
  "ls" 'persp-save-state-to-file
  "lS" 'persp-save-to-file-by-names
  "l <tab>" 'maple/jump-to-last-layout
  )
(define-key evil-normal-state-map (kbd "M-J") 'evil-window-move-very-bottom)
(define-key evil-normal-state-map (kbd "M-K") 'evil-window-move-very-top)
(define-key evil-normal-state-map (kbd "M-L") 'evil-window-move-far-right)
(define-key evil-normal-state-map (kbd "M-H") 'evil-window-move-far-left)
(provide 'init-keybind)
