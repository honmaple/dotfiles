;; C-x C-j打开当前文件所在目录
;; C-x C-f 新建文件
;; + 新建目录
;; m 标记文件
;; u 取消标记
;; U 取消所有标记
;; D 直接删除文件
;; R 重命名
;; d 标记删除
;; c 标记拷贝文件
;; C 直接拷贝文件
;; q 退出
;; H 显示隐藏文件

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-async-mode)
  :config
  (setq dired-recursive-copies 'always) ;;递归拷贝
  (put 'dired-find-alternate-file 'disabled nil)  ;; 只有一个buffer
  :bind (:map dired-mode-map
              ("H" . dired-omit-mode)
              ("RET" . dired-find-alternate-file)
              ("C-c C-e" . wdired-change-to-wdired-mode)))

(use-package dired-x
  :ensure nil
  :commands (dired-jump dired-jump-other-window)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$\\|\\.pdf$\\|\\.tex$\\|\\*~$")))

(use-package image-dired
  :ensure nil
  :commands image-dired
  :config
  (setq image-dired-thumbnail-storage 'standard)
  :evil-bind
  (normal image-dired-thumbnail-mode-map
          "j"  'image-dired-next-line
          "k"  'image-dired-previous-line
          "l"  'image-dired-forward-image
          "h"  'image-dired-backward-image
          "q"  'image-dired-kill-buffer-and-window
          (kbd "RET") 'image-dired-display-thumbnail-original-image))

(provide 'init-dired)
