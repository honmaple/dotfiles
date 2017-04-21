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
  :defer t
  :config
  (progn
    (setq dired-recursive-copies 'always) ;;递归拷贝
    (add-hook 'dired-mode-hook #'dired-async-mode)
    (after-load 'dired  ;; 只有一个buffer
      (put 'dired-find-alternate-file 'disabled nil)
      ))
  :bind (:map dired-mode-map
              ("H" . dired-omit-mode)
              ("RET" . dired-find-alternate-file)
              ("C-c C-e" . wdired-change-to-wdired-mode)))

(use-package dired-x
  :commands (dired-jump
             dired-jump-other-window)
  :config
  (progn
    (defun maple/set-dired()
      ;; (setq dired-dwim-target t)
      (setq-default dired-omit-files-p t) ; this is buffer-local variable
      (setq dired-omit-files
            (concat dired-omit-files "\\|^\\..+$\\|\\.pdf$\\|\\.tex$\\|\\*~$")))
    (add-hook 'dired-mode-hook 'maple/set-dired)
    ))
(provide 'init-dired)
