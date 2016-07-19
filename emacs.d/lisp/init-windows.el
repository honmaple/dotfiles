;;----------------------- -----------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
  Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)

(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))

(setq split-width-threshold 1 ) ;;默认分屏
(setq display-buffer-alist '(("\\*Warnings\\*" display-buffer-below-selected))) ;;设置分屏

(require-package 'golden-ratio)
(use-package golden-ratio  ;;黄金分割
  :defer t
  :init (golden-ratio-mode 1)
  :diminish golden-ratio-mode "ⓖ"
  :config
  (progn
    (setq golden-ratio-exclude-modes '("ediff-mode"
                                       "dired-mode"
                                       "restclient-mode"
                                       ))

    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

    ;; Disable auto-resizing for some buffers
    ;; (defun maple/no-golden-ratio-for-buffers (bufname)
    ;;   "Disable golden-ratio if BUFNAME is the name of a visible buffer."
    ;;   (and (get-buffer bufname) (get-buffer-window bufname 'visible)))
    ;; (defun maple/no-golden-ratio-guide-key ()
    ;;   "Disable golden-ratio for guide-key popwin buffer."
    ;;   (or (maple/no-golden-ratio-for-buffers " *guide-key*")
    ;;       (maple/no-golden-ratio-for-buffers " *popwin-dummy*")))
    ;; (add-to-list 'golden-ratio-inhibit-functions
    ;;              'maple/no-golden-ratio-guide-key)
    (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
    (add-to-list 'golden-ratio-exclude-buffer-names "*LV*")
    (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")
    (setq golden-ratio-extra-commands
          (append golden-ratio-extra-commands
                  '(evil-window-delete
                    evil-window-split
                    evil-window-vsplit
                    evil-window-left
                    evil-window-right
                    evil-window-up
                    evil-window-down
                    evil-window-bottom-right
                    evil-window-top-left
                    evil-window-mru
                    evil-window-next
                    evil-window-prev
                    evil-window-new
                    evil-window-vnew
                    evil-window-rotate-upwards
                    evil-window-rotate-downwards
                    evil-window-move-very-top
                    evil-window-move-far-left
                    evil-window-move-far-right
                    evil-window-move-very-bottom
                    select-window-0
                    select-window-1
                    select-window-2
                    select-window-3
                    select-window-4
                    select-window-5
                    select-window-6
                    select-window-7
                    select-window-8
                    select-window-9
                    buf-move-left
                    buf-move-right
                    buf-move-up
                    buf-move-down
                    ess-eval-buffer-and-go
                    ess-eval-function-and-go
                    ess-eval-line-and-go)))
    ))
(provide 'init-windows)
