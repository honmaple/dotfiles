;;默认分屏
(use-package window
  :defer t
  :init (setq split-width-threshold 1))

(use-package winner
  :defer t
  :init (add-hook 'after-init-hook #'winner-mode))

(use-package window-numbering
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook #'window-numbering-mode))

(use-package popwin
  :ensure t
  :init (add-hook 'after-init-hook #'popwin-mode)
  :config
  (setq popwin:special-display-config
        '(("*Help*" :dedicated t :position bottom :stick nil :noselect nil)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.2)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)

          ("\*flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)
          )))

(use-package golden-ratio  ;;黄金分割
  :ensure t
  :defer t
  :init (golden-ratio-mode 1)
  :diminish golden-ratio-mode "ⓖ"
  :config
  (progn
    (dolist (m '("bs-mode"
                 "calc-mode"
                 "ediff-mode"
                 "dired-mode"
                 "gud-mode"
                 "speedbar-mode"
                 "term-mode"
                 "restclient-mode"
                 "newsticker-treeview-mode"
                 "newsticker-treeview-list-mode"
                 "newsticker-treeview-item-mode"
                 ))
      (add-to-list 'golden-ratio-exclude-modes m))

    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")
    ;; golden-ratio-exclude-buffer-names
    (dolist (n '(" *NeoTree*"
                 "*LV*"
                 " *which-key*"
                 "*Ilist*"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))

    ;; golden-ratio-extra-commands
    (dolist (f '(ace-window
                 ace-delete-window
                 ace-select-window
                 ace-swap-window
                 ace-maximize-window
                 avy-pop-mark
                 buf-move-left
                 buf-move-right
                 buf-move-up
                 buf-move-down
                 evil-avy-goto-word-or-subword-1
                 evil-avy-goto-line
                 evil-window-delete
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
                 quit-window
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
                 windmove-left
                 windmove-right
                 windmove-up
                 windmove-down))
      (add-to-list 'golden-ratio-extra-commands f))
    (defun maple/no-golden-ratio-for-buffers (bufname)
      "Disable golden-ratio if BUFNAME is the name of a visible buffer."
      (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

    (defun maple/no-golden-ratio-guide-key ()
      "Disable golden-ratio for guide-key popwin buffer."
      (or (maple/no-golden-ratio-for-buffers " *guide-key*")
          (maple/no-golden-ratio-for-buffers " *popwin-dummy*")
          (maple/no-golden-ratio-for-buffers "*Ilist*")))
    (add-to-list 'golden-ratio-inhibit-functions
                 'maple/no-golden-ratio-guide-key)

    ))
(provide 'init-windows)
