;;----------------------- -----------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------

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

;;默认分屏
(setq split-width-threshold 1)
(setq display-buffer-alist '(("\\*Warnings\\*" display-buffer-below-selected)
                             ("\\*Help\\*" display-buffer-below-selected))) ;;设置分屏
;; (add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*"
;;                                           (cons #'display-buffer-no-window nil)))
(defun maybe-set-quit-key ()
  (when (string= (buffer-name) "*Async Shell Command*")
    (local-set-key (kbd "q") #'quit-window)))

(add-hook 'shell-mode-hook #'maybe-set-quit-key)

(defun maple/close-process ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)"
                                                change)
                              (kill-buffer (process-buffer proc))
                              (when (> (count-windows) 1)
                                (delete-window)))))))


(use-package winner
  :defer t
  :init (winner-mode t))

(use-package popwin
  :ensure t
  :config (popwin-mode 1))

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
