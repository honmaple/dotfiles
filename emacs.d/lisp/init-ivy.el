;; 必须的,使用频率排序
(use-package smex)

(use-package counsel
  :diminish (ivy-mode counsel-mode)
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode)
  :config
  (progn
    (setq enable-recursive-minibuffers t)
    (setq ivy-height 12
          ivy-do-completion-in-region t
          ivy-use-selectable-prompt t
          ivy-wrap t
          ivy-extra-directories nil
          ivy-fixed-height-minibuffer t
          ;; Don't use ^ as initial input
          ivy-initial-inputs-alist nil
          ;; highlight til EOL
          ivy-format-function #'ivy-format-function-line
          ;; disable magic slash on non-match
          ;; ~ to /home/user
          ivy-magic-tilde nil
          ivy-use-virtual-buffers nil
          ivy-virtual-abbreviate 'fullpath
          ivy-magic-slash-non-match-action nil)
    ;; (setq ivy-re-builders-alist
    ;;       '((t . ivy--regex-fuzzy)))
    ;; (setq confirm-nonexistent-file-or-buffer t)
    (setq ivy-re-builders-alist
          '((t   . ivy--regex-ignore-order)))
    (setq completing-read-function 'ivy-completing-read
          read-file-name-function  'read-file-name-default)
    (after-load 'evil
      (evil-make-overriding-map ivy-occur-mode-map 'normal))

    ;; Integration with `projectile'
    (after-load 'projectile
      (setq projectile-completion-system 'ivy))

    (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
      "Create parent directory if not exists while visiting file."
      (unless (file-exists-p filename)
        (let ((dir (file-name-directory filename)))
          (unless (file-exists-p dir)
            (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
                (make-directory dir)
              (keyboard-quit))
            )))))
  :bind (("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("<tab>" . ivy-partial)
         ("TAB" . ivy-partial)
         ([escape] . minibuffer-keyboard-quit)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         ("C-<return>" . ivy-immediate-done)))

(use-package ivy-rich
  :after (ivy)
  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev
        ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))


(use-package counsel-projectile)

(provide 'init-ivy)
