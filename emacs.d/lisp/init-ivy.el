;; 必须的,使用频率排序
(use-package smex)

(use-package wgrep
  :init
  (defun maple/wgrep-finish-edit()
    (interactive)
    (wgrep-finish-edit)
    (quit-window))
  :bind (:map wgrep-mode-map
              ("C-c C-c" . maple/wgrep-finish-edit)))

(use-package counsel
  :diminish (ivy-mode counsel-mode)
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode)
  :config
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
        ivy-magic-tilde t
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'fullpath
        ;; ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ;; fuzzy match
        ivy-re-builders-alist
        '((t   . ivy--regex-ignore-order)))

  ;; custom ivy display function
  (defvar maple/ivy-format-padding nil)

  (defun maple/ivy-read-around (-ivy-read &rest args)
    "Advice ivy-read `-IVY-READ` `ARGS`."
    (let ((maple/ivy-format-padding (make-string (window-left-column) ?\s)))
      (setcar args (concat maple/ivy-format-padding (car args)))
      (apply -ivy-read args)))

  (defun maple/ivy-format-function (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat maple/ivy-format-padding (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat maple/ivy-format-padding str))
     cands "\n"))

  (advice-add 'ivy-read :around #'maple/ivy-read-around)
  (setq ivy-count-format ""
        ivy-format-function 'maple/ivy-format-function)

  (defun maple/counsel-ag(-counsel-ag &optional initial-input initial-directory extra-ag-args ag-prompt)
    (when (and (not initial-input) (region-active-p))
      (setq initial-input (buffer-substring-no-properties
                           (region-beginning) (region-end))))
    (unless initial-directory (setq initial-directory default-directory))
    (funcall -counsel-ag initial-input initial-directory extra-ag-args ag-prompt))

  (advice-add 'counsel-ag :around #'maple/counsel-ag)

  (defun maple/ivy-done()
    (interactive)
    (ivy-partial-or-done)
    (delete-minibuffer-contents)
    (let ((ivy-text (ivy-state-current ivy-last)) dir)
      (insert ivy-text)
      (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                 (setq dir (ivy-expand-file-if-directory ivy-text)))
        (ivy--cd dir))))


  (defun maple/ivy-edit ()
    "Edit the current search results in a buffer using wgrep."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))

  (setq completing-read-function 'ivy-completing-read
        read-file-name-function  'read-file-name-default)

  (after-load 'evil
    (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
    (evil-make-overriding-map ivy-occur-mode-map 'normal))

  (after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))


  (use-package ivy-rich
    :init
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-path-style 'abbrev
          ivy-rich-switch-buffer-align-virtual-buffer t)
    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer))

  :custom-face
  (ivy-highlight-face ((t (:background nil))))
  :bind (("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("<tab>" . maple/ivy-done)
         ("TAB" . maple/ivy-done)
         ("C-c C-e" . maple/ivy-edit)
         ("C-h" . [backspace])
         ([escape] . minibuffer-keyboard-quit)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         ("<tab>" . maple/ivy-done)
         ("TAB" . maple/ivy-done)
         ("C-<return>" . ivy-immediate-done)))



(use-package counsel-projectile)

(provide 'init-ivy)
