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

(use-package ivy
  :diminish (ivy-mode)
  :hook (after-init . ivy-mode)
  :config
  (setq enable-recursive-minibuffers t
        completing-read-function 'ivy-completing-read)

  (setq ivy-height 12
        ivy-do-completion-in-region t
        ivy-use-selectable-prompt t
        ivy-wrap t
        ivy-extra-directories nil
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; disable magic slash on non-match
        ;; ~ to /home/user
        ivy-magic-tilde t
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ;; ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ;; ivy display
        ivy-count-format ""
        ivy-format-function 'maple/ivy-format-function
        ;; fuzzy match
        ivy-re-builders-alist
        '((t   . ivy--regex-ignore-order)))

  ;; custom ivy display function
  (advice-add 'ivy-read :around #'maple/ivy-read-around)

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

  ;; complete or done
  (defun maple/ivy-done()
    (interactive)
    (ivy-partial-or-done)
    (delete-minibuffer-contents)
    (let ((ivy-text (ivy-state-current ivy-last)) dir)
      (insert ivy-text)
      (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                 (setq dir (ivy-expand-file-if-directory ivy-text)))
        (ivy--cd dir))))

  ;; ivy-occur custom
  (defun maple/ivy-edit ()
    "Edit the current search results in a buffer using wgrep."
    (interactive)
    (run-with-idle-timer 0 nil 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))

  ;; completion-system
  (with-eval-after-load 'evil
    (evil-make-overriding-map ivy-occur-mode-map 'normal))

  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  (use-package ivy-rich
    :init
    (setq ivy-rich-path-style 'abbrev
          ivy-rich-switch-buffer-align-virtual-buffer t)
    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer))
  :custom-face
  (ivy-highlight-face ((t (:background nil)))))

(use-package counsel
  :diminish (counsel-mode)
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-preselect-current-file t)

  (defun maple/counsel-up-directory()
    (interactive)
    (if (string-equal (ivy--input) "")
        (counsel-up-directory)
      (delete-minibuffer-contents)))

  ;; custom counsel-ag
  (defun maple/counsel-ag(-counsel-ag &optional initial-input initial-directory extra-ag-args ag-prompt)
    (when (and (not initial-input) (region-active-p))
      (setq initial-input (buffer-substring-no-properties
                           (region-beginning) (region-end))))
    (unless initial-directory (setq initial-directory default-directory))
    (funcall -counsel-ag initial-input initial-directory extra-ag-args ag-prompt))

  (advice-add 'counsel-ag :around #'maple/counsel-ag)

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
         ("C-h" . maple/counsel-up-directory)
         ("<tab>" . maple/ivy-done)
         ("TAB" . maple/ivy-done)
         ("C-<return>" . ivy-immediate-done)))

(use-package counsel-projectile)

(provide 'init-ivy)
