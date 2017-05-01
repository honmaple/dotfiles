(use-package recentf
  :defer t
  :init
  (progn
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                      (recentf-mode)
                                      (recentf-track-opened-file))))

    (setq recentf-save-file (concat maple-cache-directory "recentf")
          recentf-max-saved-items 1000
          recentf-auto-cleanup 'never
          recentf-auto-save-timer (run-with-idle-timer 600 t
                                                       'recentf-save-list)))
  :config
  (progn
    (add-to-list 'recentf-exclude
                 (expand-file-name maple-cache-directory))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

(use-package savehist
  :defer t
  :init
  (progn
    ;; Minibuffer history
    (setq savehist-file (concat maple-cache-directory "savehist")
          enable-recursive-minibuffers t ; Allow commands in minibuffers
          history-length 1000
          savehist-additional-variables '(mark-ring
                                          global-mark-ring
                                          search-ring
                                          regexp-search-ring
                                          extended-command-history)
          savehist-autosave-interval 60)
    (savehist-mode t)))

(use-package saveplace
  :defer t
  :init
  (progn
    (if (fboundp 'save-place-mode)
        ;; Emacs 25 has a proper mode for `save-place'
        (save-place-mode)
      (setq save-place t))
    ;; Save point position between sessions
    (setq save-place-file (concat maple-cache-directory "places"))))


(use-package neotree
  :ensure t
  :defer t
  :commands neo-global--window-exists-p
  :config
  (progn
    (setq
     neo-create-file-auto-open t
     neo-banner-message "Press ? for neotree help"
     neo-show-updir-line nil
     neo-mode-line-type 'neotree
     neo-smart-open t
     neo-dont-be-alone t
     neo-persist-show nil
     neo-show-hidden-files nil
     neo-auto-indent-point t
     neo-modern-sidebar t
     neo-vc-integration nil)
    (evil-set-initial-state 'neotree-mode 'emacs)
    )
  :bind (([f2] . neotree-toggle)
         :map neotree-mode-map
         ("j" . neotree-next-line)
         ("k" . neotree-previous-line)
         ("C" . neotree-copy-node)
         ("D" . neotree-delete-node)
         ("R" . neotree-rename-node)
         ("+" . neotree-create-node)
         ("^" . neotree-select-up-node)
         )
  )

(use-package desktop
  :defer t
  :init
  (setq desktop-dirname maple-cache-directory)
  :config
  (push maple-cache-directory desktop-path))

(use-package undo-tree
  :defer t
  :init
  (progn
    (add-hook 'after-init-hook #'global-undo-tree-mode)
    ;; (global-undo-tree-mode)
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist
          `(("." . ,(concat maple-cache-directory "undo-tree"))))
    (unless (file-exists-p (concat maple-cache-directory "undo-tree"))
      (make-directory (concat maple-cache-directory "undo-tree")))
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    )
  :diminish undo-tree-mode
  :bind (:map evil-normal-state-map
              ("U" . undo-tree-redo)))

(defun maple/system-is-mac ()
  (eq system-type 'darwin))
(defun maple/system-is-linux ()
  (eq system-type 'gnu/linux))
(defun maple/system-is-mswindows ()
  (eq system-type 'windows-nt))


(defun maple/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun maple/open-keys-file()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-keybind.el"))

(defun maple/open-gtd-file()
  (interactive)
  (find-file "~/org-mode/gtd.org"))

(defun maple/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun maple/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


(defun maple/open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (cond
         ((maple/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((maple/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((maple/system-is-linux) (let ((process-connection-type nil))
                                    (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun maple/sudo-edit (&optional arg)
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name)) (read-file-name "File: ") buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname) last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun maple/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun maple/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun maple/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; insert one or several line below without changing current evil state
(defun maple/evil-insert-line-below (count)
  "Insert one of several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-below count)))
  (evil-normal-state)
  (evil-next-line count))


;; insert one or several line above without changing current evil state
(defun maple/evil-insert-line-above (count)
  "Insert one of several lines above the current point's line without changing
 the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-above count)))
  (evil-previous-line count)
  (evil-escape))

;; (setq maple/large-file-modes-list
;;       '(archive-mode tar-mode jka-compr git-commit-mode image-mode
;;                      doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
;;                      pdf-view-mode))

;; (defun maple/check-large-file ()
;;   (let* ((filename (buffer-file-name))
;;          (size (nth 7 (file-attributes filename))))
;;     (when (and
;;            ;; (not (memq major-mode maple/large-file-modes-list))
;;            size (> size (* 1024 1024 1))
;;            (y-or-n-p (format (concat "%s is a large file, open literally to "
;;                                      "avoid performance issues?")
;;                              filename)))
;;       (setq buffer-read-only t)
;;       (buffer-disable-undo)
;;       (fundamental-mode))))

(defun maple/check-large-file ()
  (when (and (> (buffer-size) (* 1024 1024 3))
             (y-or-n-p (format (concat "%s is a large file, open literally to "
                                       "avoid performance issues?")
                               buffer-file-name)))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

;; (defun maple/check-large-file ()
;;   (when (> (buffer-size) 500000)
;;     (progn (fundamental-mode)
;;            (hl-line-mode -1)))
;;   (if (and (executable-find "wc")
;;            (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
;;               5000))
;;       (linum-mode -1)
;;     (linum-mode 1)))
(add-hook 'find-file-hook 'maple/check-large-file)

(provide 'init-file)
