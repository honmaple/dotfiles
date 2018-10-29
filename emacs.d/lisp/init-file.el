;;; init-file.el --- Initialize file configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; file configurations.
;;

;;; Code:

(use-package recentf
  :ensure nil
  :init
  (maple/add-hook 'find-file-hook
    (unless recentf-mode
      (recentf-mode)
      (recentf-track-opened-file)))
  :config
  (setq recentf-save-file (concat maple-cache-directory "recentf")
        recentf-max-saved-items 100
        recentf-auto-cleanup 'never
        recentf-exclude (list "\\.\\(png\\|jpg\\)\\'"
                              "COMMIT_EDITMSG\\'"
                              (expand-file-name maple-cache-directory)
                              (expand-file-name package-user-dir))))

(use-package savehist
  :ensure nil
  :hook (maple-init . savehist-mode)
  :config
  ;; Minibuffer history
  (setq savehist-file (concat maple-cache-directory "savehist")
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :hook (maple-init . save-place-mode)
  :config
  (setq save-place-file (concat maple-cache-directory "places")))

(use-package neotree
  :commands neo-global--window-exists-p
  :config
  (setq neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-mode-line-type 'default
        neo-cwd-line-style 'button
        neo-smart-open t
        neo-show-hidden-files nil
        neo-auto-indent-point t
        neo-vc-integration '(face))
  (defun maple/neo-buffer--insert-root-entry (node)
    (neo-buffer--node-list-set nil node)
    (cond ((eq neo-cwd-line-style 'button)
           (neo-path--insert-header-buttonized node))
          (t
           (neo-buffer--insert-with-face (neo-path--shorten node (window-body-width))
                                         'neo-root-dir-face)))
    (neo-buffer--newline-and-begin)
    (when neo-show-updir-line
      (neo-buffer--insert-fold-symbol 'close (neo-path--updir node))
      (insert-button ".."
                     'action '(lambda (x) (neotree-change-root))
                     'follow-link t
                     'face neo-dir-link-face
                     'neo-full-path (neo-path--updir node))
      (neo-buffer--node-list-set nil (neo-path--updir node))
      (neo-buffer--newline-and-begin)))
  (fset 'neo-buffer--insert-root-entry 'maple/neo-buffer--insert-root-entry)
  (maple/evil-map neotree-mode-map)
  :bind (([f2] . neotree-toggle)
         :map neotree-mode-map
         ("C" . neotree-copy-node)
         ("D" . neotree-delete-node)
         ("R" . neotree-rename-node)
         ("+" . neotree-create-node)
         ("^" . neotree-select-up-node)))

(use-package undo-tree
  :ensure nil
  :hook (maple-init . global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist
        (list (cons "." (concat maple-cache-directory "undo-tree"))))
  (unless (file-exists-p (concat maple-cache-directory "undo-tree"))
    (make-directory (concat maple-cache-directory "undo-tree")))
  :diminish undo-tree-mode)

(defun maple/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun maple/open-keys-file()
  (interactive)
  (find-file "~/.emacs.d/lisp/init-keybind.el"))

(defun maple/open-gtd-file()
  (interactive)
  (find-file "~/org-mode/gtd.org"))

(defun maple/open-test-file()
  (interactive)
  (find-file (read-file-name "select test file: " "~/test/")))

(defun maple/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun maple/unix2dos ()
  "Convert the current buffer to DOS file format."
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
         (maple-system-is-windows (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         (maple-system-is-mac (shell-command (format "open \"%s\"" file-path)))
         (maple-system-is-linux (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun maple/sudo-edit (&optional arg)
  "Edit file with sudo ARG."
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
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
  "Remove file connected to current buffer and kill buffer."
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

(defun maple/copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (file-name-nondirectory buffer-file-name) list-buffers-directory)))
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

(defun maple/check-large-file ()
  (when (and (> (buffer-size) (* 1024 1024 3))
             (y-or-n-p (format (concat "%s is a large file, open literally to "
                                       "avoid performance issues?")
                               buffer-file-name)))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'maple/check-large-file)

(provide 'init-file)

;;; init-file.el ends here
