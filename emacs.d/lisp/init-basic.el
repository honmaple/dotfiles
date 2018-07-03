;;; init-basic.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

;; Author: lin.jiang <xiyang0807@gmail.com>
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
;; Basic configurations.
;;

;;; Code:

(eval-when-compile (require 'cl))

(defconst maple-cache-directory
  (expand-file-name (concat user-emacs-directory "cache/"))
  "Maple storage area for persistent files.")

(defconst maple-system-is-mac
  (eq system-type 'darwin))

(defconst maple-system-is-linux
  (eq system-type 'gnu/linux))

(defconst maple-system-is-windows
  (eq system-type 'windows-nt))

(defun maple/plist-get(args key &optional default)
  "Custom `plist-get` with ARGS and KEY DEFAULT."
  (or (plist-get args key)
      (plist-get (cdr args) key)
      default))

(defmacro maple/add-hook(hook &rest args)
  "Custom hook with HOOK and ARGS no need lambda."
  (declare (indent defun))
  (let ((if-p (maple/plist-get args :if t))
        (local-p (maple/plist-get args :local))
        (append-p (maple/plist-get args :append))
        (hooks (if (cdr-safe (cadr hook))
                   (cadr hook)
                 (list (cadr hook))))
        (funcs (let ((val (car args)))
                 (if (memq (car-safe val) '(quote function))
                     (if (cdr-safe (cadr val)) (cadr val)
                       (list (cadr val)))
                   (list `(lambda(&rest _) ,@args)))))
        forms)
    (dolist (fn funcs)
      (setq fn `(function ,fn))
      (dolist (i hooks)
        (push `(add-hook ',i ,fn ,append-p ,local-p) forms)))
    `(progn (when ,if-p ,@forms))))

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

(defun maple/initial-message(&optional prefix)
  "Show initial message when PREFIX comment."
  (interactive)
  (if (executable-find "fortune")
      (format (concat prefix "%s\n\n%s")
              (replace-regexp-in-string
               "\\[[0-9]*m" "" ; remove chinese shell char
               (replace-regexp-in-string
                "\n" (concat "\n" prefix) ; comment each line
                (replace-regexp-in-string
                 "\s*$" ""    ; remove spaces
                 (replace-regexp-in-string
                  "\n$" ""    ; remove trailing linebreak
                  (shell-command-to-string
                   "fortune -a | fmt -80 -s | cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n")))))
              (concat prefix "Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
    (concat prefix "Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

(defun maple/comment-or-uncomment (&optional paste)
  "Comments or uncomments the region or the current line if there's no active region with no `PASTE`."
  (interactive)
  (save-excursion
    (when (and (hs-minor-mode) (hs-already-hidden-p))
      (end-of-visual-line)
      (evil-visual-state)
      (beginning-of-visual-line))
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (when paste
        (copy-region-as-kill beg end)
        (goto-char end)
        (yank))
      (comment-or-uncomment-region beg end))))

(defun maple/copy-and-comment ()
  "Copy and comment."
  (interactive)
  (maple/comment-or-uncomment t))

(defun maple/indent-buffer ()
  "Format buffer with `indent-region`."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun maple/reload-user-init-file()
  "Reload init file."
  (interactive)
  (load-file user-init-file))

(defun maple/define-key (keymap key def &rest bindings)
  "Define multi keybind with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap key def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun maple/company-backend (hook backends)
  "Set HOOK with BACKENDS `company-backends'."
  (declare (indent defun))
  (let ((fn `(set (make-variable-buffer-local 'company-backends)
                  (append (list (company-backend-with-yas ',backends))
                          company-default-backends))))
    (add-hook hook `(lambda() ,fn))))

(defun maple/get-weekday()
  "Show weekday of today."
  (car (rassq (string-to-number (format-time-string "%w"))
              '(("Sunday" . 0)
                ("Monday" . 1)
                ("Tuesday" . 2)
                ("Wednesday" . 3)
                ("Thursday" . 4)
                ("Friday" . 5)
                ("Saturday" . 6)))))

(defun maple/truncate-lines()
  "Turn on `truncate-lines`."
  (interactive)
  (visual-line-mode t)
  (toggle-truncate-lines t))

(defun maple/close-nlinum()
  "Close nlinum."
  (nlinum-mode -1))

(defun maple/reopen-buffer(buffer-name &optional restore)
  "Reopen BUFFER-NAME RESTORE."
  (if (get-buffer buffer-name)
      (let ((buffer-text (with-current-buffer buffer-name
                           (buffer-substring (point-min) (point-max)))))
        (kill-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (when restore (insert buffer-text))))
    (message (format "%s buffer is not exists" buffer-name))))

(defun maple/startup-buffer()
  "Start buffer."
  (dolist (buffer '("*Messages*" "*Compile-Log*"))
    (and (get-buffer buffer) (maple/reopen-buffer buffer t))))

(add-hook 'emacs-startup-hook 'maple/startup-buffer)

(provide 'init-basic)

;;; init-basic.el ends here
