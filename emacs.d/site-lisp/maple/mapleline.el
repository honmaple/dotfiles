;;; mapleline.el --- modeline configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

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
;; modeline configurations.
;;

;;; Code:
(defvar mapleline-format nil)
(defvar mapleline-show-p nil)
(defvar mapleline-buffer " *Minibuf-0*")

(defun mapleline-enter()
  "Setup after enter minibuffer."
  (with-selected-window (minibuffer-selected-window)
    (mapleline-unshow)))

(defun mapleline-show (&optional force)
  "Show modeline with FORCE."
  (when mode-line-format
    (setq mapleline-format mode-line-format)
    (setq mode-line-format nil))
  (set-window-fringes (minibuffer-window) 0 0)
  (setq message-truncate-lines t)
  (with-current-buffer mapleline-buffer
    (erase-buffer)
    (insert (format-mode-line mapleline-format)))
  (when (and (not mapleline-show-p) force)
    (force-window-update))
  (setq mapleline-show-p t))

(defun mapleline-unshow (&optional force)
  "Unshow modeline with FORCE."
  (when mapleline-format
    (setq mode-line-format mapleline-format)
    (force-mode-line-update))
  (set-window-fringes (minibuffer-window) nil nil)
  (with-current-buffer mapleline-buffer
    (erase-buffer))
  (when (and mapleline-show-p force)
    (force-window-update))
  (setq mapleline-show-p nil))

(defun mapleline-mode ()
  "Setup the default modeline."
  (if (current-message)
      (mapleline-unshow t)
    (mapleline-show t)))

(defun mapleline-message (&rest _args)
  "Advice message with ARGS."
  (mapleline-mode))

(defun mapleline-enable ()
  "Setup the default modeline."
  (interactive)
  (when mode-line-format
    (setq mapleline-format mode-line-format)
    (setq mode-line-format nil))
  (mapleline-show t)
  (add-hook 'minibuffer-setup-hook 'mapleline-enter)
  (advice-add 'message :after 'mapleline-message))

(defun mapleline-disable ()
  "Disable modeline."
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'mapleline-enter)
  (advice-remove 'message 'mapleline-message)
  (mapleline-unshow t))

(provide 'mapleline)
;;; mapleline.el ends here
