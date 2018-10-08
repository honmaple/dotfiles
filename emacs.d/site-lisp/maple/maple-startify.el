;;; maple-startify.el ---  startify configuration.	-*- lexical-binding: t -*-

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
;; startify configuration.
;;

;;; Code:
(require 'recentf)
(require 'bookmark)
(require 'projectile)

(defvar maple/startify-init-finished nil)
(defvar maple/startify-alist
  '((:title "Files"
            :action 'ivy-recentf
            :open  'find-file-existing
            :source (maple/startify-subseq recentf-list 0 10)
            :require (recentf-mode))
    (:title "Projects"
            :action 'projectile-switch-project
            :open  'projectile-switch-project-by-name
            :source (maple/startify-subseq projectile-known-projects 0 10)
            :require (projectile-mode))
    (:title "Bookmarks"
            :action 'bookmark-jump
            :open  'bookmark-jump
            :source (maple/startify-subseq (bookmark-all-names) 0 10))))

(defun maple/startify-subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun maple/startify--insert-file-list (title title-action list list-action)
  "Insert list of files with `TITLE` `TITLE-ACTION` `LIST` `LIST-ACTION`."
  (when (car list)
    (maple/startify--create-widget title title-action)
    (insert ":\n")
    (mapc (lambda (el)
            (maple/startify--insert-text
             (cl-position el list :test 'equal) el
             (lambda() (funcall list-action el))))
          list))
  (insert "\n"))

(defun maple/startify--create-widget (text func)
  "Create widget button with `TEXT` and `FUNC`."
  (widget-create 'push-button
                 :action `(lambda (&rest ignore) (,func))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-face 'font-lock-keyword-face
                 :format "\n[%[%t%]]" text))

(defun maple/startify--insert-text (num text func)
  "Insert `NUM` `TEXT` with `FUNC`."
  (maple/startify--create-widget num func)
  (insert "\t")
  (insert (propertize text 'font-lock-face '(:inherit font-lock-comment-face))))

(defun maple/startify--insert-startup ()
  "Insert start message."
  (insert "\n\n")
  (insert (propertize
           (format "Emacs startup finished in %.2fms with %s packages\n"
                   (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))
                   (length load-path)) 'font-lock-face '(:inherit font-lock-comment-face))))

(defun maple/startify-previous-button ()
  "Previous button."
  (interactive)
  (move-beginning-of-line 1)
  (let ((btn (previous-button (point))))
    (if btn (goto-char btn)
      (progn
        (goto-char (point-max))
        (maple/startify-previous-button)))))

(defun maple/startify-next-button ()
  "Next button."
  (interactive)
  (let ((btn (next-button (point))))
    (if btn (goto-char btn)
      (progn
        (goto-char (point-min))
        (maple/startify-next-button)))))

(defun maple/startify-init()
  "Init startify."
  (dolist (args maple/startify-alist)
    (let* ((-action (plist-get args :action))
           (-open (plist-get args :open))
           (-source (plist-get args :source))
           (-title (plist-get args :title))
           (-require (plist-get args :require)))
      (when -require
        (eval `,-require))
      (eval `(maple/startify--insert-file-list ,-title ,-action ,-source ,-open))))
  (maple/startify--insert-text "q" "quit" 'save-buffers-kill-terminal)
  (maple/startify--insert-startup))

(defvar startify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'maple/startify-next-button)
    (define-key map (kbd "C-p") #'maple/startify-previous-button)
    (define-key map (kbd "C-q") #'save-buffers-kill-terminal)
    map)
  "Keymap of command `startify-mode'.")

;;;###autoload
(define-minor-mode startify-mode
  "Startify."
  :global nil
  :keymap startify-mode-map
  (when (and (not maple/startify-init-finished)
             startify-mode)
    (with-current-buffer "*scratch*"
      (maple/startify-init)
      (setq maple/startify-init-finished t))))

(provide 'maple-startify)
;;; maple-startify.el ends here
