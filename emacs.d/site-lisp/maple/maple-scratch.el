;;; maple-scratch.el ---  scratch message configuration.	-*- lexical-binding: t -*-

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
;; scratch message configuration.
;;

;;; Code:
(require 'subr-x)

(defvar maple/scratch-source nil
  "Whether insert source.")
(defvar maple/scratch-buffer "*scratch*"
  "Only insert scratch message in buffer.")
(defvar maple/scratch-alist
  '(("Files"
     :action 'ivy-recentf
     :source (maple/scratch--subseq recentf-list 0 10)
     :source-action 'find-file-existing
     :require (recentf-mode)
     :desc "Open Recenf Files")
    ("Projects"
     :action 'projectile-switch-project
     :source (maple/scratch--subseq projectile-known-projects 0 10)
     :source-action 'projectile-switch-project-by-name
     :require (projectile-mode)
     :desc "Open Project")
    ("Bookmarks"
     :action 'bookmark-jump
     :source (maple/scratch--subseq (bookmark-all-names) 0 10)
     :source-action  'bookmark-jump
     :require (require 'bookmark)
     :desc "Jump to Bookmark")
    ("Init"
     :action 'maple/open-init-file
     :desc "Open Init File")
    ("Test"
     :action 'maple/open-test-file
     :desc "Open Test File")
    ("quit"
     :action 'save-buffers-kill-terminal
     :desc "Quit Emacs")))

(defmacro maple/dolist (spec &rest body)
  "Like dolist but get INDEX, SPEC &REST BODY."
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  `(let ((num 0))
     (dolist ,(cdr spec)
       (let ((,(car spec) num))
         ,@body
         (setq num (+ num 1))))))

(defun maple/scratch--subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun maple/scratch--button (label action &optional -face -help)
  "Button LABEL ACTION &OPTIONAL -FACE -HELP -FORMAT."
  (insert-button
   (format "%s" label)
   'action
   `(lambda (_) (call-interactively (or (command-remapping ,action) ,action)))
   'follow-link t
   'face (or -face 'font-lock-keyword-face)
   'help-echo (or -help label)))

(defun maple/scratch--item (source action)
  "Button SOURCE ACTION &OPTIONAL -FACE."
  (insert "\n")
  (maple/dolist (index item source)
    (insert "[")
    (maple/scratch--button index `(lambda ()(interactive) `(,action ,item)))
    (insert "]\t")
    (maple/scratch--button item `(lambda ()(interactive) (,action ,item)) 'font-lock-comment-face)
    (insert "\n")))

(defun maple/scratch--text (text &optional face)
  "TEXT add FACE."
  (propertize text 'font-lock-face (or face 'font-lock-comment-face)))

(defun maple/scratch-startup ()
  "Insert start message with FACE."
  (maple/scratch--text
   (format "\nEmacs startup finished in %.2fms with %s packages\n"
           (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))
           (length load-path))))

(defun maple/scratch-previous-button ()
  "Previous button."
  (interactive)
  (move-beginning-of-line 1)
  (let ((btn (previous-button (point))))
    (if btn (goto-char btn)
      (progn
        (goto-char (point-max))
        (maple/scratch-previous-button)))))

(defun maple/scratch-next-button ()
  "Next button."
  (interactive)
  (let ((btn (next-button (point))))
    (if btn (goto-char btn)
      (progn
        (goto-char (point-min))
        (maple/scratch-next-button)))))

(defun maple/scratch--init(label action desc)
  "LABEL &KEY ACTION SOURCE SOURCE-ACTION REQUIRE DESC."
  (insert "[")
  (maple/scratch--button label action nil label)
  (insert "]\t")
  (maple/scratch--button (or desc label) action 'font-lock-comment-face)
  (insert "\n"))

(defun maple/scratch--init-with-source(label action source source-action require desc)
  "LABEL &KEY ACTION SOURCE SOURCE-ACTION REQUIRE DESC."
  (insert "[")
  (maple/scratch--button label action nil label)
  (insert "]")
  (when require (eval `,require))
  (if source (eval `(maple/scratch--item ,source ,source-action))
    (insert "\t" (maple/scratch--text desc)))
  (insert "\n"))

(defun maple/scratch-init()
  "Init maple/scratch."
  (maple/dolist (index args maple/scratch-alist)
    (cl-destructuring-bind (label &key action source source-action require desc) args
      (if maple/scratch-source
          (maple/scratch--init-with-source label action source source-action require desc)
        (maple/scratch--init index action desc))))
  (insert (maple/scratch-startup)))

(defvar maple-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'maple/scratch-next-button)
    (define-key map (kbd "C-p") #'maple/scratch-previous-button)
    (define-key map (kbd "C-q") #'save-buffers-kill-terminal)
    map) "Keymap of command `maple-scratch-mode'.")

;;;###autoload
(define-minor-mode maple-scratch-mode
  "maple-scratch-mode."
  :global nil
  :keymap maple-scratch-mode-map
  (when (and maple-scratch-mode
             maple/scratch-buffer)
    (with-current-buffer maple/scratch-buffer
      (maple/scratch-init))))

(provide 'maple-scratch)
;;; maple-scratch.el ends here
