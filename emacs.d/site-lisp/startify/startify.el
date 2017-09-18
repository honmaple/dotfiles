;;;  -*- lexical-binding: t -*-
;;; startify.el --- A startup screen

;; Copyright (c) 2017 honmaple
;;
;; Author: honmaple
;;
;;; License: GPLv3
;;
;;; Commentary:

;;; Code:

;;
;; Customs
;;

(defvar statify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'startify-next-button)
    (define-key map (kbd "k") #'startify-previous-button)
    (define-key map (kbd "n") #'startify-next-button)
    (define-key map (kbd "p") #'startify-previous-button)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar startify-init-finished nil)

(defun startify-subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun startify-insert-file-list (title title-action list list-action)
  "Insert `title `list of files in the home buffer."
  (when (car list)
    (startify--create-widget
     title title-action)
    (insert ":\n")
    (mapc (lambda (el)
            (startify--insert-text
             (cl-position el list :test 'equal)
             el
             (lambda() (funcall list-action el))))
            list))
    (insert "\n"))

(defun startify--create-widget (text func)
  (widget-create 'push-button
                 :action `(lambda (&rest ignore) (,func))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-face 'font-lock-keyword-face
                 :format "\n[%[%t%]]" text))

(defun startify--insert-text (num text func)
  (startify--create-widget num func)
  (insert "\t")
  (insert (propertize text 'font-lock-face '(:inherit font-lock-comment-face))))


(defvar startify--width 80)

(defun startify-center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

;;;###autoload
(defun startify-init()
  (recentf-mode)
  (startify-insert-file-list
   "Files"
   'helm-recentf
   (startify-subseq recentf-list 0 10)
   'find-file-existing)
  (startify-insert-file-list
   "Projects"
   'helm-projectile
   (startify-subseq projectile-known-projects 0 10)
   'projectile-switch-project-by-name)
  (require 'bookmark)
  (startify-insert-file-list
   "Bookmarks"
   'helm-bookmarks
   (startify-subseq (bookmark-all-names) 0 10)
   'bookmark-jump)
  (startify--insert-text "q" "quit" 'save-buffers-kill-terminal))

(defun startify-next-button ()
  (interactive)
  (ignore-errors (goto-char (next-button (point)))))

(defun startify-previous-button ()
  (interactive)
  (ignore-errors (goto-char (previous-button (point)))))

;;;###autoload
(define-minor-mode startify-mode
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (after-load 'evil
              (evil-define-key 'normal map
                "j" 'startify-next-button
                "k" 'startify-previous-button
                "n" 'startify-next-button
                "p" 'startify-previous-button
                "q" 'quit-window))
            map)
  (if (and (not startify-init-finished) startify-mode)
      (dolist (buffer '("*scratch*"))
        (when (and (get-buffer buffer)
                   (with-current-buffer buffer
                     (startify-init)
                     (setq-local startify-init-finished t)
                     ))))))

(provide 'startify)
