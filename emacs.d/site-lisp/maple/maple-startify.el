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

(defvar startify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'startify-next-button)
    (define-key map (kbd "C-p") #'startify-previous-button)
    (define-key map (kbd "C-q") #'save-buffers-kill-terminal)
    map)
  "Keymap of command `startify-mode'.")

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
  "Insert list of files with `TITLE` `TITLE-ACTION` `LIST` `LIST-ACTION`."
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
  "Create widget button with `TEXT` and `FUNC`."
  (widget-create 'push-button
                 :action `(lambda (&rest ignore) (,func))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-face 'font-lock-keyword-face
                 :format "\n[%[%t%]]" text))

(defun startify--insert-text (num text func)
  "Insert `NUM` `TEXT` with `FUNC`."
  (startify--create-widget num func)
  (insert "\t")
  (insert (propertize text 'font-lock-face '(:inherit font-lock-comment-face))))

(defun startify--insert-startup ()
  "Insert start message."
  (insert "\n\n")
  (insert (propertize
           (format "Emacs startup finished in %.2fms with %s packages\n"
                   (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))
                   (length load-path)) 'font-lock-face '(:inherit font-lock-comment-face))))

(defun startify-init()
  (recentf-mode)
  (startify-insert-file-list
   "Files" 'helm-recentf
   (startify-subseq recentf-list 0 10)
   'find-file-existing)
  (startify-insert-file-list
   "Projects" 'helm-projectile
   (startify-subseq projectile-known-projects 0 10)
   'projectile-switch-project-by-name)
  (require 'bookmark)
  (startify-insert-file-list
   "Bookmarks"
   'helm-bookmarks
   (startify-subseq (bookmark-all-names) 0 10)
   'bookmark-jump)
  (startify--insert-text "q" "quit" 'save-buffers-kill-terminal)
  (startify--insert-startup))

(defun startify-previous-button ()
  "Previous button."
  (interactive)
  (move-beginning-of-line 1)
  (let ((btn (previous-button (point))))
    (if btn (goto-char btn)
      (progn
        (goto-char (point-max))
        (startify-previous-button)))))

(defun startify-next-button ()
  "Next button."
  (interactive)
  (let ((btn (next-button (point))))
    (if btn (goto-char btn)
      (progn
        (goto-char (point-min))
        (startify-next-button)))))

;;;###autoload
(define-minor-mode startify-mode
  "Startify."
  :global nil
  :keymap startify-mode-map
  (when (and (not startify-init-finished)
             startify-mode)
    (with-current-buffer "*scratch*"
      (startify-init)
      (setq-local startify-init-finished t))))


(provide 'maple-startify)

;;; maple-startify.el ends here
