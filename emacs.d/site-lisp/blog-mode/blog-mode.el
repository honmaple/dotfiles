;;; blog-mode.el ---  blog mode configuration.	-*- lexical-binding: t -*-

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
;; blog mode configuration.
;;

;;; Code:
(require 'tabulated-list)
(require 's)

(defgroup blog-mode nil
  "Major mode for writing blog."
  :prefix "blog-"
  :group 'external)

(defvar blog-buffer-name "*Blog*"
  "Major buffer name.")
(defvar blog-filter-keyword nil
  "Global filter keywords.")
(defvar blog-root-path nil
  "Blog root path.")
(defvar blog-org-path nil
  "Blog org path.")
(defvar blog-md-path nil
  "Blog md path.")
(defvar blog-draft-path nil
  "Blog draft path.")

(defvar blog-org-template "#+TITLE: %s
#+AUTHOR: honmaple
#+DATE: %s
#+CATEGORY:
#+PROPERTY: MODIFIED
#+PROPERTY: TAGS
#+PROPERTY: SLUG %s
#+PROPERTY: SUMMARY
"
  "Template for org post.")

(defvar blog-md-template "Title: %s
Author: honmaple
Date: %s
Modified:
Category:
Tags:
Slug: %s
Summary:
"
  "Template for markdown post.")

(defface blog-button-face
  '((t (:underline nil :inherit font-lock-keyword-face)))
  "Blog button face.")

(defun blog-source()
  "Get post sources."
  (mapcar
   (lambda (post)
     "Convert info into datesource"
     (let* ((info (blog--read-info post))
            (title (plist-get info :title))
            (publish (plist-get info :publish))
            (category (plist-get info :category))
            (tags (plist-get info :tags))
            (date (plist-get info :date)))
       (list post
             (vector
              `(,date
                face font-lock-comment-face
                follow-link t
                action (lambda(&optional _) (find-file (tabulated-list-get-id))))
              `(,title
                face blog-button-face
                follow-link t
                action (lambda(&optional _) (find-file (tabulated-list-get-id))))
              `(,publish
                face ,(if (string= publish "YES")
                          'font-lock-comment-face
                        'blog-button-face)
                action (lambda(&optional _) (blog-publish-or-unpublish)))
              `(,category
                face blog-button-face
                action (lambda(&optional _) (blog-refresh ,category)))
              `(,tags
                face blog-button-face)))))
   (apply 'append
          (mapcar
           (lambda (append-path)
             "scan files with append-path"
             (directory-files (blog--full-path append-path)
                              t "^[^.]*\\.\\(org\\|md\\|markdown\\)$"))
           (list blog-org-path blog-md-path blog-draft-path)))))


(defun blog--read-info (post)
  "Read info of pelican POST."
  (let ((info (if (s-ends-with? ".org" post)
                  (blog--read-org-info post)
                (blog--read-md-info post))))
    (if (blog--is-in-drafts post)
        (plist-put info :publish "NO")
      (plist-put info :publish "YES"))
    (plist-put info :date (blog--format-datetime (plist-get info :date)))))

(defun blog--read-org-info (post)
  "Read info of org POST."
  (with-temp-buffer
    (insert-file-contents post)
    (let ((title (s-chop-prefix "#+TITLE:" (car (s-match "^#\\+TITLE:.*?\n" (buffer-string)))))
          (category (s-chop-prefix "#+CATEGORY:" (car (s-match "^#\\+CATEGORY:.*?\n" (buffer-string)))))
          (tags (s-chop-prefix "#+PROPERTY: TAGS" (car (s-match "^#\\+PROPERTY: TAGS.*?\n" (buffer-string)))))
          (date (s-chop-prefix "#+DATE:" (car (s-match "^#\\+DATE:.*?\n" (buffer-string))))))
      (list :title (s-trim title)
            :category (s-trim category)
            :tags (s-trim tags)
            :date (s-trim date)))))

(defun blog--read-md-info (post)
  "Read info of markdown POST."
  (with-temp-buffer
    (insert-file-contents post)
    (let ((title (s-chop-prefix "Title:" (car (s-match "^Title:.*?\n" (buffer-string)))))
          (category (s-chop-prefix "Category: " (car (s-match "^Category:.*?\n" (buffer-string)))))
          (tags (s-chop-prefix "Tags:" (car (s-match "^Tags:.*?\n" (buffer-string)))))
          (date (s-chop-prefix "Date:" (car (s-match "^Date:.*?\n" (buffer-string))))))
      (list :title (s-trim title)
            :category (s-trim category)
            :tags (s-trim tags)
            :date (s-trim date)))))

(defun blog--format-datetime (datetime)
  "Format DATETIME."
  (let* ((datetime-in-plist
          (when (not (stringp datetime))
            (plist-get (plist-get (car datetime) 'timestamp) :raw-value)))
         (datetime-str (cond
                        ((eq datetime nil) "") ;; nil
                        ((stringp datetime-in-plist) datetime-in-plist)
                        ((stringp datetime) datetime)
                        (t (car datetime))))
         (l (parse-time-string datetime-str)))
    (if (equal l '(nil nil nil nil nil nil nil nil nil)) ;; can't parse
        "Can't Parse"
      (format-time-string "%Y-%m-%d %H:%M"
                          (encode-time 0 (or (nth 1 l) 0) (or (nth 2 l) 0) (nth 3 l) (nth 4 l) (nth 5 l))))))

(defun blog--full-path (path)
  "Return full absolute path base on PATH."
  (expand-file-name
   (concat (file-name-as-directory blog-root-path) path)))

(defun blog--is-in-drafts (path)
  "Check PATH is in drafts."
  (s-starts-with?
   (blog--full-path blog-draft-path) path))

(defun blog-publish-or-unpublish ()
  "Switch between publish and draft."
  (interactive)
  (let* ((point (point))
         (path (tabulated-list-get-id))
         (dest-path (if (blog--is-in-drafts path)
                        (if (s-ends-with? ".org" path)
                            blog-org-path blog-md-path)
                      blog-draft-path)))
    (when (file-exists-p path)
      (rename-file path (expand-file-name
                         (file-name-nondirectory path)
                         (blog--full-path dest-path)))
      (let ((buffer (find-buffer-visiting path)))
        (when buffer (kill-buffer buffer))))
    (blog-refresh) (goto-char point)))

(defun blog-new-post (filename)
  "New post with FILENAME."
  (interactive "sPost's filename(new-post.org, new-post.md etc):")
  (if (or (s-ends-with? ".org" filename) (s-ends-with? ".md" filename))
      (let ((file-path (expand-file-name filename (blog--full-path blog-draft-path))))
        (find-file file-path)
        (insert
         (format
          (if (s-ends-with? ".org" filename) blog-org-template blog-md-template)
          (file-name-base filename)
          (format-time-string "%F %T" (current-time))
          (file-name-base filename))))
    (message "Post's filename must end with .org or .md!")))

(defun blog-delete-post ()
  "Delete post."
  (interactive)
  (let ((file-path (tabulated-list-get-id)))
    (when (y-or-n-p (format "Do you really want to delete %s ? " file-path))
      (delete-file file-path t)

      (let ((buffer (find-buffer-visiting file-path)))
        (when buffer (kill-buffer buffer)))
      ;; remove asset directory if exist
      (let ((dir-path (file-name-sans-extension file-path)))
        (if (file-exists-p dir-path)
            (delete-directory dir-path t)))
      (blog-refresh))))

(defun blog-refresh(&optional keyword remember-pos update)
  "Refresh with &optional KEYWORD REMEMBER-POS UPDATE."
  (interactive)
  (let ((posts (blog-source))
        (filter-keyword (or keyword blog-filter-keyword)))
    (when filter-keyword
      (setq posts (cl-remove-if-not
                   (lambda (x)
                     (let* ((row (append (car (cdr x)) nil))
                            (title (car (nth 1 row)))
                            (category (car (nth 3 row))))
                       (s-contains? keyword (mapconcat #'identity (list title category) "|") t)))
                   posts)))
    (setq tabulated-list-sort-key '("Date" . t))
    (setq tabulated-list-entries posts)
    (tabulated-list-print remember-pos update)))

(defun blog-filter()
  "Filter posts."
  (interactive)
  (blog-refresh (read-from-minibuffer "Search filter: ")))

(defvar blog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "q" 'quit-window)
    (define-key map "r" 'blog-refresh)
    (define-key map "s" 'blog-publish-or-unpublish)
    (define-key map "D" 'blog-delete-post)
    (define-key map "w" 'blog-new-post)
    (define-key map "f" 'blog-filter)
    (define-key map "gg" 'beginning-of-buffer)
    (define-key map "G" 'end-of-buffer)
    (define-key map "l" 'forward-char)
    (define-key map "h" 'backward-char)
    (define-key map "A" 'end-of-line)
    (define-key map "I" 'beginning-of-line)
    map)
  "Blog mode map.")

(define-derived-mode blog-mode tabulated-list-mode "BLOG"
  "Major mode for writing blog.
\\<blog-mode-map>
\\{blog-mode-map}"
  (setq tabulated-list-format [("Date"     16 t)
                               ("Title"    36 t)
                               ("Publish"  16 t)
                               ("Category" 16 t)
                               ("Tags"     0  nil)])
  (tabulated-list-init-header)
  (blog-refresh))

(defun blog-start()
  "Start blog."
  (interactive)
  (with-current-buffer (get-buffer-create blog-buffer-name)
    (blog-mode))
  (switch-to-buffer blog-buffer-name))

(provide 'blog-mode)
;;; blog-mode.el ends here
