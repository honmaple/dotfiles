;;; maple-header.el ---  file header configuration.	-*- lexical-binding: t -*-

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
;; file header configuration.
;;

;;; Code:
(require 'subr-x)

(defvar maple-header/alist
  '(("filename" ".*\\(File Name:\\)\\(.*\\)"
     (file-name-nondirectory (buffer-file-name)))
    ("email" ".*\\(Email:\\)\\(.*\\)" user-mail-address)))

(defvar maple-header/limit 7)

(defun maple-header/action(default)
  "Action with DEFAULT value."
  (let ((beg (match-beginning 2))
        (end (match-end 2)))
    (when (not (string= default (string-trim-left (match-string 2))))
      (goto-char beg)
      (delete-region beg end)
      (insert " " default))))

(defmacro maple-header/define (name &rest args)
  "Define header update with NAME, ARGS."
  (declare (indent 1)
           (doc-string 2))
  (let ((-regex (plist-get args :regex))
        (-default (or (plist-get args :default) ""))
        (-limit (or (plist-get args :limit) maple-header/limit)))
    `(progn
       (defvar ,(intern (format "maple-header/%s-p" name)) t)
       (defun ,(intern (format "maple-header/%s" name)) (&optional current)
         ,(format "Update %s header with regex." name)
         (interactive)
         (if current
             (when (looking-at ,-regex) (maple-header/action ,-default))
           (save-excursion
             (goto-char (point-min))
             (dotimes (_ ,-limit)
               (when (looking-at ,-regex) (maple-header/action ,-default))
               (forward-line 1))))))))

(defun maple-header/update()
  "Header auto update."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (dotimes (_ maple-header/limit)
      (cl-mapcan
       (lambda(item)
         (when (symbol-value (intern (format "maple-header/%s-p" (car item))))
           (funcall (intern (format "maple-header/%s" (car item))) t)))
       maple-header/alist)
      (forward-line 1))))

;;;###autoload
(define-minor-mode maple-header-mode
  "Maple header mode"
  :group      'maple-header
  :init-value nil
  :global     t
  (if (not maple-header-mode)
      (remove-hook 'before-save-hook  'maple-header/update)
    (dolist (item maple-header/alist)
      (eval `(maple-header/define ,(nth 0 item)
               :regex ,(nth 1 item)
               :default ,(nth 2 item))))
    (add-hook 'before-save-hook  'maple-header/update)))

(provide 'maple-header)
;;; maple-header.el ends here
