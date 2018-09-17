;;; init-auto-insert.el --- Auto insert file header.	-*- lexical-binding: t -*-

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
;; Basic configurations.
;;

;;; Code:

(use-package autoinsert
  :ensure nil
  :hook (prog-mode . auto-insert-mode)
  :config
  (defun maple/insert-string(&optional prefix)
    (replace-regexp-in-string
     "^" (or prefix comment-start)
     (concat
      (make-string 80 ?*) "\n"
      "Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
      "File Name: " (file-name-nondirectory buffer-file-name) "\n"
      "Author: " (user-full-name)"\n"
      "Email: " user-mail-address "\n"
      "Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
      "Last Update: \n"
      "         By: \n"
      "Description: \n"
      (make-string 80 ?*))))

  (setq auto-insert-query nil
        auto-insert-alist
        '(((ruby-mode . "Ruby program") nil
           "#!/usr/bin/env ruby\n"
           "# -*- encoding: utf-8 -*-\n"
           (maple/insert-string) "\n")
          ((python-mode . "Python program") nil
           "#!/usr/bin/env python\n"
           "# -*- coding: utf-8 -*-\n"
           (maple/insert-string) "\n")
          ((c-mode . "C program") nil
           "/*"
           (string-trim-left (maple/insert-string " ")) "*/\n"
           "#include<stdio.h>\n"
           "#include<string.h>\n")
          ((sh-mode . "Shell script") nil
           "#!/bin/bash\n"
           (maple/insert-string) "\n")
          ((go-mode . "Go program") nil
           "/*"
           (string-trim-left (maple/insert-string " ")) "*/\n"))))


(use-package time-stamp
  :ensure nil
  :hook (before-save . time-stamp)
  :config
  (setq time-stamp-active t
        time-stamp-line-limit 11
        time-stamp-start "[lL]ast[ -][uU]pdate[ \t]*:?"
        time-stamp-end "\n"
        time-stamp-format (concat " " (maple/get-weekday)
                                  " %Y-%02m-%02d %02H:%02M:%02S (%Z)"))
  :setq
  (:mode org-mode
         time-stamp-start "MODIFIED[ \t]*?"
         time-stamp-format " %Y-%02m-%02d %02H:%02M:%02S")
  (:mode markdown-mode
         time-stamp-start "Modified[ \t]*:?"))

(use-package header
  :load-path "site-lisp/header"
  :hook (before-save . maple/header-auto-update)
  :config
  (setq maple//header-update-filename t
        maple//header-update-email nil))

(provide 'init-auto-insert)

;;; init-auto-insert.el ends here
