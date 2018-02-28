;; init-basic.el --- Auto insert file header.

;;; Commentary:

;;; Code:

(use-package autoinsert
  :ensure nil
  :hook (after-init . auto-insert-mode)
  :config
  (progn
    (defun maple/insert-string(&optional prefix)
      (or prefix (setq prefix comment-start))
      (mapconcat
       (lambda (x) (concat prefix x))
       (split-string
        (concat
         (make-string 80 ?*)
         "\n"
         "Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
         "File Name: " (file-name-nondirectory buffer-file-name) "\n"
         "Author: " (user-full-name)"\n"
         "Email: " user-mail-address "\n"
         "Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
         "Last Update: \n"
         "         By: \n"
         "Description: \n"
         (make-string 80 ?*)) "\n") "\n"))

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
             (string-trim-left (maple/insert-string " ")) "*/\n")))
    )
  )


(use-package time-stamp
  :ensure nil
  :hook (before-save . time-stamp)
  :config
  (setq time-stamp-active t
        time-stamp-line-limit 11
        time-stamp-start "[lL]ast[ -][uU]pdate[ \t]*:?"
        time-stamp-end "\n"
        time-stamp-format (concat
                           " "
                           (car (rassq (string-to-number (format-time-string "%w"))
                                       url-weekday-alist))
                           " %Y-%02m-%02d %02H:%02M:%02S (%Z)"))
  :mode-setq
  (org-mode
   time-stamp-start "MODIFIED[ \t]*?"
   time-stamp-format " %Y-%02m-%02d %02H:%02M:%02S")
  (markdown-mode
   time-stamp-start "Modified[ \t]*:?"))

(use-package header
  :demand t
  :load-path "site-lisp/header"
  :config
  (setq maple//header-update-filename t
        maple//header-update-email nil)
  (add-hook 'before-save-hook 'maple/header-auto-update))

(provide 'init-auto-insert)

;;; init-auto-insert.el ends here
