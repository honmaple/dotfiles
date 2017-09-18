(use-package autoinsert
  :defer t
  :init (auto-insert-mode)
  :config
  (progn
    (defun maple//insert-string()
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
       (make-string 80 ?*)))

    (defun maple/insert-string(&optional prefix)
      (or prefix (setq prefix comment-start))
      (mapconcat
       (lambda (x) (concat prefix x))
       (split-string (maple//insert-string) "\n") "\n"))

    (setq auto-insert-query nil)
    (setq auto-insert-alist
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
  :defer t
  :init (add-hook 'before-save-hook 'time-stamp)
  :mode-setq
  (org-mode
   time-stamp-start "MODIFIED[ \t]*?"
   time-stamp-format " %Y-%02m-%02d %02H:%02M:%02S")
  (markdown-mode
   time-stamp-start "Modified[ \t]*:?")
  :config
  (progn
    (setq time-stamp-active t)
    (setq time-stamp-line-limit 11)
    (setq time-stamp-start "[lL]ast[ -][uU]pdate[ \t]*:?")
    (setq time-stamp-end "\n")
    (setq time-stamp-format " %#A %Y-%02m-%02d %02H:%02M:%02S (%Z)")
    ))

(use-package header
  :load-path "site-lisp/header"
  :config
  (progn
    (setq maple//header-update-filename t
          maple//header-update-email nil)
    (add-hook 'before-save-hook 'maple/header-auto-update)))

(provide 'init-auto-insert)

