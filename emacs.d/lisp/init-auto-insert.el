(use-package autoinsert
  :defer t
  :init (auto-insert-mode)
  :config
  (progn
    (setq auto-insert-query nil)
    (setq auto-insert-alist
          '(((ruby-mode . "Ruby program") nil
             "#!/usr/bin/env ruby\n"
             "# -*- encoding: utf-8 -*-\n"
             "# **************************************************************************\n"
             "# Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
             "# File Name:" (file-name-nondirectory buffer-file-name) "\n"
             "# Author: " (user-full-name)"\n"
             "# Email: xiyang0807@gmail.com\n"
             "# Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
             "# Last Update: \n"
             "#          By: \n"
             "# Description: \n"
             "# **************************************************************************\n")
            ((python-mode . "Python program") nil
             "#!/usr/bin/env python\n"
             "# -*- coding: utf-8 -*-\n"
             "# **************************************************************************\n"
             "# Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
             "# File Name: " (file-name-nondirectory buffer-file-name) "\n"
             "# Author: " (user-full-name)"\n"
             "# Email: xiyang0807@gmail.com\n"
             "# Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
             "# Last Update: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
             "#          By: \n"
             "# Description: \n"
             "# **************************************************************************\n")
            ((c-mode . "C program") nil
             "/**************************************************************************\n"
             " Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
             " File Name: " (file-name-nondirectory buffer-file-name) "\n"
             " Author: " (user-full-name)"\n"
             " Email: xiyang0807@gmail.com\n"
             " Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
             " Last Update: \n"
             "           By: \n"
             " Description: \n"
             " **************************************************************************/\n"
             "#include<stdio.h>\n"
             "#include<string.h>\n")
            ((sh-mode . "Shell script") nil
             "#!/bin/bash\n"
             "#**************************************************************************\n"
             "#Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
             "#File Name: " (file-name-nondirectory buffer-file-name) "\n"
             "#Author: " (user-full-name)"\n"
             "#Email: xiyang0807@gmail.com\n"
             "#Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
             "#Last Update: \n"
             "#          By: \n"
             "#Description: \n"
             "#**************************************************************************/\n")
            ((org-mode . "org-mode") nil
             "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"../css/worg.css\" />" "\n"
             "Title: " (file-name-base buffer-file-name) "\n"
             "Author: honmaple   \n"
             "Date: " (format-time-string "%F" (current-time)) "\n"
             "Category:   \n"
             "Tags:   \n"
             "Slug: " (file-name-base buffer-file-name) "\n"
             "Summary: ")
            ;; ((markdown-mode . "Markdown") nil
            ;;  "Title: " (file-name-base buffer-file-name) "  \n"
            ;;  "Author: honmaple   \n"
            ;;  "Date: " (format-time-string "%F" (current-time)) "\n"
            ;;  "Category:   \n"
            ;;  "Tags:   \n"
            ;;  "Slug: " (file-name-base buffer-file-name) "  \n"
            ;;  "Summary: ")
            ((rst-mode . "reStructuredText") nil
             (file-name-nondirectory buffer-file-name) "  \n"
             "##############\n"
             ":author: honmaple   \n"
             ":date: " (format-time-string "%F" (current-time)) "\n"
             ":category:   \n"
             ":tags:   \n"
             ":slug: " (file-name-nondirectory buffer-file-name) "  \n"
             ":summary: ")))
    )
  )


(use-package time-stamp
  :defer t
  :init (add-hook 'before-save-hook 'time-stamp)
  :config
  (progn
    (setq time-stamp-active t)
    (setq time-stamp-line-limit 11)
    (setq time-stamp-start "[lL]ast[ -][uU]pdate[ \t]*:?")
    (setq time-stamp-end "\n")
    (setq time-stamp-format "%#A %Y-%:m-%:d %:H:%:M:%:S (%Z)")
    ))

(provide 'init-auto-insert)

