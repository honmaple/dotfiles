; (require-package 'header2)
; (require 'header2)
(auto-insert-mode)
(setq auto-insert-query nil)
(setq user-full-name "jianglin")
; (defsubst my/header-blank ()
          ; (insert header-prefix-string "**************************************************************************\n"))

; (setq user-full-name "jianglin")
; (setq header-copyright-notice "Copyright © 2015-2016 jianglin.\n")
; (setq make-header-hook '(my/header-blank
                          ; header-copyright
                          ; header-file-name
                          ; header-author
                          ; header-creation-date
                          ; header-modification-date
                          ; header-modification-author
                          ; header-update-count
                          ; header-description
                          ; my/header-blank))

; (add-hook 'emacs-lisp-mode-hook #'auto-make-header)
; (add-hook 'python-mode-hook #'auto-make-header)
; (add-hook 'write-file-hooks 'auto-update-file-header)

(setq auto-insert-alist
      '(((ruby-mode . "Ruby program") nil
                                      "# !/usr/bin/env ruby\n"
                                      "# -*- encoding:UTF-8 -*-\n"
                                      "# **************************************************************************\n"
                                      "# Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
                                      "# File Name: " (file-name-nondirectory buffer-file-name) "\n"
                                      "# Author: " (user-full-name)"\n"
                                      "# Email: xiyang0807@gmail.com\n"
                                      "# Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
                                      "# Last Update: \n"
                                      "#          By: \n"
                                      "# Description: \n"
                                      "# **************************************************************************\n")
        ((python-mode . "Python program") nil
                                          "# !/usr/bin/env python\n"
                                          "# -*- coding=UTF-8 -*-\n"
                                          "# **************************************************************************\n"
                                          "# Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
                                          "# File Name: " (file-name-nondirectory buffer-file-name) "\n"
                                          "# Author: " (user-full-name)"\n"
                                          "# Email: xiyang0807@gmail.com\n"
                                          "# Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
                                          "# Last Update: \n"
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
        ((markdown-mode . "Markdown") nil
                                      "Title: " (file-name-nondirectory buffer-file-name) "  \n"
                                      "Author: honmaple   \n"
                                      "Date: " (format-time-string "%F" (current-time)) "\n"
                                      "Category:   \n"
                                      "Tags: []  \n"
                                      "Slug: " (file-name-nondirectory buffer-file-name) "  \n"
                                      "Summary: ")))

(setq time-stamp-active t)
(setq time-stamp-line-limit 11)
(setq time-stamp-start "[lL]ast[ -][uU]pdate[ \t]*:[  \t]+\\\\?")
(setq time-stamp-end "\n")
(setq time-stamp-format "%#A %Y-%:m-%:d %:H:%:M:%:S (%Z)")
(add-hook 'write-file-hooks 'time-stamp)

(provide 'init-auto-insert)

