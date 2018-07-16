;;; init-gui.el --- Initialize golang configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

;; Author: lin.jiang <xiyang0807@gmail.com>
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
;; Golang configurations.
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;;

;;; Code:

(use-package go-mode
  :config
  (setq gofmt-show-errors nil
        go-packages-function 'maple/go-packages-function)

  ;; speed up go-import-add from https://gist.github.com/juergenhoetzel/8107a01039df08ea3f1c208494ddd7bf
  (defun maple/go-packages-function()
    (sort
     (delete-dups
      (cl-mapcan
       (lambda (topdir)
         (let ((pkgdir (concat topdir "/pkg/")))
           (mapcar (lambda (file)
                     (let ((sub (substring file 0 -2)))
                       (mapconcat #'identity (cdr (split-string sub "/")) "/")))
                   (split-string (shell-command-to-string
                                  (format "find \"%s\" -not -path \"%s/tool*\" -not -path \"%s/obj/*\" -name \"*.a\"  -printf \"%%P\\n\""
                                          pkgdir pkgdir pkgdir))))))
       (go-root-and-paths)))
     #'string<))

  (use-package golint)

  (use-package go-eldoc
    :hook (go-mode . go-eldoc-setup))

  (use-package company-go
    :functions maple/company-backend
    :init (maple/company-backend 'go-mode-hook '(company-go company-keywords)))

  :evil-bind
  (normal go-mode-map
          ([f6] . gofmt)
          ("gd" . godef-jump)))


(provide 'init-go)
;;; init-go.el ends here
