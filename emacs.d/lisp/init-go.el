;;; init-gui.el --- Initialize golang configurations.	-*- lexical-binding: t -*-

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
;; Golang configurations.
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/haya14busa/gopkgs/cmd/gopkgs
;;

;;; Code:

(use-package go-mode
  :config
  (setq gofmt-show-errors nil
        go-packages-function 'maple/go-packages-function)

  (defun maple/go-packages-function()
    "Return a list of all Go packages, using `gopkgs'."
    (sort (process-lines "gopkgs") #'string<))

  (use-package golint)
  (use-package go-rename)

  (use-package go-eldoc
    :unless *lsp*
    :hook (go-mode . go-eldoc-setup))

  (use-package company-go
    :unless *lsp*
    :functions maple/company-backend
    :init (maple/company-backend 'go-mode-hook '(company-go company-keywords)))

  (defun maple/go-auto-comment()
    (interactive)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (let* ((imenu-max-item-length "Unlimited")
           (imenu-auto-rescan t)
           (imenu-auto-rescan-maxout (if current-prefix-arg
                                         (buffer-size)
                                       imenu-auto-rescan-maxout))
           (items (imenu--make-index-alist t))
           (items (delete (assoc "*Rescan*" items) items)))
      (dolist (item items)
        (cl-mapcan
         (if (string= (car item) "func")
             'maple/go-func-comment
           'maple/go-type-comment)
         (cdr item)))))

  (defun maple/go-add-comment(func point)
    (save-excursion
      (goto-char point)
      (forward-line -1)
      (when (not (looking-at (concat "// " func)))
        (end-of-line) (newline-and-indent)
        (insert (concat "// " func " ..")))))

  (defun maple/go-func-comment(f)
    (let* ((point (cdr f))
           (func (car f))
           (func (if (and (string-prefix-p "(" func)
                          (string-match "[)] \\(.*?\\)[(]\\(.*?\\)[)]\\(.*\\)$" func))
                     (match-string 1 func)
                   (if (string-match "\\(.*?\\)[(]\\(.*?\\)[)]\\(.*\\)$" func)
                       (match-string 1 func) func))))
      (maple/go-add-comment func point)))

  (defun maple/go-type-comment(f)
    (maple/go-add-comment (car f) (cdr f)))

  :custom
  (:language
   "go-mode"
   :indent 'gofmt
   :definition 'godef-jump))

(provide 'init-go)
;;; init-go.el ends here
