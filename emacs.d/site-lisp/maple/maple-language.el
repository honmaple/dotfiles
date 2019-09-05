;;; maple-language.el --- Initialize language configurations.	-*- lexical-binding: t -*-

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
;; language configurations.
;;

;;; Code:

(defgroup maple-language nil
  "Display minibuffer with another frame."
  :group 'maple)

(defcustom maple-language:run nil
  "Language run script."
  :type 'function
  :group 'maple-language)

(defcustom maple-language:fold 'maple-language:default-fold
  "Language toggle fold."
  :type 'function
  :group 'maple-language)

(defcustom maple-language:indent 'maple-language:default-indent
  "Language call indent format."
  :type 'function
  :group 'maple-language)

(defcustom maple-language:definition 'maple-language:default-definition
  "Language find definition."
  :type 'function
  :group 'maple-language)

(defcustom maple-language:references 'xref-find-references
  "Language find references."
  :type 'function
  :group 'maple-language)

(defcustom maple-language:documentation nil
  "Language find documentation."
  :type 'function
  :group 'maple-language)

(defmacro maple-language:define (mode &rest args)
  "Language define with MODE ARGS."
  (declare (indent defun))
  (let ((hook (intern (format "%s-hook" mode)))
        (run (plist-get args :run))
        (fold (plist-get args :fold))
        (indent (plist-get args :indent))
        (definition (plist-get args :definition))
        (references (plist-get args :references))
        (documentation (plist-get args :documentation)))
    (add-hook
     `,hook
     `(lambda()
        (when ,run (setq-local maple-language:run ,run))
        (when ,fold (setq-local maple-language:fold ,fold))
        (when ,indent (setq-local maple-language:indent ,indent))
        (when ,definition (setq-local maple-language:definition ,definition))
        (when ,references (setq-local maple-language:references ,references))
        (when ,documentation (setq-local maple-language:documentation ,documentation))))))

(defun maple-language:default-indent()
  "Call default indent."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))

(defun maple-language:default-fold()
  "Call default fold."
  (interactive)
  (call-interactively (cond ((bound-and-true-p evil-mode)
                             'evil-toggle-fold)
                            (t 'hs-toggle-hiding))))

(defun maple-language:default-definition()
  "Call default definition."
  (interactive)
  (call-interactively (cond ((bound-and-true-p lsp-mode)
                             'lsp-find-definition)
                            ((bound-and-true-p evil-mode)
                             'evil-goto-definition)
                            (t 'xref-find-definitions))))

(defun maple-language:call-run()
  "Call run."
  (interactive)
  (call-interactively maple-language:run))

(defun maple-language:call-fold()
  "Call fold."
  (interactive)
  (call-interactively maple-language:fold))

(defun maple-language:call-indent()
  "Call indent."
  (interactive)
  (call-interactively maple-language:indent))

(defun maple-language:call-definition()
  "Call definition."
  (interactive)
  (call-interactively maple-language:definition))

(defun maple-language:call-references()
  "Call references."
  (interactive)
  (call-interactively maple-language:references))

(defun maple-language:call-documentation()
  "Call documentation."
  (interactive)
  (call-interactively maple-language:documentation))

;;;###autoload
(define-minor-mode maple-language-mode
  "maple header mode"
  :group      'maple-language
  :global     t
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      "gd" 'maple-language:call-definition))
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "=" 'maple-language:call-indent))
  (global-set-key [f5] 'maple-language:call-run)
  (global-set-key [f6] 'maple-language:call-indent))

(provide 'maple-language)
;;; maple-language.el ends here
