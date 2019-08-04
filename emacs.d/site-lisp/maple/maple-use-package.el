;;; maple-use-package.el --- define evil bind with use-package.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-modeline

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
;; define evil bind with use-package.
;;
;; (use-package package-name
;;   :evil-bind
;;   (:state normal :map python-mode-map
;;           ("C-c" . run-python))
;;   (:state (normal insert) :map python-mode-map
;;    ("C-s" . run-python))
;;   :evil-leader
;;   ("C-c" . run-python)
;;   (("C-c" . run-python)
;;    ("C-s" . run-python))
;;   (:mode python-mode
;;          ("C-s" . run-python)
;;          ("C-c" . run-python))
;;   :evil-state
;;   (comint-mode . insert)
;;   (sql-interactive-mode . insert)
;;   :custom
;;   (:mode org-mode
;;          company-tooltip-align-annotations nil)
;;   (:mode markdown-mode
;;          company-tooltip-align-annotations nil))
;;

;;; Code:
(require 'use-package)
(require 'maple-keybind)

(add-to-list 'use-package-keywords :evil-bind t)
(add-to-list 'use-package-keywords :evil-leader t)
(add-to-list 'use-package-keywords :evil-state t)
(add-to-list 'use-package-keywords :hydra t)

(defalias 'use-package-normalize/:evil-bind 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-leader 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-state 'use-package-normalize-forms)
(defalias 'use-package-normalize/:hydra 'use-package-normalize-forms)

(defun maple-use-package/plist-get (plist prop)
  "Get the values associated PLIST to PROP, a modified plist."
  (let ((tail plist)
        common
        result)
    (while (and (consp tail) (not (keywordp (car tail))))
      (when (not common) (setq common (list nil)))
      (push (pop tail) common))

    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (append (cl-remove-if nil (list (nreverse common) (nreverse result)))
            ;; maybe define prop multi times
            (when tail (maple-use-package/plist-get tail prop)))))

(defun maple-use-package/evil-state(args)
  "Evil bind ARGS."
  `((evil-set-initial-state ',(car args) ',(cdr args))))

(defun maple-use-package/hydra(args)
  "Evil bind ARGS."
  `((defhydra ,@args)))

(defun maple-use-package/custom-keyword(args)
  "Custom variable with ARGS."
  (pcase (car args)
    (:default `((setq-default ,@(cdr args))))
    (:mode (unless (featurep 'mode-local)
             (require 'mode-local))
           (cl-loop for arg in (maple-use-package/plist-get args :mode) collect
                    `(setq-mode-local ,(car arg) ,@(cdr arg))))
    (:face `((custom-set-faces ,@(cdr args))))))

(defun maple-use-package/custom(args)
  "Custom variable with ARGS."
  (if (keywordp (car args))
      (maple-use-package/custom-keyword args)
    (let ((variable (nth 0 args))
          (value (nth 1 args))
          (comment (nth 2 args)))
      (unless (and comment (stringp comment))
        (setq comment (format "Customized %s with use-package" variable)))
      `((customize-set-variable (quote ,variable) ,value ,comment)))))

(defun use-package-handler/:evil-bind (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil
       ,@(mapcan 'maple-keybind/evil-bind args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-leader (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil-leader
       ,@(mapcan 'maple-keybind/evil-leader args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-state (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil
       ,@(mapcan 'maple-use-package/evil-state args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:custom (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   (mapcan 'maple-use-package/custom args)
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:hydra (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'hydra
       ,@(mapcan 'maple-use-package/hydra args)))
   (use-package-process-keywords name rest state)))

(provide 'maple-use-package)
;;; maple-use-package.el ends here
