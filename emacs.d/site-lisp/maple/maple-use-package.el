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
;;   (normal python-mode-map
;;           ("C-c" . run-python))
;;   ((normal insert) python-mode-map
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
;;   :setq
;;   (:mode org-mode
;;          company-tooltip-align-annotations nil)
;;   (:mode markdown-mode
;;          company-tooltip-align-annotations nil))
;;

;;; Code:
(require 'use-package)

(add-to-list 'use-package-keywords :evil-bind t)
(add-to-list 'use-package-keywords :evil-leader t)
(add-to-list 'use-package-keywords :evil-state t)
(add-to-list 'use-package-keywords :setq t)

(defalias 'use-package-normalize/:evil-bind 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-leader 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-state 'use-package-normalize-forms)
(defalias 'use-package-normalize/:setq 'use-package-normalize-forms)

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
    ;; maybe define prop multi times
    (if tail
        (append (list (nreverse common) (nreverse result))
                (maple-use-package/plist-get tail prop))
      (cl-remove-if nil (list (nreverse common) (nreverse result))))))

(defun maple-use-package/keybind(args)
  "Evil bind ARGS."
  (mapcan
   (lambda(arg)
     (let ((key (car arg)))
       (list (if (char-or-string-p key) (kbd key) key) `',(cdr arg))))
   args))

(defun maple-use-package/evil-bind(args)
  "Evil bind ARGS."
  (let ((state (pop args))
        (map (pop args)))
    `(with-eval-after-load 'evil
       (evil-define-key ',state ,map ,@(maple-use-package/keybind args)))))

(defun maple-use-package/evil-leader(args)
  "Evil bind ARGS."
  (cl-loop for bind in (maple-use-package/plist-get (if (stringp (car args)) (list args) args) :mode) collect
           (if (car bind)
               `(with-eval-after-load 'evil-leader
                  (evil-leader/set-key-for-mode ,(car bind) ,@(maple-use-package/keybind (cdr bind))))
             `(with-eval-after-load 'evil-leader
                (evil-leader/set-key ,@(maple-use-package/keybind (cdr bind)))))))

(defun maple-use-package/evil-state(args)
  "Evil bind ARGS."
  `(with-eval-after-load 'evil
     (evil-set-initial-state ',(car args) ',(cdr args))))

(defun maple-use-package/setq(args)
  "Evil bind ARGS."
  (when (not (featurep 'mode-local))
    (require 'mode-local))
  (cl-loop for arg in (maple-use-package/plist-get args :mode) collect
           `(setq-mode-local ,(car arg) ,@(cdr arg))))

(defun use-package-handler/:evil-bind (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   (mapcar 'maple-use-package/evil-bind args)
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-leader (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   (mapcan 'maple-use-package/evil-leader args)
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-state (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   (mapcar 'maple-use-package/evil-state args)
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:setq (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   (mapcan 'maple-use-package/setq args)
   (use-package-process-keywords name rest state)))

(provide 'maple-use-package)
;;; maple-use-package.el ends here
