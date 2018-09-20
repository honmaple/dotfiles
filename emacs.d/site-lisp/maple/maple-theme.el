;;; maple-theme.el ---  theme configuration.	-*- lexical-binding: t -*-

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
;; theme configuration.
;;

;;; Code:

;; (defvar  maple/theme-alist (mapcar 'symbol-name (custom-available-themes)))
;; (defvar  maple/theme-alist (delete "doom-one-light"
;;                                  (mapcar 'symbol-name (custom-available-themes))))
(defvar maple/theme-alist '("monokai"
                            "spacemacs-dark"
                            "solarized-light"
                            "solarized-dark"
                            "doom-molokai"
                            "doom-one"
                            "doom-peacock"
                            "doom-vibrant"))

(defun maple/theme-cycle (num)
  "Theme switch with NUM."
  (let ((index (+ num
                  (cl-position
                   (car (mapcar 'symbol-name custom-enabled-themes)) maple/theme-alist :test 'equal)))
        (len (length maple/theme-alist)))
    (when (>= index len)
      (setq index 0))
    (when (eq index -1)
      (setq index (1- len)))
    (let ((maple-current-theme (nth index maple/theme-alist)))
      (mapc 'disable-theme custom-enabled-themes)
      (let ((progress-reporter
             (make-progress-reporter
              (format "Loading theme %s..." maple-current-theme))))
        (load-theme (intern maple-current-theme) t)
        (progress-reporter-done progress-reporter)))))

;;;###autoload
(defun maple/theme-next()
  "Next theme."
  (interactive)
  (maple/theme-cycle 1))

;;;###autoload
(defun maple/theme-previous()
  "Previous theme."
  (interactive)
  (maple/theme-cycle -1))

(provide 'maple-theme)
;;; maple-theme.el ends here
