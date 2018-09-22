;;; maple-modeline.el --- modeline configurations.	-*- lexical-binding: t -*-

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
;; modeline configurations.
;;

;;; Code:


(require 'powerline)
(require 'subr-x)

(defvar evil-state)
(defvar evil-previous-state)
(defvar pyvenv-virtual-env-name)

(defvar mapleline-highlight-face-func 'mapleline-get-face)
(defvar mapleline-priority-table (make-hash-table :test 'equal))

(dolist (i '((default "DarkGoldenrod2" "Default highlight face for modeline.")
             (evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (evil-insert "chartreuse3" "Evil insert state face.")
             (evil-emacs "SkyBlue2" "Evil emacs state face.")
             (evil-replace "chocolate" "Evil replace state face.")
             (evil-visual "gray" "Evil visual state face.")
             (evil-motion "plum3" "Evil motion state face.")))
  (eval `(defface ,(intern (format "mapleline-%s-face" (nth 0 i)))
           '((t (:background ,(nth 1 i)
                             :foreground "#3E3D31"
                             :inherit 'mode-line)))
           ,(nth 2 i))))

(defun mapleline-get-table-list (hash-table)
  "Return a list of keys in HASH-TABLE."
  (let ((l '()))
    (maphash (lambda (k v) (push (cons k v) l)) hash-table)
    (sort l (lambda(a b) (< (cdr a) (cdr b))))))

(defun mapleline-get-face ()
  "Get face."
  (if (bound-and-true-p evil-local-mode)
      (intern (format "mapleline-evil-%s-face"
                      (if (eq 'operator evil-state)
                          evil-previous-state
                        evil-state)))
    'mapleline-default-face))

(defmacro mapleline-define (name &rest args)
  "Define modeline with NAME and ARGS."
  (declare (indent 1)
           (doc-string 2))
  (let* ((-if (or (plist-get args :if) t))
         (-format (plist-get args :format))
         (-priority (or (plist-get args :priority) 100))
         (-name (format "%s" name)))
    `(progn
       (puthash ,-name ,-priority mapleline-priority-table)
       (defvar ,(intern (format "mapleline-%s-p" name)) t)
       (defun ,(intern (format "mapleline--%s" name)) (&optional face)
         (when  (and ,-if
                     ,(intern (format "mapleline-%s-p" name))
                     (<= (gethash ,-name mapleline-priority-table) 100))
           (powerline-raw ,-format face))))))

(defmacro mapleline-define-fill(width)
  "Define mapleline--fill with WIDTH."
  `(defun mapleline--fill(&optional face)
     (powerline-fill face ,width)))

(defmacro mapleline-flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  `(let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p ,state))
          (err (or (cdr (assq ,state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running) (format "•%s" err))))


(defun mapleline--string-trim-from-center (str len)
  "Return STR with its center chars trimmed for it to be a maximum length of LEN."
  (if (> (length str) len)
      (let ((mid (/ (- len 3) 2)))
        (concat (substring str 0 mid)
                (apply #'propertize "..." (text-properties-at (- mid 1) str))
                (substring str (- (1+ mid)) nil)))
    str))

(defun mapleline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t str)))

(mapleline-define window-number
  :if (bound-and-true-p window-numbering-mode)
  :format
  (format " %s" (mapleline--unicode-number
                 (int-to-string (window-numbering-get-number)))))

(mapleline-define version-control
  :if (and vc-mode (vc-state (buffer-file-name)))
  :priority 78
  :format
  (format "%s:%s" "Git" (vc-state (buffer-file-name))))

(mapleline-define major-mode
  :priority 75
  :format
  (powerline-major-mode))

(mapleline-define buffer-info
  :format
  (format "%s %s %s"
          (powerline-raw "%*")
          (powerline-buffer-size)
          (string-trim
           (mapleline--string-trim-from-center
            (powerline-buffer-id
             (if (powerline-selected-window-active) 'mode-line-buffer-id
               'mode-line-buffer-id-inactive))
            45))))

(mapleline-define flycheck
  :if (bound-and-true-p flycheck-mode)
  :priority 72
  :format
  (format "%s%s%s"
          (propertize
           (or (mapleline-flycheck-lighter 'info) "")
           'face 'flycheck-fringe-info)
          (propertize
           (or (mapleline-flycheck-lighter 'warning) "")
           'face 'flycheck-fringe-warning)
          (propertize
           (or (mapleline-flycheck-lighter 'error) "")
           'face 'flycheck-fringe-error)))

(mapleline-define python-pyvenv
  :if (and (eq 'python-mode major-mode)
           (bound-and-true-p pyvenv-virtual-env-name))
  :format
  (format "pyvenv:%s" pyvenv-virtual-env-name))

(mapleline-define selection-info
  :if (and (powerline-selected-window-active) (region-active-p))
  :priority 95
  :format
  (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
         (chars (- (1+ (region-end)) (region-beginning)))
         (multi-line (> lines 1)))
    (cond
     (multi-line (format "%d lines" lines))
     (t (format "%d chars" chars)))))

(mapleline-define count
  :priority 75
  :format
  (format "%s | %s:%s"
          (let ((buf-coding (format "%s" buffer-file-coding-system)))
            (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                (match-string 1 buf-coding) buf-coding)) "%l" "%c"))

(mapleline-define screen
  :priority 80
  :format  "%p ")

(mapleline-define hud
  :if powerline-display-hud
  :priority 79
  :format
  (powerline-render (list (powerline-hud 'mapleline-default-face (or face 'powerline-active0)))))

(defun mapleline--separator-left(face-left face-right)
  "Separator-left with FACE-LEFT FACE-RIGHT."
  (let ((func (intern (format "powerline-%s-%s"
                              (powerline-current-separator)
                              (car powerline-default-separator-dir)))))
    (powerline-render
     (list (powerline-raw " " face-left)
           (funcall func face-left face-right)
           (powerline-raw " " face-right)))))

(defun mapleline--separator-right(face-left face-right)
  "Separator-right with FACE-LEFT FACE-RIGHT."
  (let ((func (intern (format "powerline-%s-%s"
                              (powerline-current-separator)
                              (cdr powerline-default-separator-dir)))))
    (powerline-render
     (list (powerline-raw " " face-left)
           (funcall func face-left face-right)
           (powerline-raw " " face-right)))))

(defun mapleline-display(displays face0 face1)
  "Return DISPLAYS FACE0 FACE1."
  (cl-reduce
   (lambda(x y)
     (let* ((odd (cl-oddp (/ (length x) 2)))
            (d (cdr y))
            (face (plist-get d :face))
            (after (if (plist-member d :after) (plist-get d :after) t))
            (ds (funcall (car y) (or face (if odd face0 face1))))
            (sep (if odd (mapleline--separator-right (or face face0) face1)
                   (mapleline--separator-left (or face face1) face0))))
       (if (or (not ds) (string= ds ""))
           (append x)
         (append x (list ds (when after sep))))))
   displays :initial-value (list)))

(defun mapleline-init()
  "Setup the default modeline."
  (let* ((active (powerline-selected-window-active))
         (face0  (if active 'powerline-active0 'powerline-inactive0))
         (face1  (if active 'powerline-active1 'powerline-inactive1))
         ;; (face2  (if active 'powerline-active2  'powerline-inactive2))
         (face3  (if active (funcall mapleline-highlight-face-func)
                   'powerline-inactive1))
         (left `((mapleline--window-number
                  :face ,face3)
                 (mapleline--buffer-info)
                 (mapleline--major-mode)
                 (mapleline--flycheck)
                 (mapleline--version-control)))
         (right '((mapleline--python-pyvenv)
                  (mapleline--selection-info)
                  (mapleline--count)
                  (mapleline--screen
                   :after nil)
                  (mapleline--hud
                   :after nil))))
    (mapleline-define-fill (+ 4.5 (powerline-width (mapleline-display right face0 face1))))
    (mapleline-display (append left '((mapleline--fill)) right) face0 face1)))


(defun mapleline-width-reset()
  "Auto reset modeline width."
  (let* ((p (car (mapleline-get-table-list
                  mapleline-priority-table)))
         (key (car p))
         (value (cdr p)))
    (puthash key (+ value 100) mapleline-priority-table)))

(defun mapleline-theme ()
  "Setup modeline."
  (let* ((display (mapleline-init))
         (width (with-current-buffer (current-buffer)
                  (+ (window-width)
                     (or (cdr (window-margins)) 0)
                     (or (car (window-margins)) 0))))
         (modeline-width (powerline-width display)))
    (while (> modeline-width width)
      (mapleline-width-reset)
      (setq display (mapleline-init))
      (setq modeline-width (powerline-width display)))
    (maphash
     (lambda (key value)
       (when (> value 100)
         (puthash key (- value 100) mapleline-priority-table)))
     mapleline-priority-table)
    display))

(defun mapleline-default-theme ()
  "Setup the default modeline."
  (interactive)
  (setq-default mode-line-format '("%e" (:eval (mapleline-theme)))))

(provide 'maple-modeline)
;;; maple-modeline.el ends here
