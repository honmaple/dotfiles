;;; init-tool.el --- Initialize tool configurations.	-*- lexical-binding: t -*-

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
;; TOOL configurations.
;;

;;; Code:

(use-package docker-tramp)
(use-package dockerfile-mode)
(use-package nginx-mode)

(use-package esup
  :config
  (maple/evil-map esup-mode-map))

(use-package pangu-spacing
  :commands (pangu-spacing-space-current-buffer)
  :config
  (defun pangu-spacing-search-and-replace (match regexp)
    "Replace regexp with match in buffer."
    (let* ((p (use-region-p))
           (start (if p (region-beginning) (point-min)))
           (end (if p (region-end) (point-max))))
      (pangu-spacing-search-buffer regexp start end (replace-match match nil nil)))))

(use-package quickrun
  :hook (quickrun--mode . maple/truncate-lines)
  :config
  (maple/evil-map quickrun--mode-map))

(use-package maple-note
  :ensure nil
  :commands maple-note
  :config
  (setq maple-note-root-path "~/git/pelican"
        maple-note-draft-path "content/draft")
  (maple/evil-map maple-note-mode-map))

(use-package maple-imenu
  :ensure nil
  :commands (maple-imenu)
  :config
  (maple/evil-map maple-imenu-mode-map)
  (setq maple-imenu-display-alist '((side . left) (slot . -1))))

(use-package youdao-dictionary
  :config
  (setq url-automatic-caching t
        youdao-dictionary-search-history-file (concat maple-cache-directory "youdao")
        youdao-dictionary-use-chinese-word-segmentation t)
  (maple/evil-map youdao-dictionary-mode-map))

(use-package avy
  :commands (avy-pop-mark)
  :config
  (setq avy-all-windows 'all-frames
        avy-background t))

(use-package figlet)

(use-package 2048-game
  :config
  (maple/evil-map 2048-mode-map))

(use-package maple-search
  :ensure nil
  :hook (maple-init . maple/search-init))

(use-package maple-scratch
  :ensure nil
  :hook (window-setup . maple-scratch-mode)
  :defines (maple-scratch-mode-map maple-scratch-alist maple-scratch-source)
  :config
  (maple/evil-map maple-scratch-mode-map)
  (setq maple-scratch-alist
        (append (butlast maple-scratch-alist)
                '(("Init"
                   :action 'maple-file/open-init
                   :desc "Open Init File")
                  ("Test"
                   :action 'maple-file/open-test
                   :desc "Open Test File"))
                (last maple-scratch-alist))
        maple-scratch-source nil))

(use-package maple-echoarea
  :ensure nil
  :commands (maple-echoarea-enable maple-echoarea-disable))

(use-package maple-tabbar
  :ensure nil
  :commands (maple-tabbar-mode))

(provide 'init-tool)
;;; init-tool.el ends here
