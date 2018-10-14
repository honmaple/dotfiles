;;; init-tool.el --- Initialize tool configurations.	-*- lexical-binding: t -*-

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
;; TOOL configurations.
;;

;;; Code:

(use-package ctable)
(use-package docker-tramp)
(use-package dockerfile-mode)
(use-package nginx-mode)

(use-package esup
  :config
  (maple/evil-map esup-mode-map))

;; (use-package maple-minimap
;;   :ensure nil
;;   :commands (minimap-mode)
;;   :config
;;   (maple/evil-map minimap-mode-map))

(use-package pangu-spacing
  :commands (pangu-spacing-space-current-buffer))

(use-package quickrun
  :hook (quickrun--mode . maple/truncate-lines)
  :config
  (maple/evil-map quickrun--mode-map))

(use-package blog-admin
  :load-path "site-lisp/blog-admin"
  :commands blog-admin-start
  :config
  (setq blog-admin-backend-type 'pelican
        blog-admin-backend-new-post-in-drafts t ;; create new post in drafts by default
        blog-admin-backend-new-post-with-same-name-dir nil ;; create same-name directory with new post
        blog-admin-backend-path "~/git/pelican"
        blog-admin-backend-pelican-config-file "pelicanconf.py"
        blog-admin-backend-pelican-posts-dir "content/org"
        blog-admin-backend-pelican-org-mode-dir "content/org"
        blog-admin-backend-pelican-markdown-dir "content/markdown"
        blog-admin-backend-pelican-drafts-dir "content/draft")
  (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)

  (maple/add-hook 'blog-admin-mode-hook
    :if (display-graphic-p)
    ;; (set-face-attribute 'variable-pitch nil :font "-Sony-Sony Fixed-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (set-face-attribute 'variable-pitch nil :font "Inconsolata 12")
    (buffer-face-mode)))

(use-package imenu-list
  :commands (imenu-list-minor-mode)
  :config
  (maple/evil-map imenu-list-major-mode-map)
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-mode-line-format mode-line-format)
  :custom-face
  (imenu-list-entry-face-0 ((t (:foreground "#f92672"))))
  :bind (:map evil-leader--default-map
              ("bi" . imenu-list-minor-mode)
              :map imenu-list-major-mode-map
              ("d" . imenu-list-display-entry)
              ("q" . imenu-list-minor-mode)
              ("tb" . imenu-list-minor-mode)
              ("j" . next-line)
              ("k" . previous-line)))

(use-package youdao-dictionary
  :config
  (setq url-automatic-caching t
        youdao-dictionary-search-history-file (concat maple-cache-directory "youdao")
        youdao-dictionary-use-chinese-word-segmentation t)
  (maple/evil-map youdao-dictionary-mode-map))

;; (use-package cal-china-x
;;   :config
;;   (setq mark-holidays-in-calendar t)
;;   (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
;;   (setq calendar-holidays cal-china-x-important-holidays))

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
  :load-path "site-lisp/maple"
  :hook (window-setup . maple-scratch-mode)
  :config (setq maple/scratch-source nil))

(provide 'init-tool)
;;; init-tool.el ends here
