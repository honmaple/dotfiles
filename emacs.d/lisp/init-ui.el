;;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

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
;; UI configurations.
;;

;;; Code:

(eval-when-compile (require 'init-basic))

(use-package monokai-theme)
(use-package solarized-theme)
(use-package spacemacs-theme)
(use-package doom-themes)

(maple/add-hook 'after-init-hook
  (load-theme user-default-theme t))

;; (use-package mapleline
;;   :load-path "site-lisp/maple"
;;   :hook (after-init . mapleline-default-theme)
;;   :config
;;   (setq powerline-default-separator 'wave))

;; (use-package powerline
;;   :hook (after-init . powerline-center-evil-theme))

(use-package spaceline-config
  :ensure spaceline
  :hook (after-init . spaceline-spacemacs-theme)
  :config
  (setq spaceline-byte-compile nil)
  (setq powerline-default-separator 'wave
        spaceline-window-numbers-unicode t
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package hydra
  :config
  (use-package maple-theme
    :commands (maple/cycle-themes/body)
    :load-path "site-lisp/maple"
    :config
    (defhydra maple/cycle-themes ()
      ("n" maple/next-theme "next theme")
      ("p" maple/previous-theme "prev theme"))))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.2
        which-key-allow-evil-operators t)
  (which-key-add-key-based-replacements
    ",f" "file"
    ",b" "buffer"
    ",o" "orgmode"
    ",e" "flycheck error"
    ",j" "avy"
    ",g" "git"
    ",w" "window"
    ",p" "project"
    ",sq" "sql"
    ",t" "toggle mode"))


;; this is ugly
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))

;; (use-package nlinum
;;   :hook ((prog-mode text-mode) . nlinum-mode))

;; (use-package nlinum-relative
;;   :hook (nlinum-mode . nlinum-relative-on)
;;   :config
;;   (setq nlinum-relative-current-symbol ""
;;         nlinum-relative-redisplay-delay 0)
;;   (nlinum-relative-setup-evil))

;; ;;; 80列
(use-package fill-column-indicator
  :config
  (setq fci-rule-column 80
        fci-rule-width 1
        fci-rule-color "#D0BF8F")
  (push '(fci-mode "") minor-mode-alist))

;; 高亮括号
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; 颜色
(use-package rainbow-mode
  :hook ((prog-mode conf-unix-mode) . rainbow-mode)
  :diminish rainbow-mode)


;; 相同字符
(use-package highlight-symbol
  :hook
  ((prog-mode text-mode) . highlight-symbol-nav-mode)
  ((prog-mode text-mode) . highlight-symbol-mode)
  :diminish highlight-symbol-mode)


(use-package volatile-highlights
  :hook (after-init . volatile-highlights-mode)
  :config
  ;; additional extensions
  ;; evil
  (with-eval-after-load 'evil
    (vhl/define-extension 'evil
                          'evil-move
                          'evil-paste-after
                          'evil-paste-before
                          'evil-paste-pop)
    (vhl/install-extension 'evil))
  ;; undo-tree
  (with-eval-after-load 'undo-tree
    (vhl/define-extension 'undo-tree
                          'undo-tree-move
                          'undo-tree-yank)
    (vhl/install-extension 'undo-tree))
  :custom-face (vhl/default-face ((t (:background "Springgreen3" :foreground "#272822"))))
  :diminish volatile-highlights-mode)

;; 显示缩进
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  :diminish highlight-indent-guides-mode)


(use-package whitespace
  :ensure nil
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-action '(auto-cleanup)
        whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))
  :diminish whitespace-mode "ⓦ")

(provide 'init-ui)

;;; init-ui.el ends here
