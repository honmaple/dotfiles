(require-package 'evil-org)
(require-package 'org-pomodoro)
(require-package 'org-bullets)
(require-package 'ox-reveal)
(require-package 'alert) ;;org-pomodoro依赖

(use-package org
  :mode ("\\.org$" . org-mode)
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
  :defer t
  :config
  (progn
    ;; (setq org-tags-column -100)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org-mode/gtd.org" "Workspace")
             "* TODO [#B] %?\n  %i\n"
             :empty-lines 1)
            ("n" "笔记" entry (file+headline "~/org-mode/notes.org" "我的笔记")
             "* TODO [#C] %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "博客" entry (file+headline "~/org-mode/blog.org" "博客")
             "** TODO [#B] %?     :blog:\n  %i %U"
             :empty-lines 1)
            ("w" "学习" entry (file+headline "~/org-mode/gtd.org" "学习")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 1)
            ("j" "日程安排"
             entry (file+datetree "~/org-mode/journal.org")
             "* %?"
             :empty-lines 1)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (sh . t)
       (python . t)
       (R . t)
       (ruby . t)
       (ditaa . t)
       (dot . t)
       (octave . t)
       (sqlite . t)
       (perl . t)
       (C . t)))
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                  (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                  (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

    (setq org-todo-keyword-faces
          (quote (("NEXT" :inherit warning)
                  ("PROJECT" :inherit font-lock-string-face))))
    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
    ;; (evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
    )
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c l" . org-store-link)))

(use-package evil-org
  :commands evil-org-mode
  :diminish evil-org-mode
  :init
  (add-hook 'org-mode-hook 'evil-org-mode))


;; (setq org-log-done t)

;; 设置默认浏览器

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(use-package org-agenda
  :defer t
  :init (setq org-agenda-restore-windows-after-quit t)
  :config
  (progn
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-files (quote ("~/org-mode" )))
    (setq org-default-notes-file "~/org-mode/gtd.org")
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
            ("b" "博客" tags-todo "blog")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"flask\"")
            ("W" "Weekly Review"
             ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              )))))
  :bind (:map org-agenda-mode-map
              ("j" . org-agenda-next-line)
              ("k" . org-agenda-previous-line)
              ("M-j" . org-agenda-next-item)
              ("M-k" . org-agenda-previous-item)
              ("M-h" . org-agenda-earlier)
              ("M-l" . org-agenda-later)))

;; (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
;; (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)
;; (define-key org-agenda-mode-map (kbd "M-j") 'org-agenda-next-item)
;; (define-key org-agenda-mode-map (kbd "M-k") 'org-agenda-previous-item)
;; (define-key org-agenda-mode-map (kbd "M-h") 'org-agenda-earlier)
;; (define-key org-agenda-mode-map (kbd "M-l") 'org-agenda-later))
;; (define-key org-agenda-mode-map (kbd "gd") 'org-agenda-toggle-time-grid)
;; (define-key org-agenda-mode-map (kbd "gr") 'org-agenda-redo))

(use-package org-bullets
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode)
  :config (setq org-bullets-bullet-list '("①" "②" "③" "④" "⑤"))
  )


(use-package org-pomodoro
  :defer t
  :config
  (progn
    (setq org-pomodoro-keep-killed-pomodoro-time t)
    (after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
      )))

;; org-mode 設定
(use-package org-crypt
  :defer t
  :config
  (progn
    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)
    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")
    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))
    ;; 用於加密的 GPG 金鑰
    ;; (setq org-crypt-key nil)
    (setq org-crypt-key "21305E7E")
    ;; (add-hook 'org-mode-hook (lambda () (run-hooks 'org-mode-hook)))
    ))

(use-package ox-reveal
  :init(setq org-reveal-root "file:///home/jianglin/git/ppt/reveal.js"))

;; 写博客
(defun org-new-blog (title)
  (interactive "sInput Title: ")
  (let ((blog-path (substitute-in-file-name (format "~/git/pelican/content/org/%s.org" title))))
    (if (file-exists-p blog-path)
        (message "the path '%s' already exists!" blog-path)
      (insert (format "[[file://%s][%s]]" blog-path title)))
    ))

(defun maple//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s]]" prefix))
    (insert (format "![%s](%s)" imagename prefix))))

(defun maple/capture-screenshot (basename)
  (interactive "sScreenshot name: ")
  (let ((blog-image-path (substitute-in-file-name
                          (format "~/git/pelican/content/images/%s-%s.png" basename (format-time-string "%Y%m%d_%H%M%S")))))
    (if (file-exists-p blog-image-path)
        (message "the path '%s' already exists!" blog-image-path)
      (progn
        (call-process-shell-command "scrot" nil nil nil nil "-s" blog-image-path)
        (maple//insert-org-or-md-img-link blog-image-path basename)))
    )
  (insert "\n"))

(defun maple/org-md-export-to-markdown ()
  (interactive)
  (shell-command
   (format "mv -v %s %s"
           (shell-quote-argument (org-md-export-to-markdown))
           "../markdown/")))

(defun maple/org-html-export-to-html ()
  (interactive)
  (shell-command
   (format "mv -v %s %s"
           (shell-quote-argument (org-html-export-to-html))
           "../html/")))
(provide 'init-org)
