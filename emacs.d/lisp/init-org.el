(require-package 'org-plus-contrib)
(require-package 'org-pomodoro)
(require-package 'org-bullets)
(require-package 'ox-reveal)
(require-package 'alert) ;;org-pomodoro依赖


(use-package org
  :mode ("\\.org$" . org-mode)
  :defer t
  :commands (orgtbl-mode)
  :init
  (progn
    (setq org-emphasis-regexp-components
          ;; markup 记号前后允许中文
          (list (concat " \t('\"{"            "[:nonascii:]")
                (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
                " \t\r\n,\"'"
                "."
                1))
    )
  :config
  (progn
    (setq org-tags-column 80)
    (setq org-html-checkbox-type 'html)
    (setq org-image-actual-width '(600))
    (setq org-export-with-sub-superscripts (quote {}))
    (after-load 'org
      (setq org-match-substring-regexp
            (concat
             ;; 限制上标和下标的匹配范围，org 中对其的介绍见：(org) Subscripts and superscripts
             "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
             "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
             "\\|"
             "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
             "\\|"
             "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)")))

    ;; 中英文对齐
    (if (and (fboundp 'daemonp) (daemonp))
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                    (with-selected-frame frame
                      (set-face-font 'org-table "-Misc-Fixed-normal-normal-normal-*-18-*-*-*-c-90-iso10646-1")
                      ;; (set-face-font 'org-table "WenQuanYi Micro Hei Mono"
                      ;;                (:size 18) t)
                      )))
      (set-face-font 'org-table "-Misc-Fixed-normal-normal-normal-*-18-*-*-*-c-90-iso10646-1"))

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
    (add-to-list 'org-babel-default-header-args:python
                 '(:results . "output"))

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                  (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                  (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

    (setq org-todo-keyword-faces
          (quote (("NEXT" :inherit warning)
                  ("PROJECT" :inherit font-lock-string-face))))

    (defadvice org-open-at-point (after org-open-at-point activate)
      (while (>  (count-windows) 2)
        (delete-window (cadr (window-list-1)))))

    (defadvice org-toggle-inline-images (after org-toggle-inline-images activate)
      (if smooth-scrolling-mode (smooth-scrolling-mode -1)
        (smooth-scrolling-mode 1)))

    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
    (evil-define-key 'normal org-mode-map (kbd "t") 'org-todo)
    (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
    (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle))
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c l" . org-store-link)))


(use-package org-capture
  :defer t
  :config
  (progn
    (setq org-capture-templates
          '(("t" "待办"
             entry (file+headline "~/org-mode/gtd.org" "待办事项")
             "* TODO [#B] %?      :%^{Where|@Office|@Home|@Lunchtime|@School}:\n  %i\n"
             :empty-lines 1)
            ("w" "工作"
             entry (file+headline "~/org-mode/project.org" "工作安排")
             "* TODO [#A] %?      :project:%^{Where|@Office|@Home|@Lunchtime}:\n  %i\n %U"
             :empty-lines 1)
            ("n" "笔记"
             entry (file+headline "~/org-mode/notes.org" "笔记")
             "*  %?\n  %i\n %U"
             :empty-lines 1)
            ("m" "电影"
             entry (file+headline "~/org-mode/notes.org" "影视歌曲")
             "*  %?               :%^{看了什么|Movie|Song}:\n Watched on %T\n %i\n"
             :empty-lines 1)
            ("r" "阅读"
             entry (file+headline "~/org-mode/notes.org" "阅读")
             "*  %?               :Book:\n  %T\n %i\n"
             :empty-lines 1)
            ("b" "博客"
             entry (file+headline "~/org-mode/blog.org" "博客")
             "** TODO [#B] %?     :blog:\n  %i %U"
             :empty-lines 1)
            ("c" "账单"
             table-line (file+headline "~/org-mode/mine.org" "账单")
             "| %^{用途|吃饭|购买衣服|出行} | %U | %? | |")
            ("s" "代码片段"
             entry (file "~/org-mode/snippets.org")
             "*  %?\t%^g\n#+BEGIN_SRC %^{language|python|html|css|javascript}\n\n#+END_SRC")
            ("j" "日程安排"
             entry (file+headline "~/org-mode/gtd.org" "日程安排")
             "* TODO [#B] %?      :%^{去哪儿|上海|南京|常州|昆明}:Journal:\n %^U\n"
             :empty-lines 1)
            ("z" "总结"
             entry (file+datetree  "~/org-mode/summary.org" "总结")
             "* %?                :%^{周期|Yearly|Monthly|Weekly|Daily}:Summary:"
             :empty-lines 1)
            ("g" "毕业设计"
             entry (file+datetree  "~/org-mode/gradution.org" "毕业设计")
             "* %?                :%^{周期|Yearly|Monthly|Weekly|Daily}:Summary:"
             :empty-lines 1)
            ))
    (setq org-refile-targets
          (quote (("~/org-mode/gtd.org" :level . 1)
                  ("~/org-mode/summary.org" :maxlevel . 4))))
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (advice-add 'org-todo :after 'org-save-all-org-buffers)
    ))

;; 设置默认浏览器
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(use-package org-agenda
  :defer t
  :init (setq org-agenda-restore-windows-after-quit t)
  :config
  (progn
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-inhibit-startup t)   ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-log-done t)
    (setq org-agenda-files (quote ("~/org-mode" )))
    (setq org-default-notes-file "~/org-mode/gtd.org")
    (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
            ("b"  "博客" tags-todo "blog")
            ("p" . "项目")
            ("pi" tags-todo "iot")
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

(use-package org-crypt
  :after org
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
  :after org
  :init(setq org-reveal-root "file:///home/jianglin/git/ppt/reveal.js"))

;; 写博客
(defun org-new-blog (title)
  (interactive "sInput Title: ")
  (let ((blog-path (substitute-in-file-name (format "~/git/pelican/content/org/%s.org" title))))
    (if (file-exists-p blog-path)
        (message "the path '%s' already exists!" blog-path)
      (insert (format "[[file://%s][%s]]" blog-path title)))
    ))

(defun maple/insert-org-or-md-img-link (prefix imagename)
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
        (shell-command
         (format "scrot -s %s" blog-image-path))
        (maple/insert-org-or-md-img-link blog-image-path basename)))
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
