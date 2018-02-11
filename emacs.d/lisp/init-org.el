(use-package org-plus-contrib
  :ensure t
  :defer t)

;; org-pomodoro依赖
(use-package alert
  :ensure t
  :defer 5)

(use-package org
  :mode ("\\.org$" . org-mode)
  :defer t
  :commands orgtbl-mode
  :init
  (setq org-emphasis-regexp-components
        ;; markup 记号前后允许中文
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "." 1))
  :config
  (progn
    (setq org-tags-column 80)
    (setq org-html-checkbox-type 'html)
    (setq org-image-actual-width '(600))
    (setq org-export-with-sub-superscripts (quote {}))
    (setq org-descriptive-links nil) ;; 不要锁定连接，保持原样
    (setq org-match-substring-regexp
          (concat
           ;; 限制上标和下标的匹配范围，org 中对其的介绍见：(org) Subscripts and superscripts
           "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
           "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
           "\\|"
           "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
           "\\|"
           "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)"))

    ;; 中英文对齐
    (when (display-graphic-p)
      (set-face-attribute 'org-table nil :font "-Misc-Fixed-normal-normal-normal-*-18-*-*-*-c-90-iso10646-1"))


    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh . t)
       (python . t)
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
        (delete-window (cadr (window-list-1))))))
  :evil-bind
  (normal org-mode-map
          "RET" 'org-open-at-point
          "t" 'org-todo
          "TAB" 'org-cycle
          "<tab>" 'org-cycle))


(use-package org-capture
  :defer t
  :config
  (progn
    (setq org-capture-templates
          '(("t" "待办"
             entry (file+headline "~/org-mode/gtd.org" "待办事项")
             "* TODO [#B] %?      :%^{Where|@Office|@Home|@Lunchtime|@School}:\n  %^T\n%i"
             :empty-lines 1)
            ("w" "工作"
             entry (file+headline "~/org-mode/project.org" "工作安排")
             "* TODO [#A] %?      :project:%^{Where|@Office|@Home|@Lunchtime}:\n  %i\n %U"
             :empty-lines 1)
            ("n" "笔记")
            ("na" "笔记"
             entry (file+headline "~/org-mode/notes.org" "笔记")
             "*  %?\n  %i\n %U"
             :empty-lines 1)
            ("nm" "电影"
             entry (file+headline "~/org-mode/notes.org" "影视歌曲")
             "*  %?               :%^{看了什么|Movie|Song}:\n Watched on %T\n %i\n"
             :empty-lines 1)
            ("nr" "阅读"
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
            ("s" "代码片段")
            ("j" "日程安排"
             entry (file+headline "~/org-mode/gtd.org" "日程安排")
             "* TODO [#B] %?      :%^{去哪儿|上海|南京|常州|昆明}:Journal:\n %^U\n"
             :empty-lines 1)
            ("z" "总结"
             entry (file+datetree  "~/org-mode/summary.org" "总结")
             "* %?                :%^{周期|Yearly|Monthly|Weekly|Daily}:Summary:"
             :empty-lines 1)
            ("g" "毕业设计"
             entry (file+datetree+prompt  "~/org-mode/gradution.org" "毕业设计")
             "* %?                :%^{周期|Yearly|Monthly|Weekly|Daily}:Summary:\n"
             :empty-lines 1)
            ))

    (defun maple/capture-target ()
      "Set point for capturing at what capture target file+headline with headline set to %l would do."
      (org-capture-put :target (list
                                'file+headline
                                (nth 1 (org-capture-get :target))
                                (completing-read "Followed by: " (plist-get org-capture-plist :tags))))
      (org-capture-put-target-region-and-position)
      (widen)
      (let ((hd (nth 2 (org-capture-get :target))))
        (goto-char (point-min))
        (if (re-search-forward
             (format org-complex-heading-regexp-format (regexp-quote hd))
             nil t)
            (goto-char (point-at-bol))
          (goto-char (point-max))
          (or (bolp) (insert "\n"))
          (insert "* " hd "\n")
          (beginning-of-line 0))))

    (defun maple/capture-snip (keybind src &optional tags)
      (add-to-list 'org-capture-templates
                   `(,keybind ,src entry (file+function
                                          ,(format "~/git/pelican/content/org/%s gist.org" src)
                                          maple/capture-target)
                              ,(concat "** %?\t\n#+BEGIN_SRC " src "\n\n#+END_SRC") :tags ,tags)))

    (maple/capture-snip "sp" "python" '("Tool" "Flask" "Tornado"))
    (maple/capture-snip "sl" "lua" '("Tool" "Nginx"))

    (setq org-refile-targets
          (quote (("~/org-mode/gtd.org" :level . 1)
                  ("~/org-mode/summary.org" :maxlevel . 4))))
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (advice-add 'org-todo :after 'org-save-all-org-buffers)
    ))

(use-package org-agenda
  :defer t
  :config
  (progn
    (setq org-agenda-restore-windows-after-quit t
          org-agenda-window-setup 'current-window
          org-agenda-inhibit-startup t   ;; ~50x speedup
          org-agenda-use-tag-inheritance nil ;; 3-4x speedup
          org-log-done t
          org-agenda-files '("~/org-mode" )
          org-default-notes-file "~/org-mode/gtd.org")
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
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook 'org-bullets-mode)
  :config (setq org-bullets-bullet-list '("①" "②" "③" "④" "⑤")))


(use-package org-pomodoro
  :ensure t
  :defer t
  :config (setq org-pomodoro-keep-killed-pomodoro-time t)
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro)))

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
