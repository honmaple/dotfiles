(use-package org-plus-contrib)

;; org-pomodoro依赖
(use-package org
  :ensure nil
  :config
  (setq org-tags-column 80
        org-html-checkbox-type 'html
        org-image-actual-width '(600)
        org-export-with-sub-superscripts '{}
        org-descriptive-links nil ;; 不要锁定连接，保持原样
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
          (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
          (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))

  (defadvice org-open-at-point (after org-open-at-point activate)
    (while (>  (count-windows) 2)
      (delete-window (cadr (window-list-1)))))

  ;; 中英文对齐
  ;; (set-face-attribute 'org-table nil :font "-Sony-Sony Fixed-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
  ;; (set-face-attribute 'org-table nil :font "-jis-fixed-medium-r-normal--16-*-75-75-c-160-jisx0208.1983-0")
  ;; (set-face-attribute 'org-table nil :font "-jis-fixed-medium-r-normal--16-*-100-100-c-160-jisx0208.1983-0")
  ;; (set-face-attribute 'org-table nil :font "-Misc-Misc Fixed Wide-normal-normal-normal-ko-18-*-*-*-c-180-iso10646-1")
  (when (display-graphic-p)
    (set-face-attribute 'org-table nil :font "-Misc-Fixed-normal-normal-normal-*-18-*-*-*-c-90-iso10646-1")
    ;; (set-face-attribute 'org-table nil :font "-jis-fixed-medium-r-normal--16-*-75-75-c-160-jisx0208.1983-0")
    ;; (set-face-attribute 'org-table nil :font "-Sony-Sony Fixed-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    )

  :evil-bind
  (normal org-mode-map
          ((kbd "RET") . org-open-at-point)
          ("t" . org-todo)
          ("TAB" . org-cycle)
          ("<tab>" . org-cycle)))

(use-package ob
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)
     (octave . t)
     (sqlite . t)
     (perl . t)
     (C . t)))
  (use-package ob-python
    :ensure nil
    :config
    (add-to-list 'org-babel-default-header-args:python
                 '(:results . "output"))))


(use-package org-capture
  :ensure nil
  :config
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
                                        ,(format "~/git/pelican/content/org/%s笔记.org" src)
                                        maple/capture-target)
                            ,(concat "** %?\t\n#+BEGIN_SRC " src "\n\n#+END_SRC") :tags ,tags)))

  (maple/capture-snip "sp" "python" '("Tool" "Flask" "Tornado"))
  (maple/capture-snip "sl" "lua" '("Tool" "Nginx"))
  (maple/capture-snip "sg" "golang" '("Tool"))

  (setq org-refile-targets
        (quote (("~/org-mode/gtd.org" :level . 1)
                ("~/org-mode/summary.org" :maxlevel . 4))))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-todo :after 'org-save-all-org-buffers))

(use-package org-agenda
  :ensure nil
  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-inhibit-startup t   ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-log-done t
        org-agenda-files '("~/org-mode" )
        org-default-notes-file "~/org-mode/gtd.org")
  (setq org-agenda-custom-commands
        '(("b"  "博客" tags-todo "blog")
          ("p"  "项目" tags-todo "@Office")
          ("w" "Weekly Review"
           ((stuck "")
            (tags-todo "project")))))
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  :bind (:map org-agenda-mode-map
              ("j" . org-agenda-next-line)
              ("k" . org-agenda-previous-line)
              ("M-j" . org-agenda-next-item)
              ("M-k" . org-agenda-previous-item)
              ("M-h" . org-agenda-earlier)
              ("M-l" . org-agenda-later)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("①" "②" "③" "④" "⑤")))


(use-package org-pomodoro
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (use-package alert)
  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro)))

(use-package org-crypt
  :ensure nil
  :config
  ;; 當被加密的部份要存入硬碟時，自動加密回去
  (org-crypt-use-before-save-magic)
  ;; 設定要加密的 tag 標籤為 secret
  (setq org-crypt-tag-matcher "secret")
  ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
  ;; (但是子項目還是會被加密喔)
  (setq org-tags-exclude-from-inheritance (quote ("secret")))
  ;; 用於加密的 GPG 金鑰
  (setq org-crypt-key "21305E7E"))

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
