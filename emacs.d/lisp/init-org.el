(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; (setq org-log-done t)

;; 设置默认浏览器
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(setq org-agenda-files (quote ("~/org-mode" )))
(setq org-default-notes-file "~/org-mode/gtd.org")
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
  (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)
  (define-key org-agenda-mode-map (kbd "M-j") 'org-agenda-next-item)
  (define-key org-agenda-mode-map (kbd "M-k") 'org-agenda-previous-item)
  (define-key org-agenda-mode-map (kbd "M-h") 'org-agenda-earlier)
  (define-key org-agenda-mode-map (kbd "M-l") 'org-agenda-later))
;; (define-key org-agenda-mode-map (kbd "gd") 'org-agenda-toggle-time-grid)
;; (define-key org-agenda-mode-map (kbd "gr") 'org-agenda-redo))

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

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-custom-commands
      '(
        ("w" . "任务安排")
        ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
        ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
        ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
        ("b" "博客" tags-todo "BLOG")
        ("p" . "项目安排")
        ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
        ("W" "Weekly Review"
         ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
          (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
          ))))

;; 设置todo
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))

(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
  ;; (diminish 'anzu-mode)
  )

;; org-mode 設定
(require 'org-crypt)
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
   (C . t)
   ))
;; 写博客
(defun org-new-blog (title)
  (interactive "sInput Title: ")
  (let ((blog-path (substitute-in-file-name (format "~/git/pelican/content/articles/%s.org" title))))
    (if (file-exists-p blog-path)
        (message "the path '%s' already exists!" blog-path)
      (insert (format "[[file://%s][%s]]" blog-path title)))
    ))

(provide 'init-org)
