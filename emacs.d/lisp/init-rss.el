(use-package newsticker
  :defer t
  :init
  (progn
    (add-to-list 'evil-emacs-state-modes 'newsticker-treeview-mode)
    (add-to-list 'evil-emacs-state-modes 'newsticker-treeview-list-mode)
    ;; (add-to-list 'evil-emacs-state-modes 'newsticker-treeview-item-mode)

    ;; (setq-default
    ;;  newsticker-groups-filename (concat "~/Rss/" "groups")
    ;;  newsticker-groups '("Feeds" "网易新闻" "南都周刊"
    ;;                      ("新闻" "阮一峰的网络日志" "月光博客" "honmaple's blog")))
    (setq
     ;; newsticker-dir "~/Rss/"
     ;; newsticker-groups-filename (concat "~/Rss/" "groups")
     newsticker-url-list-defaults nil   ;;设置默认的列表为空
     ;; newsticker-automatically-mark-items-as-old t          ;;自动标记项目为已经检索的项目
     ;; newsticker-automatically-mark-visited-items-as-old t  ;;自动标记已经访问过的项目
     newsticker--sentinel-callback nil
     ;; newsticker-heading-format "%t"
     ;; newsticker-item-format "%t"
     ;; newsticker-desc-format "%d\n%c"
     ;; newsticker-hide-old-items-in-newsticker-buffer t
     ;; newsticker-html-renderer 'w3m-region
     ;; newsticker-frontend 'newsticker-plainview
     ;; newsticker-use-full-width nil
     newsticker-retrieval-interval 0   ;don't fetch when I'm not reading RSS
     newsticker-automatically-mark-items-as-old nil

     newsticker-url-list
     '(("honmaple's blog" "https://honmaple.com/blog/rss")
       ("南都周刊" "http://www.nbweekly.com/rss/smw/")
       ("月光博客" "http://feed.williamlong.info/")
       ("阮一峰的网络日志" "http://www.ruanyifeng.com/blog/atom.xml")
       ("网易新闻" "http://news.163.com/special/00011K6L/rss_newsattitude.xml")
       ("知乎" "http://www.zhihu.com/rss")
       ("知乎日报" "https://link.zhihu.com/?target=http%3A//feeds.feedburner.com/zhihu-daily")
       ))

    (add-hook 'newsticker-treeview-mode-hook
              (lambda ()
                (define-key newsticker-treeview-mode-map (kbd "j") 'newsticker-treeview-next-feed)
                (define-key newsticker-treeview-mode-map (kbd "k") 'newsticker-treeview-prev-feed)
                (define-key newsticker-treeview-mode-map (kbd "l") 'newsticker-treeview-next-page)
                ))
    (add-hook 'newsticker-treeview-list-mode-hook
              (lambda ()
                (define-key newsticker-treeview-list-mode-map (kbd "j") 'next-line)
                (define-key newsticker-treeview-list-mode-map (kbd "k") 'previous-line)
                ))
    ))

(provide 'init-rss)
