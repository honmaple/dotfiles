(use-package gnus
  :defer t
  :commands gnus
  :init
  (evil-leader/set-key "ag" 'gnus)
  :config
  (progn
    ;; No primary server
    (setq gnus-select-method '(nnnil ""))

    ;; Use topics per default
    (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

    (setq gnus-visible-headers
          "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

    ;; Show the article headers in this order.
    (setq gnus-sorted-header-list
          '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
            "^Subject:" "^Date:" "^Gnus"))

    (setq-default
     gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
     gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
     gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
     gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
     gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
     gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
     gnus-sum-thread-tree-false-root ""
     gnus-sum-thread-tree-indent " "
     gnus-sum-thread-tree-leaf-with-other "├► "
     gnus-sum-thread-tree-root ""
     gnus-sum-thread-tree-single-leaf "╰► "
     gnus-sum-thread-tree-vertical "│"
     gnus-article-browse-delete-temp t
     gnus-treat-strip-trailing-blank-lines 'last
     gnus-keep-backlog 'nil
     gnus-summary-display-arrow nil ; Don't show that annoying arrow:
     gnus-mime-display-multipart-related-as-mixed t ; Show more MIME-stuff:
     gnus-auto-select-first nil ; Don't get the first article automatically:
     smiley-style 'medium
     gnus-keep-backlog '0)

    (require 'browse-url)
    (require 'nnrss)
    (defun spacemacs/browse-nnrss-url (arg)
      "Open RSS Article directy in the browser"
      (interactive "p")
      (let ((url (assq nnrss-url-field
                       (mail-header-extra
                        (gnus-data-header
                         (assq (gnus-summary-article-number)
                               gnus-newsgroup-data))))))
        (if url
            (progn
              (browse-url (cdr url))
              (gnus-summary-mark-as-read-forward 1))
          (gnus-summary-scroll-up arg))))
    (add-to-list 'nnmail-extra-headers nnrss-url-field)))
(setq gnus-secondary-select-methods
  '(
    (nntp "gmane"
           (nntp-address "news.gmane.org"))
    (nntp "news.eternal-september.org")
    (nntp "nntp.aioe.org")
    (nntp "news.gwene.org")
    ))
(setq gnus-secondary-select-methods
'(
  (nnimap "gmail"
           (nnimap-address
            "imap.gmail.com")
           (nnimap-server-port 993)
           (nnimap-stream ssl))
  ))

;; Send email via Gmail:
(setq message-send-mail-function 'smtpmail-send-it
  smtpmail-default-smtp-server "smtp.gmail.com")

;; Archive outgoing email in Sent folder on imap.gmail.com:
(setq gnus-message-archive-method '(nnimap "imap.gmail.com")
    gnus-message-archive-group "[Gmail]/Sent Mail")

;; set return email address based on incoming email address
(setq gnus-posting-styles
    '(((header "to" "xiyang0807@outlook.com")
       (address "xiyang0807@outlook.com"))
  ((header "to" "xiyang0807@gmail.com")
     (address "xiyang0807@gmail.com"))))

;; store email in ~/gmail directory
(setq nnml-directory "~/gmail")
(setq message-directory "~/gmail")

(provide 'init-gnus)
