;;; maple-marco.el --- Marco switch

;; Copyright (c) 2018 honmaple
;;
;; Author: honmaple
;;
;;; License: GPLv3
;;
;;; Commentary:

;;; Code:

;;
;; Customs
;;

(defvar maple/search-alist
  '(("Google" "http://www.google.com/search?q=")
    ("GitHub" "https://github.com/search?q=")
    ("Google Image" "https://google.com/images?q=%s")
    ))

(defun maple/search (query-url prompt)
  "Open the QUERY-URL.PROMPT set the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if (region-active-p)
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defun maple/search-engine (engine)
  "Give ENGINE create search."
  (let ((engine-name (replace-regexp-in-string " " "-" (downcase (nth 0 engine))))
        (engine-prompt (concat (nth 0 engine) ": ") )
        (engine-url (nth 1 engine)))
    `(defun ,(intern (format "maple/search-%s" engine-name)) ()
       ,(format "Search %s with a query or region if any." engine-name)
       (interactive)
       (maple/search ,engine-url ,engine-prompt))))

(defmacro maple/search-macro ()
  "Search macro."
  `(progn ,@(mapcar #'maple/search-engine maple/search-alist)))

;;;###autoload
(defun maple/search-init ()
  "Search macro init."
  (maple/search-macro))

(provide 'maple-macro)
