(defun maple/search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
  PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if (region-active-p)
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

;;;###autoload
(defmacro maple/search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "maple/search-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (maple/search ,search-engine-url ,search-engine-prompt)))

(provide 'maple-macro)
