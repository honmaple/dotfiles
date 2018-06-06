;;; markdown-preview-mode.el --- markdown realtime preview minor mode.

(require 'cl-lib)
(require 'websocket)
(require 'simple-httpd)

(defgroup markdown-preview nil
  "Realtime Markdown Viewer"
  :group 'text
  :prefix "markdown-preview:")

(defcustom markdown-preview:host "localhost"
  "Port number for Plack App"
  :type 'string
  :group 'markdown-preview)

(defcustom markdown-preview:port 8080
  "Port number for Plack App"
  :type 'integer
  :group 'markdown-preview)

(defcustom markdown-preview:http-port 8081
  "Port number for Plack App"
  :type 'integer
  :group 'markdown-preview)

(defvar markdown-preview:websocket nil)
(defvar markdown-preview:websocket-server nil
  "`markdown-preview' websocket server.")
(defvar markdown-preview:websocket-client nil
  "`markdown-preview' websocket client.")
(defvar markdown-preview:http-server nil
  "`markdown-preview' http server.")

(defvar markdown-preview:home-path (file-name-directory load-file-name))
(defvar markdown-preview:preview-file (concat markdown-preview:home-path "index.html"))
(defvar markdown-preview:css (concat markdown-preview:home-path "static/css/markdown.css"))


(defun markdown-preview:init-websocket ()
  "Init websocket."
  (markdown-preview:start-ws-server)
  (markdown-preview:start-ws-client))

(defun markdown-preview:start-ws-server ()
  "Start websocket."
  (when (not markdown-preview:websocket-server)
    (setq markdown-preview:websocket-server
          (websocket-server
           markdown-preview:port
           :host markdown-preview:host
           :on-message (lambda (_websocket frame)
                         (setq markdown-preview:websocket _websocket)
                         (markdown-preview:send-preview _websocket))
           :on-open (lambda (_websocket)
                      (message "websocket: I'm opened."))
           :on-error (lambda (_websocket type err)
                       (message "error connecting"))
           :on-close (lambda (_websocket)
                       (setq markdown-preview:websocket-server nil))))))

(defun markdown-preview:start-ws-client ()
  "Start the `markdown-preview' local client."
  (when (not markdown-preview:websocket-client)
    (setq markdown-preview:websocket-client
          (websocket-open
           (format "ws://%s:%d" markdown-preview:host markdown-preview:port)
           :on-error (lambda (ws type err)
                       (message "error connecting"))
           :on-close (lambda (_websocket)
                       (setq markdown-preview:websocket-client nil))))))


(defun markdown-preview:send-preview (websocket)
  "Send the `markdown-preview' preview to clients."
  (let ((mark-position-percent
         (number-to-string
          (truncate
           (* 100
              (/
               (float (-  (line-number-at-pos) (/ (count-screen-lines (window-start) (point)) 2)))
               (count-lines (point-min) (point-max))))))))
    (websocket-send-text websocket
                         (concat
                          "<div id=\"position-percentage\" style=\"display:none;\">"
                          mark-position-percent
                          "</div>"
                          (buffer-substring-no-properties (point-min) (point-max))))))

(defun markdown-preview:send-to-server ()
  "Send the `markdown-preview' preview to clients."
  (when (bound-and-true-p markdown-preview-mode)
    (markdown-preview:send-preview markdown-preview:websocket)))

(defun markdown-preview:init-http-server ()
  "Start http server at PORT to serve preview file via http."
  (when (not markdown-preview:http-server)
    (fset 'httpd-log 'ignore)
    (setq httpd-root markdown-preview:home-path
          httpd-host markdown-preview:host
          httpd-port markdown-preview:http-port)
    (httpd-stop)
    (setq markdown-preview:http-server
          (make-network-process
           :name     "httpd"
           :service  httpd-port
           :server   t
           :host     httpd-host
           :family   httpd-ip-family
           :filter   'httpd--filter
           :filter-multibyte nil
           :coding   'binary
           :log      'httpd--log))))

(defun markdown-preview:open-browser ()
  "Open browser."
  (browse-url
   (format "http://%s:%s" markdown-preview:host markdown-preview:http-port)))

(defun markdown-preview:init ()
  "Markdown preview init."
  (markdown-preview:init-websocket)
  (markdown-preview:init-http-server)
  (markdown-preview:open-browser)
  (add-hook 'after-save-hook #'markdown-preview:send-to-server nil t))

(defun markdown-preview:finalize ()
  "Markdown preview close."
  (when markdown-preview:websocket-client
    (websocket-close markdown-preview:websocket-client))
  (when markdown-preview:websocket-server
    (websocket-server-close markdown-preview:websocket-server))
  (when markdown-preview:http-server
    (when (process-status markdown-preview:http-server)
      (delete-process markdown-preview:http-server)
      ;; close connection
      (dolist (i (process-list))
        (when (and (string-prefix-p "httpd <127.0.0.1" (process-name i))
                   (equal (process-type i) 'network))
          (delete-process i))))
    (setq markdown-preview:http-server nil))
  (remove-hook 'after-save-hook 'markdown-preview:send-to-server t))

;;;###autoload
(defun markdown-preview-cleanup ()
  "Cleanup `markdown-preview' mode."
  (interactive)
  (markdown-preview:finalize))

;;;###autoload
(define-minor-mode markdown-preview-mode
  "Markdown preview mode"
  :group      'markdown-preview
  :init-value nil
  :global     nil
  (if markdown-preview-mode
      (markdown-preview:init)
    (markdown-preview:finalize)))

(provide 'markdown-preview-mode)

;;; markdown-preview-mode.el ends here
