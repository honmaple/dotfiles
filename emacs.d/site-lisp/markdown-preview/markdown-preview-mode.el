;;; markdown-preview-mode.el --- markdown realtime preview minor mode.

(require 'cl-lib)
(require 'websocket)
(require 'web-server)

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


(defun markdown-preview:init-websocket ()
  (markdown-preview:start-ws-server)
  (markdown-preview:start-ws-client))

(defun markdown-preview:start-ws-server ()
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
  (unless markdown-preview:http-server
    (lexical-let ((docroot default-directory))
      (setq markdown-preview:http-server
            (ws-start
             (lambda (request)
               (with-slots (process headers) request
                 (let* ((path (substring (cdr (assoc :GET headers)) 1))
                        (filename (expand-file-name path docroot)))
                   (if (string= path "")
                       (progn
                         (ws-send-file
                          process
                          (expand-file-name
                           markdown-preview:preview-file)))
                     ))))
             markdown-preview:http-port nil :host markdown-preview:host)))))

(defun markdown-preview:open-browser ()
  "Open browser."
  (browse-url
   (format "http://%s:%s" markdown-preview:host markdown-preview:http-port)))

(defun markdown-preview:init ()
  "Markdown preview init."
  (markdown-preview:init-websocket)
  (markdown-preview:init-http-server)
  (markdown-preview:open-browser)
  ;; (add-hook 'post-command-hook (lambda ()
  ;;                              (markdown-preview:send-to-server) nil t)))
  (add-hook 'after-save-hook (lambda ()
                               (markdown-preview:send-to-server) nil t)))

(defun markdown-preview:finalize ()
  "Markdown preview close."
  (when markdown-preview:websocket-server
    (websocket-server-close markdown-preview:websocket-server))
  (when markdown-preview:websocket-client
    (websocket-close markdown-preview:websocket-client))
  (when markdown-preview:http-server
    (ws-stop markdown-preview:http-server))
  ;; (remove-hook 'post-command-hook 'markdown-preview:send-to-server t))
  (remove-hook 'after-save-hook 'markdown-preview:send-to-server t))


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
