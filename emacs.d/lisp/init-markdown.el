(require-package 'markdown-mode)

(use-package markdown-mode
  :defer t
  :config
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))
(provide 'init-markdown)
