;;; init-buffer.el --- buffer

;;; Commentary:
;;


;;; Code:
(defun maple/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun maple/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun maple/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun maple/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun maple/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? " (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (message "Buffers deleted!")))

(defun maple/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun maple/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(provide 'init-buffer)
