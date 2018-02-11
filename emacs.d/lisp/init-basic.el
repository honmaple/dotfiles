(eval-when-compile (require 'cl))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defconst maple-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Maple storage area for persistent files.")

(defconst maple-system-is-mac
  (eq system-type 'darwin))

(defconst maple-system-is-linux
  (eq system-type 'gnu/linux))

(defconst maple-system-is-mswindows
  (eq system-type 'windows-nt))

(defun maple/set-quit-key (map)
  "Use q `quit-window` in MAP."
  (after-load 'evil
    (evil-define-key 'normal map
      (kbd "q") 'quit-window)))

(defun maple/close-process ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)"
                                                change)
                              (kill-buffer (process-buffer proc))
                              (when (> (count-windows) 1)
                                (delete-window)))))))

(defun maple/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (save-excursion
    (when (hs-already-hidden-p)
      (end-of-visual-line)
      (evil-visual-state)
      (beginning-of-visual-line))
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end))))

(defun maple/indent-buffer ()
  "Format buffer with `indent-region`."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun maple/reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(defun maple/open-git-repo ()
  "Open remote repo URL."
  (interactive)
  (require 'magit)
  (let ((url (magit-get "remote" "origin" "url")))
    (progn
      (browse-url
        (if (string-match "^http" url)
            url
          (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                    "https://\\2/\\3"
                                    url)))
      (message "opening repo %s" url))))

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-basic)

;;; init-basic.el ends here
