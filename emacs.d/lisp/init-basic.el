(eval-when-compile (require 'cl))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defconst maple-cache-directory
  (expand-file-name (concat user-emacs-directory "cache/"))
  "Maple storage area for persistent files.")

(defconst maple-system-is-mac
  (eq system-type 'darwin))

(defconst maple-system-is-linux
  (eq system-type 'gnu/linux))

(defconst maple-system-is-windows
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

(defun maple/initial-message(&optional prefix)
  (interactive)
  (if (executable-find "fortune")
      (format (concat prefix "%s\n\n%s")
              (replace-regexp-in-string
               "\\[[0-9]*m" "" ; remove chinese shell char
               (replace-regexp-in-string
                "\n" (concat "\n" prefix) ; comment each line
                (replace-regexp-in-string
                 "\s*$" ""    ; remove spaces
                 (replace-regexp-in-string
                  "\n$" ""    ; remove trailing linebreak
                  (shell-command-to-string
                   "fortune -a | fmt -80 -s | cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n")))))
              (concat prefix "Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
    (concat prefix "Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

(defun maple/comment-or-uncomment (&optional paste)
  "Comments or uncomments the region or the current line if there's no active region with no `PASTE`."
  (interactive)
  (save-excursion
    (when (and (hs-minor-mode) (hs-already-hidden-p))
      (end-of-visual-line)
      (evil-visual-state)
      (beginning-of-visual-line))
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (when paste
        (copy-region-as-kill beg end)
        (goto-char end)
        (yank))
      (comment-or-uncomment-region beg end))))

(defun maple/copy-and-comment ()
  "Copy and comment."
  (interactive)
  (maple/comment-or-uncomment t))

(defun maple/indent-buffer ()
  "Format buffer with `indent-region`."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun maple/reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(defun maple/define-key (keymap key def &rest bindings)
  "Define multi keybind with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap key def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun maple/company-backend (hook backend)
  "Set HOOK with BACKEND `company-backends'."
  (add-hook hook `(lambda()
                    (set (make-variable-buffer-local 'company-backends)
                         (append (list (company-backend-with-yas ',backend))
                                 company-default-backends)))))

(defun maple/get-weekday()
  (car (rassq (string-to-number (format-time-string "%w"))
              '(("Sunday" . 0)
                ("Monday" . 1)
                ("Tuesday" . 2)
                ("Wednesday" . 3)
                ("Thursday" . 4)
                ("Friday" . 5)
                ("Saturday" . 6)))))

(defun maple/truncate-lines()
  (toggle-truncate-lines t))

(defun maple/close-nlinum()
  (nlinum-mode -1))

(defun maple/switch-theme()
  (load-theme user-default-theme t))

(defun maple/reopen-buffer(buffer-name &optional restore)
  "Reopen BUFFER-NAME."
  (if (get-buffer buffer-name)
      (let ((buffer-text (with-current-buffer buffer-name
                           (buffer-substring (point-min) (point-max)))))
        (kill-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (when restore
            (insert buffer-text))))
    (message (format "%s buffer is not exists" buffer-name))))

(defun maple/startup-buffer()
  (dolist (buffer '("*Messages*" "*Compile-Log*"))
    (and (get-buffer buffer) (maple/reopen-buffer buffer t))))

(add-hook 'emacs-startup-hook 'maple/startup-buffer)

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-basic)

;;; init-basic.el ends here
