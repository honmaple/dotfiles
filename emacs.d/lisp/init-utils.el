(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; (defvar load-user-customized-major-mode-hook t)
;; (defvar cached-normal-file-full-path nil)
;; (defun is-buffer-file-temp ()
;;   (interactive)
;;   "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
;;   (let ((f (buffer-file-name))
;;         org
;;         (rlt t))
;;     (cond
;;      ((not load-user-customized-major-mode-hook) t)
;;      ((not f)
;;       ;; file does not exist at all
;;       (setq rlt t))
;;      ((string= f cached-normal-file-full-path)
;;       (setq rlt nil))
;;      ((string-match (concat "^" temporary-file-directory) f)
;;       ;; file is create from temp directory
;;       (setq rlt t))
;;      ((and (string-match "\.html$" f)
;;            (file-exists-p (setq org (replace-regexp-in-string "\.html$" ".org" f))))
;;       ;; file is a html file exported from org-mode
;;       (setq rlt t))
;;      (t
;;       (setq cached-normal-file-full-path f)
;;       (setq rlt nil)))
;;     rlt))

(defun maple/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defconst maple-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "maple storage area for persistent files")

;; (defun maple/smart-move-beginning-of-line (arg)
;;   "Move point back to indentation of beginning of line.
;; Move point to the first non-whitespace character on this line.
;; If point is already there, move to the beginning of the line.
;; Effectively toggle between the first non-whitespace character and
;; the beginning of the line.
;; If ARG is not nil or 1, move forward ARG - 1 lines first. If
;; point reaches the beginning or end of the buffer, stop there."
;;   (interactive "^p")
;;   (setq arg (or arg 1))
;;   ;; Move lines first
;;   (when (/= arg 1)
;;     (let ((line-move-visual nil))
;;       (forward-line (1- arg))))
;;   (let ((orig-point (point)))
;;     (back-to-indentation)
;;     (when (= orig-point (point))
;;       (move-beginning-of-line 1))))

;; (defun maple/backward-kill-word-or-region (&optional arg)
;;   "Calls `kill-region' when a region is active and
;; `backward-kill-word' otherwise. ARG is passed to
;; `backward-kill-word' if no region is active."
;;   (interactive "p")
;;   (if (region-active-p)
;;       ;; call interactively so kill-region handles rectangular selection
;;       ;; correctly (see https://github.com/syl20bnr/spacemacs/issues/3278)
;;       (call-interactively #'kill-region)
;;     (backward-kill-word arg)))
(provide 'init-utils)
