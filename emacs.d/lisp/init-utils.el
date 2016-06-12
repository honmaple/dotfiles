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
(provide 'init-utils)
