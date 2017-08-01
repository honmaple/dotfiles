(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


(defconst maple-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "maple storage area for persistent files")

(fset 'yes-or-no-p 'y-or-n-p)
;; 让emacs自动給script加上可执行权限
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq-default regex-tool-backend 'perl)


(eval-when-compile (require 'cl))

(defun add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(add-subdirs-to-load-path
 (expand-file-name (concat user-emacs-directory "site-lisp/")))

(provide 'init-utils)
