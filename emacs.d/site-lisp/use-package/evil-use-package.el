;;; evil-use-package.el --- aa
;;; Commentary:

;;; Code:


(add-to-list 'use-package-keywords :evil-bind t)
(add-to-list 'use-package-keywords :evil-leader t)
(add-to-list 'use-package-keywords :evil-state t)
(add-to-list 'use-package-keywords :setq t)


(defun maple/mplist-get (plist prop)
  "Get the values associated PLIST to PROP, a modified plist."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun use-package-normalize/:evil-bind (name keyword args)
  "Normalize use-package custom keyword."
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (listp arg)
          (use-package-error
           (concat label " a (<symbol> <value> <keybind>)"
                   " or list of these")))
        (if (or (use-package-non-nil-symbolp (car arg))
                (stringp (car arg)))
            (list arg)
          arg))))

(defalias 'use-package-normalize/:evil-leader 'use-package-normalize/:evil-bind)
(defalias 'use-package-normalize/:evil-state 'use-package-normalize-forms)
(defalias 'use-package-normalize/:setq 'use-package-normalize-forms)

(defun use-package-handler/:evil-bind (name keyword args rest state)
  (use-package-concat
   (mapcar
    #'(lambda (def)
        (let ((state (nth 0 def))
              (map (nth 1 def))
              (bind (nthcdr 2 def)))
          (eval-after-load 'evil
            `(evil-define-key ',state ,map ,@bind)))
        )
    args)
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-leader (name keyword args rest state)
  (use-package-concat
   (mapcar
    #'(lambda (arg)
        (let ((mode (maple/mplist-get (if (plist-get arg :mode) arg (list arg)) :mode))
              (binds '()))
          (if (not mode)
              `(eval-after-load 'evil
                 (evil-leader/set-key ,(car arg) ',(cdr arg)))
            (mapc #'(lambda (arg)
                      (setq binds (append binds `(,(car arg) ',(cdr arg)))))
                  (cdr mode))
            `(eval-after-load 'evil
               (evil-leader/set-key-for-mode ',(car mode)
                 ,@binds))
            )
          )
        )
    args)
   (use-package-process-keywords name rest state)))


(defun use-package-handler/:evil-state (name keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (arg)
                 `(with-eval-after-load 'evil
                    (evil-set-initial-state ',(car arg) ',(cdr arg))
                    ))
             args)
     body)))

(defun use-package-handler/:setq (name keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (arg)
                 (when (not (featurep 'mode-local))
                   (require 'mode-local))
                 (let ((mode (maple/mplist-get arg :mode)))
                   `(setq-mode-local ,(car mode) ,@(cdr mode))
                   ))
             args)
     body)))

(provide 'evil-use-package)
;;; evil-use-package.el ends here
