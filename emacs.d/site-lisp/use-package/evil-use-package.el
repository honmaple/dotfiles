;;; evil-use-package.el --- aa
;;; Commentary:

;;; Code:
(add-to-list 'use-package-keywords :evil-bind t)
(add-to-list 'use-package-keywords :evil-leader t)
(add-to-list 'use-package-keywords :evil-emacs t)
(add-to-list 'use-package-keywords :mode-setq t)

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
(defalias 'use-package-normalize/:evil-emacs 'use-package-normalize-symlist)
(defalias 'use-package-normalize/:mode-setq 'use-package-normalize-forms)

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

;; (pp (macroexpand
;;      ))

(defun use-package-handler/:evil-leader (name keyword args rest state)
  (use-package-concat
   (let ((mode-flag nil)
         (mode nil))
     (mapcar
      #'(lambda (def)
          (eval-after-load 'evil
            (cond ((eq def :mode) (progn
                                    (setq mode-flag t)
                                    `nil))
                  ((and mode-flag (symbolp def))
                   (progn
                     (setq mode def)
                     `nil
                     ))
                  ((and mode-flag mode)
                   `(progn
                      (evil-leader/set-key-for-mode ',mode ,(car def) ',(cdr def))
                      )
                   )
                  ((not mode-flag)
                   `(progn
                      (evil-leader/set-key ,(car def) ',(cdr def)))
                   )
                  )))
      args))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-emacs (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (var)
                 `(with-eval-after-load 'evil
                    ,(if (consp var)
                         `(evil-set-initial-state ',(car var) ,(cdr var))
                       `(evil-set-initial-state ',var 'emacs))))
             arg)
     body)))


(defun use-package-handler/:mode-setq (name keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name))
         (config-body
          (use-package-concat
           (use-package-hook-injector (symbol-name name-symbol)
                                      :config `((when (not (featurep 'mode-local))
                                                  (require 'mode-local))
                                                (setq-mode-local ,(caar arg) ,@(cdar arg))))
           body
           (list t))))
    config-body))

;; (defun use-package-handler/:evil-emacs (name keyword arg rest state)
;;   (let* ((body (use-package-process-keywords name rest state))
;;          (name-symbol (use-package-as-symbol name))
;;          (config-body
;;           (use-package-concat
;;            (use-package-hook-injector (symbol-name name-symbol)
;;                                       :config `((after-load 'evil
;;                                                   (evil-set-initial-state ',arg 'emacs))))
;;            body
;;            (list t))))
;;     config-body))

(provide 'evil-use-package)
;;; evil-use-package.el ends here
