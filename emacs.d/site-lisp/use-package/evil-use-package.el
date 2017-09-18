;;; evil-use-package.el --- aa
;;; Commentary:

;;; Code:

(defalias 'use-package-normalize/:evil-bind 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-leader 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-emacs 'use-package-normalize-symlist)

(defalias 'use-package-normalize/:mode-setq 'use-package-normalize-forms)

(defun use-package-handler/:evil-bind (name keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name))
         (config-body
          (use-package-concat
           (use-package-hook-injector (symbol-name name-symbol)
                                      :init `((after-load 'evil
                                                (evil-define-key ',(caar arg) ,@(cdar arg)))))
           body
           (list t))))
    config-body))

(defun use-package-handler/:evil-leader (name keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name))
         (config-body
          (use-package-concat
           (use-package-hook-injector (symbol-name name-symbol)
                                      :init `((after-load 'evil-leader
                                                (evil-leader/set-key ',(caar arg) ,@(cdar arg)))))
           body
           (list t))))
    config-body))

(defun use-package-handler/:evil-emacs (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (var)
                 `(after-load 'evil
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

(add-to-list 'use-package-keywords :evil-bind t)
(add-to-list 'use-package-keywords :evil-leader t)
(add-to-list 'use-package-keywords :evil-emacs t)
(add-to-list 'use-package-keywords :mode-setq t)

(provide 'evil-use-package)
;;; evil-use-package.el ends here
