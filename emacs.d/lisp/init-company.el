(require-package 'company)
(require-package 'company-statistics)

;; (require 'company)

(eval-after-load 'company
  '(progn
     ;; @see https://github.com/company-mode/company-mode/issues/348
     (unless (featurep 'company-statistics)
       (require 'company-statistics)
       (setq company-statistics-file (concat user-emacs-directory
                                             "company-statistics-cache.el"))
                                        ; (add-hook 'company-mode-hook 'company-statistics-mode)
       (company-statistics-mode 1))
     (setq company-echo-delay 0)
     (setq company-idle-delay 0.1)
     (setq company-auto-complete nil)
     (setq company-show-numbers t)
     (setq company-tooltip-limit 15)
     (setq company-minimum-prefix-length 1)
     (setq company-dabbrev-other-buffers t)
     (setq company-dabbrev-downcase nil)
     (setq company-dabbrev-ignore-case t)
     (setq company-tooltip-align-annotations t)
     (setq company-begin-commands '(self-insert-command))
     (let ((map company-active-map))
       (define-key map (kbd "C-j") 'company-select-next)
       (define-key map (kbd "C-k") 'company-select-previous)
       (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
       (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
       (define-key map (kbd "<RET>") 'company-complete-selection))
     (setq company-global-modes
           '(not comint-mode erc-mode gud-mode rcirc-mode
                 minibuffer-inactive-mode))
     (diminish 'company-mode " ⓐ")
     ))


(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)

(setq company-backends
        '((company-dabbrev-code company-gtags company-etags company-keywords :with company-yasnippet)
          (company-files :with company-yasnippet)
          (company-dabbrev :with company-yasnippet)))

;; (setq company-backends
;;       '((company-files          ; files & directory
;;          company-keywords       ; keywords
;;          company-capf
;;          company-abbrev 
;;          company-dabbrev
;;          company-yasnippet)
;;         ))
;; (add-hook 'css-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends) '(company-css))))

                                        ; (setq company-tooltip-flip-when-above t)

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
