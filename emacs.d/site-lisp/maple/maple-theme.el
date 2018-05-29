;;; maple-theme.el --- Theme switch

;; Copyright (c) 2018 honmaple
;;
;; Author: honmaple
;;
;;; License: GPLv3
;;
;;; Commentary:

;;; Code:

;;
;; Customs
;;


;; (defvar maple-cycle-themes (mapcar 'symbol-name (custom-available-themes)))
;; (defvar maple-cycle-themes (delete "doom-one-light"
;;                                  (mapcar 'symbol-name (custom-available-themes))))
(defvar maple-cycle-themes '("monokai"
                             "spacemacs-dark"
                             "solarized-light"
                             "solarized-dark"
                             "doom-molokai"
                             "doom-one"
                             "doom-peacock"
                             "doom-vibrant"))
(defun maple/cycle-theme (num)
  "Theme switch with NUM."
  (interactive)
  (let ((maple-current-theme-index
         (+ num
            (cl-position
             (car (mapcar 'symbol-name custom-enabled-themes)) maple-cycle-themes :test 'equal))))
    (when (>= maple-current-theme-index (length maple-cycle-themes))
      (setq maple-current-theme-index 0))
    (when (eq maple-current-theme-index -1)
      (setq maple-current-theme-index (1- (length maple-cycle-themes))))
    (let ((maple-current-theme (nth maple-current-theme-index maple-cycle-themes)))
      (mapc 'disable-theme custom-enabled-themes)
      (let ((progress-reporter
             (make-progress-reporter
              (format "Loading theme %s..." maple-current-theme))))
        (load-theme (intern maple-current-theme) t)
        (progress-reporter-done progress-reporter)))))

;;;###autoload
(defun maple/next-theme()
  (interactive)
  (maple/cycle-theme 1))

;;;###autoload
(defun maple/previous-theme()
  (interactive)
  (maple/cycle-theme -1))

(provide 'maple-theme)

;;; maple-theme.el ends here
