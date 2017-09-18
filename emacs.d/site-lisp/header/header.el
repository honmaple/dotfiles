;;; header.el --- header.

;; Copyright (c) 2017 honmaple
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

(defgroup maple/header nil
  "Auth update file header."
  :group 'text)

(defcustom maple//header-update-email nil
  "Auto update email."
  :type 'boolean
  :group 'maple/header)

(defcustom maple//header-update-filename nil
  "Auto update filename."
  :type 'boolean
  :group 'maple/header)

(defun maple/header-update-action(name)
  "A."
  (let ((beg (match-beginning 2))
        (end (match-end 2)))
    (when (not (string= name (string-trim-left (match-string 2))))
      (goto-char beg)
      (delete-region beg end)
      (insert " " name))))

(defun maple/header-update(regex default line-limit)
  "B."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((lines 0))
      (while (< lines line-limit)
        (when (and (looking-at regex))
          (maple/header-update-action default))
        (setq lines (1+ lines))
        (forward-line 1)))))

(defmacro maple/header-update-engine (name regex default &optional line-limit)
  "A NAME aa, REGEX aa, DEFAULT aa LINE-LIMIT aa."
  `(defun ,(intern (format "maple/header-update-%s" name)) ()
     ,(format "Update %s with regex." name)
     (interactive)
     (maple/header-update ,regex ,default ,(or line-limit 7))))


(maple/header-update-engine "filename"
                            ".*\\(File Name:\\)\\(.*\\)"
                            (file-name-nondirectory (buffer-file-name)) 7)
(maple/header-update-engine "email"
                            ".*\\(Email:\\)\\(.*\\)"
                            "xiyang0807@gmail.com" 7)

;;;###autoload
(defun maple/header-auto-update()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((lines 0))
      (while (< lines 7)
        (when (and maple//header-update-filename
                   (looking-at ".*\\(File Name:\\)\\(.*\\)"))
          (maple/header-update-action (file-name-nondirectory (buffer-file-name))))
        (when (and maple//header-update-email
                   (looking-at ".*\\(Email:\\)\\(.*\\)"))
          (maple/header-update-action "xiyang0807@gmail.com"))
        (setq lines (1+ lines))
        (forward-line 1)))))

;; # File Name: header.el
;; # Author: jianglin
;; # Email: xiyang0807@gmail.com

(provide 'header)

;;; header.el ends here
