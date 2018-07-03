(require 'powerline)

(defface mapleline-highlight
  `((t (:background "DarkGoldenrod2"
                    :foreground "#3E3D31"
                    :inherit 'mode-line)))
  "Default highlight face for spaceline.")

(defmacro mapleline-define (name &rest args)
  "Define modeline with NAME and ARGS."
  (declare (indent 1)
           (doc-string 2))
  (let* ((-if (or (plist-get args :if) t))
         (-format (plist-get args :format))
         (-priority (or (plist-get args :priority) 0)))
    `(progn
       (defvar ,(intern (format "mapleline-%s-p" name)) t)
       (defun ,(intern (format "mapleline--%s" name)) (&optional face)
         (let ((width (with-current-buffer (current-buffer)
                        (+ (window-width)
                           (or (cdr (window-margins)) 0)
                           (or (car (window-margins)) 0)))))
           (when  (and ,-if (> width ,-priority))
             (powerline-raw ,-format face))))
       )))

(defmacro mapleline-define-fill(width)
  "Define mapleline--fill with WIDTH."
  `(defun mapleline--fill(&optional face)
     (powerline-fill face ,width)))

(defmacro mapleline-flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  `(let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p ,state))
          (err (or (cdr (assq ,state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running) (format "•%s" err))))


(defun mapleline--string-trim-from-center (str len)
  "Return STR with its center chars trimmed for it to be a maximum length of LEN."
  (if (> (length str) len)
      (let ((mid (/ (- len 3) 2)))
        (concat (substring str 0 mid)
                (apply #'propertize "..." (text-properties-at (- mid 1) str))
                (substring str (- (1+ mid)) nil)))
    str))

(defun mapleline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "10" str) "➓")
   (t str)))

(mapleline-define window-number
  :if (bound-and-true-p window-numbering-mode)
  :format
  (format " %s" (mapleline--unicode-number
                 (int-to-string (window-numbering-get-number)))))

(mapleline-define version-control
  :if (and vc-mode (vc-state (buffer-file-name)))
  :priority 56
  :format
  (format "%s:%s" "Git" (vc-state (buffer-file-name))))

(mapleline-define major-mode
  :priority 75
  :format
  (powerline-major-mode))

(mapleline-define buffer-info
  :format
  (format "%s %s %s"
          (powerline-raw "%*")
          (powerline-buffer-size)
          (string-trim
           (mapleline--string-trim-from-center
            (powerline-buffer-id
             (if (powerline-selected-window-active) 'mode-line-buffer-id
               'mode-line-buffer-id-inactive))
            45))))

(mapleline-define flycheck
  :if (bound-and-true-p flycheck-mode)
  :format
  (format "%s%s%s"
          (propertize
           (or (mapleline-flycheck-lighter 'info) "")
           'face 'flycheck-fringe-info)
          (propertize
           (or (mapleline-flycheck-lighter 'warning) "")
           'face 'flycheck-fringe-warning)
          (propertize
           (or (mapleline-flycheck-lighter 'error) "")
           'face 'flycheck-fringe-error)))

(mapleline-define selection-info
  :if (and (powerline-selected-window-active) (region-active-p))
  :priority 75
  :format
  (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
         (chars (- (1+ (region-end)) (region-beginning)))
         (multi-line (> lines 1)))
    (cond
     (multi-line (format "%d lines" lines))
     (t (format "%d chars" chars)))))

(mapleline-define count
  :priority 75
  :format
  (format "%s | %s:%s"
          (let ((buf-coding (format "%s" buffer-file-coding-system)))
            (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                (match-string 1 buf-coding) buf-coding)) "%l" "%c"))

(mapleline-define screen
  :priority 79
  :format  "%p ")

(mapleline-define hud
  :if powerline-display-hud
  :priority 79
  :format
  (powerline-render (list (powerline-hud 'mapleline-highlight (or face 'powerline-active0)))))

(defun mapleline--separator-left(face-left face-right)
  (let ((func (intern (format "powerline-%s-%s"
                              (powerline-current-separator)
                              (car powerline-default-separator-dir)))))
    (powerline-render
     (list (powerline-raw " " face-left)
           (funcall func face-left face-right)
           (powerline-raw " " face-right)))))

(defun mapleline--separator-right(face-left face-right)
  (let ((func (intern (format "powerline-%s-%s"
                              (powerline-current-separator)
                              (cdr powerline-default-separator-dir)))))
    (powerline-render
     (list (powerline-raw " " face-left)
           (funcall func face-left face-right)
           (powerline-raw " " face-right)))))

(defun mapleline-display(displays face0 face1 &optional before after)
  "Return DISPLAYS FACE0 FACE1."
  (cl-reduce
   (lambda(x y)
     (let* ((odd (cl-oddp (/ (length x) 2)))
            (d (cdr y))
            (face (plist-get d :face))
            (after (if (plist-member d :after) (plist-get d :after) t))
            (ds (funcall (car y) (or face (if odd face0 face1))))
            (sep (if odd (mapleline--separator-right (or face face0) face1)
                   (mapleline--separator-left (or face face1) face0))))
       (if (or (not ds) (string-equal ds ""))
           (append x)
         (append x (list ds (when after sep))))))
   displays :initial-value (list)))

(defun mapleline-init()
  "Setup the default modeline."
  (let* ((active (powerline-selected-window-active))
         (face0  (if active 'powerline-active0 'powerline-inactive0))
         (face1  (if active 'powerline-active1 'powerline-inactive1))
         (face2  (if active 'powerline-active2  'powerline-inactive2))
         (face3  (if active 'mapleline-highlight  'powerline-inactive1))
         (left `((mapleline--window-number
                  :face ,face3)
                 (mapleline--buffer-info)
                 (mapleline--major-mode)
                 (mapleline--flycheck)
                 (mapleline--version-control)))
         (right '((mapleline--selection-info)
                  (mapleline--count)
                  (mapleline--screen
                   :after nil)
                  (mapleline--hud
                   :after nil))))
    (mapleline-define-fill (+ 4.5 (powerline-width (mapleline-display right face0 face1))))
    (mapleline-display (append left '((mapleline--fill)) right) face0 face1)))

(defun mapleline-default-theme ()
  "Setup the default modeline."
  (interactive)
  (setq-default mode-line-format '("%e" (:eval (mapleline-init)))))

(provide 'maple-modeline)
