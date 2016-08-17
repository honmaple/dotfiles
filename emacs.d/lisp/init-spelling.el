(require-package 'flyspell)
;; (require-package 'auto-dictionary)

(use-package flyspell
  :defer t
  :diminish flyspell-mode "⒮"
  :init
  (progn
    ;; use apsell as ispell backend
    (setq-default ispell-program-name "aspell")
    ;; use American English as ispell default dictionary
    (ispell-change-dictionary "english" t)
    (defvar spell-checking-enable-by-default t
      "Enable spell checking by default.")
    (defun spell-checking/add-flyspell-hook (hook)
      "Add `flyspell-mode' to the given HOOK, if
        `spell-checking-enable-by-default' is true."
      (when spell-checking-enable-by-default
        (add-hook hook 'flyspell-mode)))
    (spell-checking/add-flyspell-hook 'text-mode-hook)
    (when spell-checking-enable-by-default
      (add-hook 'prog-mode-hook 'flyspell-prog-mode))))

(provide 'init-spelling)
