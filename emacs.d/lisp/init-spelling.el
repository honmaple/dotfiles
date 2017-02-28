(require-package 'flyspell)
(require-package 'auto-dictionary)
(require-package 'flyspell-correct)
(require-package 'flyspell-correct-helm)


(use-package auto-dictionary
  :defer t
  :diminish auto-dictionary-mode
  :init
  (progn
    (add-hook 'flyspell-mode-hook 'auto-dictionary-mode)
    ;; Select the buffer local dictionary if it was set, otherwise
    ;; auto-dictionary will replace it with a guessed one at each activation.
    ;; https://github.com/nschum/auto-dictionary-mode/issues/5
    (defun maple/adict-set-local-dictionary ()
      "Set the local dictionary if not nil."
      (when (and (fboundp 'adict-change-dictionary)
                 ispell-local-dictionary)
        (adict-change-dictionary ispell-local-dictionary)))
    (add-hook 'auto-dictionary-mode-hook
              'maple/adict-set-local-dictionary 'append)))

(use-package flyspell
  :defer t
  :diminish flyspell-mode "⒮"
  :config
  (progn
    ;; use apsell as ispell backend
    (setq-default ispell-program-name "aspell")
    ;; use American English as ispell default dictionary
    (ispell-change-dictionary "american" t)
    ;; (add-hook 'text-mode-hook 'flyspell-mode)
    ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    ;; (add-hook 'flyspell-mode-hook 'maple/set-spell)
    ))

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  )

(use-package flyspell-correct-helm
  :defer t)


(provide 'init-spelling)
