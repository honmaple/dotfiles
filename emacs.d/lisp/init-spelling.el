(use-package auto-dictionary
  :ensure t
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
  :ensure t
  :defer t
  :diminish flyspell-mode "⒮"
  :config
  (progn
    ;; use apsell as ispell backend
    (defun maple/set-spell ()
      ;; use American English as ispell default dictionary
      (setq-default ispell-program-name "aspell")
      (ispell-change-dictionary "american" t))
    (add-hook 'flyspell-mode-hook 'maple/set-spell)
    ;; (add-hook 'text-mode-hook 'flyspell-mode)
    ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    ))

(use-package flyspell-correct
  :ensure t
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  )

(use-package flyspell-correct-helm
  :ensure t
  :defer t)


(provide 'init-spelling)
