(require-package 'flyspell)
(require-package 'auto-dictionary)
(require-package 'flyspell-correct)
(require-package 'flyspell-correct-helm)

(defvar spell-checking-enable-auto-dictionary nil
  "Specify if auto-dictionary should be enabled or not.")

(defvar enable-flyspell-auto-completion nil
  "If not nil, show speeling suggestions in popups.")

(use-package auto-dictionary
  :defer t
  :if spell-checking-enable-auto-dictionary
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
  :diminish flyspell-mode "⒮"
  :init
  (progn
    ;; use apsell as ispell backend
    (setq-default ispell-program-name "aspell")
    ;; use American English as ispell default dictionary
    (ispell-change-dictionary "american" t)
    (defun spell-checking/add-flyspell-hook (hook)
      "Add `flyspell-mode' to the given HOOK, if
        `*spell-check*' is true."
      (when *spell-check*
        (add-hook hook 'flyspell-mode)))
    (spell-checking/add-flyspell-hook 'text-mode-hook)
    (when *spell-check*
      (add-hook 'prog-mode-hook 'flyspell-prog-mode))))

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic)
  )

(use-package flyspell-correct-helm)

(use-package flyspell-correct-popup)


(provide 'init-spelling)
