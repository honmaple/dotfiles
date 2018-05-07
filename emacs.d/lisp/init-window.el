;;默认分屏
(use-package window
  :ensure nil
  :init (setq split-width-threshold 1))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

(use-package window-numbering
  :hook (after-init . window-numbering-mode))

(use-package popwin
  :hook (after-init . popwin-mode)
  :config
  (setq popwin:special-display-config
        '(("*Help*" :dedicated t :position bottom :stick nil :noselect nil)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.2)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Python*" :dedicated t :position right :stick nil :noselect nil)

          ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)
          ("\*flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)
          ("*Anaconda*" :regexp t :position bottom :stick t :noselect t))))

(use-package golden-ratio  ;;黄金分割
  :diminish golden-ratio-mode
  :hook (after-init . golden-ratio-mode)
  :config
  (setq golden-ratio-exclude-modes '("calc-mode"
                                     "ediff-mode"
                                     "gud-mode"
                                     "term-mode"
                                     "restclient-mode"
                                     "anaconda-view-mode")
        golden-ratio-exclude-buffer-regexp '("^\\*[hH]elm.*"
                                             "^\\*Flycheck.*")
        golden-ratio-exclude-buffer-names '(" *NeoTree*"
                                            "*LV*"
                                            " *which-key*"
                                            "*Ilist*"
                                            " *MINIMAP*")
        golden-ratio-extra-commands nil)
  (defadvice select-window (after select-window activate)
    (golden-ratio)))

(provide 'init-window)
