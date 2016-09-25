(use-package youdao-dictionary
  :defer t
  :config
  (progn
    ;; Enable Cache
    (setq url-automatic-caching t
          ;; Set file path for saving search history
          youdao-dictionary-search-history-file (concat maple-cache-directory "youdao")
          ;; Enable Chinese word segmentation support
          youdao-dictionary-use-chinese-word-segmentation t))
  )

(provide 'init-tool)
