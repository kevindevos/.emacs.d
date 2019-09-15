(require 'flycheck)
(use-package flycheck :ensure t
  :config
  (flymake-mode-off)
  (flycheck-mode t)
  (setq flycheck-clang-include-path (list "/Applications/JUCE/modules/"))
  )

(set-buffer-file-coding-system 'unix)

(use-package cquery :ensure t
  :init
  (setq cquery-executable "/Users/kevindevos/cquery/build/cquery"))

(use-package lsp-mode :ensure t
  :config
  (lsp))
