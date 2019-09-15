(require 'flycheck)
(flycheck-mode)				

(set-buffer-file-coding-system 'unix)

(use-package lsp-mode :ensure t
  :init
  (lsp))


(use-package cquery :ensure t
  :init
  (setq cquery-executable "/Users/kevindevos/cquery/build/cquery"))

(flymake-mode-off)
(flycheck-mode t)
