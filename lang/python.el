(require 'flycheck)
(flycheck-mode)

(require 'lsp-mode)
(use-package lsp-python-ms :ensure t
  :config
  (lsp))
