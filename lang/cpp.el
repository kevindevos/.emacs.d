(require 'flycheck)
(use-package flycheck :ensure t
  :config
  (flycheck-mode t)
  (setq flycheck-clang-include-path (list "/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/include"))
  (setq flycheck-clang-standard-library "libc++")
  )

(set-buffer-file-coding-system 'unix)

(use-package cquery :ensure t
  :init
  (setq cquery-executable "/Users/kevindevos/cquery/build/cquery"))

(use-package lsp-mode :ensure t
  :config
  (lsp))

;; Disable flymake 
(flymake-mode -1)
(setq lsp-prefer-flymake nil)
