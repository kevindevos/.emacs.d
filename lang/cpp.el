(require 'flycheck)
(defun setup-flycheck-clang-project-path ()
  (let ((root (ignore-errors (projectile-project-root))))
    (when root
      (add-to-list 
       (make-variable-buffer-local 'flycheck-clang-include-path)
       root))))
(use-package flycheck :ensure t
  :config
  (flycheck-mode t)
  (setq flycheck-clang-include-path (list "/Library/Developer/CommandLineTools/SDKs/MacOSX10.15.sdk/usr/include"))
  (setq flycheck-clang-standard-library "libc++")
  (setup-flycheck-clang-project-path)
  )

(set-buffer-file-coding-system 'unix)

(use-package cquery :ensure t
  :init
  (setq cquery-executable "/Users/kevindevos/cquery/build/cquery"))

(use-package lsp-mode :ensure t
  :config
  (lsp)
  (setq flycheck-checker 'c/c++-clang)
  )

;; Disable flymake 
(flymake-mode -1)
(setq lsp-prefer-flymake nil)


