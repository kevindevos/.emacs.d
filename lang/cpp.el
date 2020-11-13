
(use-package ccls
  :after projectile
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

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
  ;; TODO specific c++ flycheck versions for diferent projects
  ;; for now temporarily set flycheck to c++17
  ;; (setq flycheck-gcc-language-standard "c++17")
  ;; Note that we use clang here, not gcc
  (setq flycheck-clang-language-standard "c++17")
  (setup-flycheck-clang-project-path)
  )

(set-buffer-file-coding-system 'unix)

;; (use-package cquery :ensure t
;;   :init
;;   (setq cquery-executable "/Users/kevindevos/cquery/build/cquery"))

(use-package lsp-mode :ensure t
  :config
  (lsp)
  (setq flycheck-checker 'c/c++-clang)
  )

;; Disable flymake 
(flymake-mode -1)
(setq lsp-prefer-flymake nil)

