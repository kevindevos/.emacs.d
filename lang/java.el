;; Java emacs configuration

(require 'projectile)

(require 'flycheck)
(flycheck-mode)				
(flycheck-gradle-setup)

(require 'eglot)
(eglot)

;; (require 'lsp-java)
;; (lsp)

;; (use-package company-lsp :commands company-lsp :ensure t)
;; (setq company-backends '(company-lsp))
;; (push 'company-lsp company-backends)

;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list) 

;; (setq lsp-enable-indentation nil)
;; (setq lsp-java-autobuild-enabled nil)
(setq company-idle-delay 0.2)
