;; Java emacs configuration

(require 'projectile)

(require 'flycheck)
(flycheck-mode)				
(flycheck-gradle-setup)

(require 'lsp-java)
(lsp)

(defun lsp-java-generate-constructor ()
  (interactive)
  (lsp-execute-code-action-by-kind "source.generate.constructor"))

;; indentation style
(setq c-basic-offset 4
      tab-width 4
      indent-tabs-mode t)

(require 'all-the-icons)
;; (require 'eglot)
;; (if (projectile-project-p)
;;     (eglot-ensure))

(require 'ggtags)
(ggtags-mode)

(setq company-backends (delete "company-capf" company-backends))
