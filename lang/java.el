;; Java emacs configuration

(require 'projectile)

(require 'flycheck)
(flycheck-mode)				
(flycheck-gradle-setup)

(require 'lsp-java)
(lsp)

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

;; language specific configuration
;; java eglot, set classpath before hook
;; (require 'eglot)
;; (defconst my/eclipse-jdt-home "/Users/kevindevos/Documents/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_1.5.500.v20190715-1310.jar")
;; (defun my/eclipse-jdt-contact (interactive)
;;   (let ((cp (getenv "CLASSPATH")))
;;     (setenv "CLASSPATH" (concat cp ":" my/eclipse-jdt-home))
;;     (unwind-protect
;;         (eglot--eclipse-jdt-contact nil)
;;       (setenv "CLASSPATH" cp))))
;; (setcdr (assq 'java-mode eglot-server-programs) #'my/eclipse-jdt-contact)
