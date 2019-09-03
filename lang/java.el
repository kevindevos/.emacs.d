;; Java emacs configuration

(require 'projectile)

(require 'flycheck)
(flycheck-mode)				
(flycheck-gradle-setup)

(require 'eglot)
(if (projectile-project-p)
    (eglot-ensure))

(require 'ggtags)
(ggtags-mode)

(setq lsp-java-autobuild-enabled nil)

