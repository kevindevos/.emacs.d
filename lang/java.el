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

(require 'ggtags)
(ggtags-mode)

(setq company-backends (delete "company-capf" company-backends))

(set-buffer-file-coding-system 'unix)

(defun package-name-for-buffer ()
  "Get suitable package for java class."
  (interactive)
  (let* ((projectFileName (file-relative-name (buffer-file-name) (projectile-project-root)))
		 (noPrefix (replace-regexp-in-string "src/main/java/" "" projectFileName))
		 (noBufferName (replace-regexp-in-string (concat "\/" (buffer-name)) "" noPrefix)))
	(replace-regexp-in-string "\/" "\." noBufferName)
	  ))

(replace-regexp-in-string "src\/main\/java/" "" "src/main/java/shopping/view/Main.java")
(replace-regexp-in-string (concat "\/" "Main.java") "" "shopping/view/Main.java")
(replace-regexp-in-string "\/" "\." "shopping/view/test1/test2")

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

