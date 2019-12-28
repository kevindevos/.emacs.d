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

(setq company-backends (delete "company-capf" company-backends))

(set-buffer-file-coding-system 'unix)

(defun package-name-for-buffer ()
  "Get suitable package for java class."
  (interactive)
  (let* ((projectFileName (file-relative-name (buffer-file-name) (projectile-project-root)))
		 (noPrefix (replace-regexp-in-string "src/main/java/" "" projectFileName))
		 (noBufferName (replace-regexp-in-string (concat "\/" (buffer-name)) "" noPrefix)))
	(replace-regexp-in-string "\/" "\." noBufferName)))

