;; Package management and add the melpa package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Garbage Collection higher threshold
(setq gc-cons-threshold 200000000)

;; source https://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; constants 
(defconst my-dotfile-path "~/.emacs.d/init.el")
(defconst my-keybindings-file-path "~/.emacs.d/keybindings.el")

;; set emacs starting buffer to the scrath buffer
(setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))
(setq initial-scratch-message "")

;; general configuration
(tool-bar-mode -1) ;; disable gui tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; default emacs to start maximized
(set-scroll-bar-mode nil) ;; disable scrollbar in all buffers 
(setq neo-show-hidden-files t) ;; show hidden files in neotree
(set-face-attribute 'default nil :font "Hack" :height 125) ;; set font size to 12pt , value is 1/10 pt

;; disable ring bell sound 
(setq ring-bell-function 'ignore) 

;; theme
(load-theme 'sanityinc-tomorrow-eighties t)
;overide neotheme font face for files
;; (defface neo-file-link-face
;;   '((((background dark)) (:foreground "Black"))
;;     (t                   (:foreground "Black")))
;;   "*Face used for open file/dir in neotree buffer."
;;   :group 'neotree :group 'font-lock-highlighting-faces)
;; (defvar neo-file-link-face 'neo-file-link-face)
;; (defface neo-dir-link-face
;;   '((((background dark)) (:foreground "Dark" :weight bold))
;;     (t                   (:foreground "Dark" :weight bold)))
;;   "*Face used for expand sign [+] in neotree buffer."
;;   :group 'neotree :group 'font-lock-highlighting-faces)
;; (defvar neo-dir-link-face 'neo-dir-link-face)

;; telephone line ( power line)
(require 'telephone-line)
(telephone-line-mode 1)

;; open linum-mode for all files
(global-linum-mode)

;; set default company backends ( does not include company-eclim company-xcode and some others that were included beforehand
(use-package company
  :config
  (progn
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0)
    (setq company-backends '(company-bbdb company-semantic company-clang company-capf company-keywords
				      company-files company-dabbrev-code company-gtags company-etags 
				      company-oddmuse company-dabbrev))
    (global-company-mode)
    )
  )

(use-package evil
  :config (evil-mode 1)
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
;; to delete surrounding is ds<textobject>

;; use evil in *Packages* buffer
(add-to-list 'evil-buffer-regexps '("*Packages*" . normal))
(add-to-list 'evil-buffer-regexps '("*Backtrace*" . normal))
(add-to-list 'evil-buffer-regexps '("*Help*" . normal))
(add-to-list 'evil-buffer-regexps '("*info*" . normal))


(require 'which-key)
(which-key-mode)

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

;; folding
(require 'origami)
(global-origami-mode 1)

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; projectile
(require 'projectile)
(projectile-mode)

;; swap meta key from option key to the command key 
(setq mac-option-modifier nil) ;; alt
(setq mac-command-modifier 'meta) ;; command

(require 'helm-projectile)
(require 'info)
;; general makes it easy to manage keybindings
(use-package general
  :config
  (load my-keybindings-file-path)
  )

;; define a specific folder for emacs backup files ( backup files are those appended with ~
(setq backup-directory-alist '("~/.saves"))
(setq make-backup-files nil)
(setq auto-save-default nil)


;; language specific configuration
;; java eglot, set classpath before hook
(require 'eglot)
(defconst my/eclipse-jdt-home "/Users/kevindevos/Documents/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_1.5.500.v20190715-1310.jar")
(defun my/eclipse-jdt-contact (interactive)
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" my/eclipse-jdt-home))
    (unwind-protect
        (eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))
(setcdr (assq 'java-mode eglot-server-programs) #'my/eclipse-jdt-contact)

(load-file "~/.emacs.d/gtags.el")
(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-hook 'java-mode-hook (lambda () (load-file "~/.emacs.d/lang/java.el")))
(add-hook 'emacs-lisp-mode-hook (lambda () (load-file "~/.emacs.d/lang/elisp.el")))
(add-hook 'csharp-mode-hook (lambda () (load-file "~/.emacs.d/lang/csharp.el")))


(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
           ; (projectile-project-root)
           (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#000000" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#eaeaea"))
 '(beacon-color "#d54e53")
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".class"))
 '(custom-safe-themes
   '("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" default))
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.class"))
 '(hl-paren-background-colors '("#e8fce8" "#c1e7f8" "#f8e8e8"))
 '(hl-paren-colors '("#40883f" "#0287c8" "#b85c57"))
 '(package-selected-packages
   '(helm-gtags ggtags dap-mode helm-lsp company-lsp lsp-mode eglot all-the-icons android-mode rainbow-delimiters omnisharp google-this flycheck-gradle lispy helm-projectile origami hideshow-org ag helm-ag evil-surround color-theme-sanityinc-tomorrow telephone-line zone-nyan plan9-theme flycheck yasnippet git-gutter+ company neotree projectile magit general helm evil use-package))
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build" "gradle" ".gradle"))
 '(projectile-globally-ignored-file-suffixes '(".class" ".bin" ".lock" ".jar"))
 '(projectile-indexing-method 'hybrid)
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; show emacs-init-time on startup
(message "Initialized in %s" (emacs-init-time))



