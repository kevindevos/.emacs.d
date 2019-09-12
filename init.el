;; Package management and add the melpa package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; use-package
(eval-when-compile
  (add-to-list 'load-path "elpa")
  (require 'use-package))

;; Garbage Collection higher threshold
(setq gc-cons-threshold 200000000)

;; macOS titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; no proxy icon
(setq ns-use-proxy-icon nil)

;; disable company-mode for eshell only
(setq company-global-modes '(not eshell-mode))

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

;; swap meta key from option key to the command key 
(setq mac-option-modifier nil) ;; alt
(setq mac-command-modifier 'meta) ;; command
(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "M-c") 'evil-yank)
      (global-set-key (kbd "M-v") 'evil-paste-after)
      )
  )

;; define a specific folder for emacs backup files ( backup files are those appended with ~
(setq backup-directory-alist '("~/.saves"))
(setq make-backup-files nil)
(setq auto-save-default nil)

;; general configuration
(tool-bar-mode -1) ;; disable gui tool bar
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-to-list 'default-frame-alist '(width . 112))
(set-scroll-bar-mode nil) ;; disable scrollbar in all buffers 
(setq neo-show-hidden-files t) ;; show hidden files in neotree
(set-face-attribute 'default nil :font "Hack" :height 125) ;; set font size to 12pt , value is 1/10 pt
(set-default-coding-systems 'unix)

;; disable ring bell sound 
(setq ring-bell-function 'ignore) 

;; automatically add ending braces like } on open
(electric-pair-mode)

(use-package material-theme :ensure t
  :config
  (load-theme 'material t)
  )


;; telephone line ( power line)
(use-package telephone-line :ensure t :config (telephone-line-mode 1))


(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 1)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; set default company backends ( does not include company-eclim company-xcode and some others that were included beforehand
(use-package company
  :ensure t
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

(use-package which-key :ensure t :config (which-key-mode))
(use-package helm :ensure t :config (global-set-key (kbd "M-x") 'helm-M-x))
(use-package yasnippet :ensure t :config (yas-global-mode 1))
(use-package projectile :ensure t :config (projectile-mode t))
(use-package helm-projectile :ensure t)
(use-package info :ensure t)
(use-package general :ensure t :config (load my-keybindings-file-path))
(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode 1))
  ;; use evil in *Packages* buffer
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Backtrace*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Help*" . normal))
  (add-to-list 'evil-buffer-regexps '("*info*" . normal))
  )
(use-package evil-surround :ensure t :config (global-evil-surround-mode 1))


(load-file "~/.emacs.d/gtags.el")
(use-package helm-gtags
  :ensure t
  :config
  (progn
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
    )
  )

(use-package flycheck-pos-tip :ensure t :after flycheck :config (flycheck-pos-tip-mode))
(use-package lsp-ui :ensure t :after lsp-mode)
(use-package git-gutter :ensure t :config (global-git-gutter-mode))
(use-package evil-magit :ensure t :after magit
  :config
  ;; Refresh git gutter on all buffers when staging/unstaging
  (add-hook 'magit-post-refresh-hook
            #'git-gutter:update-all-windows)
  )

(use-package evil-ediff :ensure t :after ediff)
(use-package groovy-mode :ensure t) ;; for .gradle files
(use-package python-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-hook 'python-mode-hook (lambda () (load-file "~/.emacs.d/lang/python.el"))))

;; language config hooks
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode) '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-hook 'java-mode-hook (lambda () (load-file "~/.emacs.d/lang/java.el")))
(add-hook 'emacs-lisp-mode-hook (lambda () (load-file "~/.emacs.d/lang/elisp.el")))
(add-hook 'csharp-mode-hook (lambda () (load-file "~/.emacs.d/lang/csharp.el")))
(add-hook 'c++-mode-hook (lambda () (load-file "~/.emacs.d/lang/cpp.el")))

(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
  ;; hand over the word to helm-swoop when doing isearch
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Save buffer when helm-muti-swoop-edit is complete
  (setq helm-multi-swoop-edit-save t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#000000" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#eaeaea"))
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(beacon-color "#d54e53")
 '(c-label-minimum-indentation 'set-from-style)
 '(c-syntactic-indentation t)
 '(c-syntactic-indentation-in-macros t)
 '(company-box-enable-icon nil)
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(company-box-show-single-candidate t)
 '(company-show-numbers t)
 '(company-tooltip-limit 30)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".class"))
 '(custom-safe-themes
   '("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "acfac6b14461a344f97fad30e2362c26a3fe56a9f095653832d8fc029cb9d05c" "e396098fd5bef4f0dd6cedd01ea48df1ecb0554d8be0d8a924fb1d926f02f90f" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" default))
 '(evil-auto-indent t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(gradle-mode t)
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.class"))
 '(hl-paren-background-colors '("#e8fce8" "#c1e7f8" "#f8e8e8"))
 '(hl-paren-colors '("#40883f" "#0287c8" "#b85c57"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(lsp-enable-indentation nil)
 '(lsp-java-code-generation-generate-comments nil)
 '(lsp-ui-doc-alignment 'frame)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-max-height 75)
 '(lsp-ui-doc-max-width 60)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-webkit t)
 '(lsp-ui-sideline-delay 0.1)
 '(lsp-ui-sideline-enable t)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-diagnostics t)
 '(lsp-ui-sideline-show-hover nil)
 '(lsp-ui-sideline-show-symbol nil)
 '(neo-show-hidden-files t)
 '(neo-theme 'ascii)
 '(neo-window-width 35)
 '(package-selected-packages
   '(lsp-python-ms lsp-python python-mode lsp-clangd groovy-mode gradle-mode flymake-gradle evil-ediff evil-magit dired material-theme git-gutter rainbow-mode twilight-bright-theme wgrep helm-swoop lsp helm-config google-c-style flycheck-pos-tip lsp-ui spacemacs-theme company-box lsp-treemacs flucui-themes all-the-icons lsp-java helm-gtags ggtags dap-mode helm-lsp company-lsp lsp-mode eglot android-mode rainbow-delimiters omnisharp google-this flycheck-gradle lispy helm-projectile origami hideshow-org ag helm-ag evil-surround color-theme-sanityinc-tomorrow telephone-line zone-nyan plan9-theme flycheck yasnippet git-gutter+ company neotree projectile magit general helm evil use-package))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
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
 '(window-divider-mode nil)
 '(yas-choose-keys-first t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'neotree)
;; toggle neotree to view the whole project
(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
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
            (company-abort)
          (indent-for-tab-command))))))

;; allow usage of dired-find-alternate-file without warning
(put 'dired-find-alternate-file 'disabled nil)

;; show emacs-init-time on startup
(message "Initialized in %s" (emacs-init-time))
