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

;; buffer file encoding
(set-buffer-file-coding-system 'mac)

;; source https://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


(use-package ggtags
  :ensure t
  :config
  (setq ggtags-use-idutils t)
  (setq ggtags-use-project-gtagsconf nil)
  (setq ggtags-global-mode 1)
  (setq ggtags-oversize-limit 104857600)
  (setq ggtags-sort-by-nearness t)
  )

(setq evil-want-keybinding 'nil)
(use-package evil
  :ensure t
  :config
  (evil-mode 1)  ;; use evil in *Packages* buffer
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Backtrace*" . normal))
  (add-to-list 'evil-buffer-regexps '("*Help*" . normal))
  (add-to-list 'evil-buffer-regexps '("*info*" . normal))
  (use-package evil-collection :ensure t
    :custom (evil-collection-company-use-tng nil)
    :init (evil-collection-init))
  )
(use-package evil-surround :ensure t :config (global-evil-surround-mode 1))

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

;; (use-package material-theme :ensure t
;;   :config
;;   (load-theme 'material-light t)
;;   )
(use-package doom-themes :ensure t
  :config
  (load-theme 'doom-sourcerer t)
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


(load-file "~/.emacs.d/gtags.el")
(load-file "~/.emacs.d/help-fns.el")
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

(with-eval-after-load "flycheck"
    (setq flycheck-clang-warnings `(,@flycheck-clang-warnings
                                    "no-pragma-once-outside-header")))

(use-package evil-ediff :ensure t :after ediff)
(use-package groovy-mode :ensure t) ;; for .gradle files
(use-package python-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-hook 'python-mode-hook (lambda () (load-file "~/.emacs.d/lang/python.el"))))

;; language config hooks
(setf (cdr (rassoc 'c-or-c++-mode auto-mode-alist)) 'c++-mode) ;; ERROR consp nil?
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode) '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-hook 'java-mode-hook (lambda () (load-file '"~/.emacs.d/lang/java.el")))
(add-hook 'emacs-lisp-mode-hook (lambda () (load-file "~/.emacs.d/lang/elisp.el")))
(add-hook 'csharp-mode-hook (lambda () (load-file "~/.emacs.d/lang/csharp.el")))
(add-hook 'c++-mode-hook (lambda () (load-file "~/.emacs.d/lang/cpp.el")))
(add-hook 'after-save-hook 'my-flycheck-project-buffers)

;; Flycheck all buffers of current project
(defun my-flycheck-project-buffers ()
  "Check syntax with flycheck on every buffer of current project with some major modes."
  (interactive)
  (if (and (bound-and-true-p flycheck-mode) (projectile-project-p))
      (let ((buffers (projectile-project-buffers)))
	;; remove current buffer and check it first
	(setq buffers (delete (current-buffer) buffers))
	(flycheck-buffer)
	;; for each buffer , switch to it, if has extension, run flycheck on it
	(save-window-excursion
	  (dolist (buffer buffers)
	    (when (valid-major-mode-p (buffer-mode buffer))
	      (message (concat "Flycheck checking buffer " (buffer-name buffer)))
	      (switch-to-buffer buffer)
	      (flycheck-buffer))
	    (message (concat "Flycheck checked syntax for current project's open buffers.")))))
    (message "Not in Project or flycheck-mode disabled.")))

(defun buffer-mode (buffer)
  "Get 'major-mode' of specified BUFFER."
  (with-current-buffer buffer major-mode))

(defun valid-major-mode-p (mode)
  "Check whether the specfied major MODE is a valid mode for flycheck checking."
  (member mode '(c++-mode java-mode emacs-lisp-mode)))

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
 '(company-box-enable-icon t)
 '(company-box-icons-alist 'company-box-icons-icons-in-terminal)
 '(company-box-show-single-candidate t)
 '(company-show-numbers t)
 '(company-tooltip-limit 30)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".class"))
 '(custom-safe-themes
   '("8c75e2bdf8d1293c77a752dd210612cfb99334f7edd360a42a58a8497a078b35" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "a9c619535d63719a15f22e3c450a03062d3fed1e356ef96d33015849c4c43946" "4a9f595fbffd36fe51d5dd3475860ae8c17447272cf35eb31a00f9595c706050" "669e05b25859b9e5b6b9809aa513d76dd35bf21c0f16d8cbb80fb0727dc8f842" "41039913efab185af1ec1b13ff4df36d6941994d5e3dee39791f30fcd94b42be" "5c9a906b076fe3e829d030a404066d7949e2c6c89fc4a9b7f48c054333519ee7" "b60f08ddc98a95485ec19f046a81d5877b26ab80a67782ea5b91a00ea4f52170" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "de43de35da390617a5b3e39b6b27c107cc51271eb95cceb1f43d13d9647c911d" "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c" "6cbf6003e137485fb3f904e76fb15bc48abc386540f43f54e2a47a9884e679f6" "bc99493670a29023f99e88054c9b8676332dda83a37adb583d6f1e4c13be62b8" "5091eadbb87fa0a168a65f2c3e579d1a648d764f12ab9d3ab7bdefca709cd2a5" "32fd809c28baa5813b6ca639e736946579159098d7768af6c68d78ffa32063f4" "9d54f3a9cf99c3ffb6ac8e84a89e8ed9b8008286a81ef1dbd48d24ec84efb2f1" "a4b9eeeabde73db909e6b080baf29d629507b44276e17c0c411ed5431faf87dd" "15ba8081651869ec689c9004288bed79003de5b4ee9c51a9d4a208d9e3439706" "eb94e44599a45c495ad472ad551a40b87cbc4bae9631211e7a78d72b102c61b1" "1897b97f63e91a792e8540c06402f29d5edcbfb0aafd64b1b14270663d6868ee" "a02836a5807a687c982d47728e54ff42a91bc9e6621f7fe7205b0225db677f07" "4b0b568d63b1c6f6dddb080b476cfba43a8bbc34187c3583165e8fb5bbfde3dc" "a4fa3280ffa1f2083c5d4dab44a7207f3f7bcb76e720d304bd3bd640f37b4bef" "c6b93ff250f8546c7ad0838534d46e616a374d5cb86663a9ad0807fd0aeb1d16" "92d8a13d08e16c4d2c027990f4d69f0ce0833c844dcaad3c8226ae278181d5f3" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "acfac6b14461a344f97fad30e2362c26a3fe56a9f095653832d8fc029cb9d05c" "e396098fd5bef4f0dd6cedd01ea48df1ecb0554d8be0d8a924fb1d926f02f90f" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" default))
 '(debug-on-error t)
 '(evil-auto-indent t)
 '(evil-collection-company-use-tng nil nil nil "may break company auto complete and cause weird behavior")
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#424242")
 '(flycheck-check-syntax-automatically
   '(save idle-change idle-buffer-switch new-line mode-enabled))
 '(flycheck-clang-standard-library nil)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-disabled-checkers nil)
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(frame-background-mode 'dark)
 '(ggtags-global-output-format 'ctags)
 '(gradle-mode t)
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.class"))
 '(gtags-auto-update t)
 '(hl-paren-background-colors '("#e8fce8" "#c1e7f8" "#f8e8e8"))
 '(hl-paren-colors '("#40883f" "#0287c8" "#b85c57"))
 '(hl-sexp-background-color "#efebe9")
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
 '(jdee-db-active-breakpoint-face-colors (cons "#1d2127" "#dd8844"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1d2127" "#858253"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1d2127" "#494952"))
 '(lsp-auto-guess-root t)
 '(lsp-enable-indentation nil)
 '(lsp-face-highlight-read ((t (:foreground "#DFDFDF" :weight bold))))
 '(lsp-face-highlight-textual ((t (:foreground "#DFDFDF" :weight bold))))
 '(lsp-face-highlight-write ((t (:foreground "#DFDFDF" :weight bold))))
 '(lsp-java-code-generation-generate-comments nil)
 '(lsp-ui-doc-alignment 'frame)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-max-height 75)
 '(lsp-ui-doc-max-width 60)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-sideline-delay 0.1)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-diagnostics t)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol nil)
 '(neo-show-hidden-files t)
 '(neo-theme 'ascii)
 '(neo-window-width 35)
 '(objed-cursor-color "#aa4450")
 '(package-selected-packages
   '(doom-themes cquery lsp-python-ms lsp-python python-mode lsp-clangd groovy-mode gradle-mode flymake-gradle evil-ediff evil-magit dired material-theme git-gutter rainbow-mode twilight-bright-theme wgrep helm-swoop lsp helm-config google-c-style flycheck-pos-tip lsp-ui spacemacs-theme company-box lsp-treemacs flucui-themes all-the-icons lsp-java helm-gtags ggtags dap-mode helm-lsp company-lsp lsp-mode eglot android-mode rainbow-delimiters omnisharp google-this flycheck-gradle lispy helm-projectile origami hideshow-org ag helm-ag evil-surround color-theme-sanityinc-tomorrow telephone-line zone-nyan plan9-theme flycheck yasnippet company neotree projectile magit general helm evil use-package))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "build" "gradle" ".gradle" ".cquery_cached_index"))
 '(projectile-globally-ignored-file-suffixes '(".class" ".bin" ".lock" ".jar"))
 '(projectile-indexing-method 'alien)
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
 '(helm-swoop-target-line-face ((t (:background "gray100" :foreground "MediumPurple1" :inverse-video t))))
 '(lsp-face-highlight-read ((t (:foreground "#DFDFDF" :weight bold))))
 '(lsp-face-highlight-textual ((t (:foreground "#DFDFDF" :weight bold))))
 '(lsp-face-highlight-write ((t (:foreground "#DFDFDF" :weight bold))))
 '(region ((t (:background "gray58" :foreground "black")))))

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

;; Disable eldoc-mode globally
(global-eldoc-mode -1)

;; force off debug on error 
(setq debug-on-error nil)

;; remove dabbrev backend to prevent autocompletion in comments
(setq company-backends (delete 'company-dabbrev company-backends))

;; show emacs-init-time on startup
(message "Initialized in %s" (emacs-init-time))
