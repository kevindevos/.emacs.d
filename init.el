;; Notes
;; setq stands for set quoted, so (setq variable_name 4) is the same as (set 'variable_name 4)
;; we do variable_name to access the value of said variable (or function potentially)
;; we do 'variable_name to access only the variable's symbol itself
;; the p in stringp, boundp, comandp , etc... stands for predicate, a predicate has the purpose of either returning t or nil (boolean style)

;; QUOTING LAMBDAS ?
;; There is no need to quote a lambda expression because lambda is a macro that expands into (function (lambda ...)).
;; (function ...) is more or less equivalent to (quote ...) and that is equivalent to '... – using function instead of quote just helps the byte compiler a little bit.


;; !!!!!!!!!!!!!
;; Refactoring in emacs projects
;; I now use helm-ag to find all instances of the function name (searches in all files, incl. subdirs, not just in open buffers), and
;; then I use C-c C-e to enter a buffer that lists all the matches and there I change the function name. When I am done I press 
;; C-c C-c (helm-ag--edit-commit) to store the changes to all the opened files.


;; Package management and add the melpa package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; source https://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;; CONSTANTS
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
(load-theme 'sanityinc-tomorrow-eighties t) ;; last parameter t to enable NO_COMFIRM 

;; telephone line ( power line)
(require 'telephone-line)
(telephone-line-mode 1)

;; open linum-mode for all files
(add-hook 'find-file-hook 'linum-mode)

;; set default company backends ( does not include company-eclim company-xcode and some others that were included beforehand
(use-package company
  :config
  (progn
    (setq company-idle-delay 0.2)
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
;; to surround a word with ' for example
;; do ys<textobject>   where textobject is determined by the evil keys like w b e W B E ..  etc
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


;; LANGUAGE SPECIFIC CONFIG
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-hook 'java-mode-hook (lambda () (load-file "~/.emacs.d/lang/java.el")))
(add-hook 'emacs-lisp-mode-hook (lambda () (load-file "~/.emacs.d/lang/elisp.el")))
(add-hook 'csharp-mode-hook (lambda () (load-file "~/.emacs.d/lang/csharp.el")))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" default)))
 '(package-selected-packages
   (quote
    (rainbow-delimiters omnisharp google-this flycheck-gradle lispy helm-projectile meghanada origami hideshow-org ag helm-ag evil-surround color-theme-sanityinc-tomorrow telephone-line zone-nyan plan9-theme flycheck yasnippet git-gutter+ company neotree projectile magit general helm evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; show emacs-init-time on startup
(message "Initialized in %s" (emacs-init-time))



