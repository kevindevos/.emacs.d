;; Package management and add the melpa package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company neotree projectile magit general helm evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; CONSTANTS
(defconst my-dotfile-path "~/.emacs.d/init.el")
(defconst my-keybindings-file-path "~/.emacs.d/keybindings.el")

;; general configuration
(tool-bar-mode -1) ;; disable gui tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; default emacs to start maximized
(set-scroll-bar-mode nil) ;; disable scrollbar in all buffers 
(setq neo-show-hidden-files t) ;; show hidden files in neotree
(set-face-attribute 'default nil :height 120) ;; set font size to 12pt , value is 1/10 pt

;; open linum-mode for all files
(add-hook 'find-file-hook 'linum-mode)

;; set default company backends ( does not include company-eclim company-xcode and some others that were included beforehand
(use-package company
  :config
  (progn
    (setq company-idle-delay 0.1)
    (setq company-backends '(company-bbdb company-semantic company-clang company-capf company-keywords
				      company-files company-dabbrev-code company-gtags company-etags 
				      company-oddmuse company-dabbrev))
    (global-company-mode)
    )
  )

(require 'evil)
(evil-mode 1)

(require 'which-key)
(which-key-mode)

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

(setq mac-command-modifier 'meta)

;; general makes it easy to manage keybindings
(use-package general
  :config
  (load my-keybindings-file-path)
  )

;; TODO doom emacs speed?
;; TODO yasnippet
;; TODO flycheck
;; TODO auto add to known projects when theres a .projectile file
