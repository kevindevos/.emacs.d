;; Package management and add the melpa package archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit general helm evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; CONSTANTS
(defconst my-dotfile-path "~/.emacs.d/init.el")

;; general configuration
(tool-bar-mode -1) ;; disable gui tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; default emacs to start maximized

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
  (load "~/.emacs.d/keybindings_general.el")
  )



		
