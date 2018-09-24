;; my general.el keybindings
(defconst my-leader "SPC")
(defconst my-leader-emacs "SPC q")
(defconst my-leader-frames "SPC F")
(defconst my-leader-windows "SPC w")
(defconst my-leader-buffers "SPC b")
(defconst my-leader-files "SPC f")
(defconst my-leader-files-edit "SPC f e")
(defconst my-leader-projectile "SPC p")
(defconst my-leader-imenu "SPC i")
(defconst my-leader-help "SPC I")
(defconst my-leader-meghanada "SPC g")


;; general.el leader key definers

(general-create-definer my-leader-def
  :prefix my-leader)

(general-create-definer my-leader-emacs-def
  :prefix my-leader-emacs)

(general-create-definer my-leader-files-def
  :prefix my-leader-files)

(general-create-definer my-leader-files-edit-def
  :prefix my-leader-files-edit)

(general-create-definer my-leader-windows-def
  :prefix my-leader-windows)

(general-create-definer my-leader-frames-def
  :prefix my-leader-frames)

(general-create-definer my-leader-buffers-def
  :prefix my-leader-buffers)

(general-create-definer my-leader-projectile-def
  :prefix my-leader-projectile)

(general-create-definer my-leader-imenu-def
  :prefix my-leader-imenu)

(general-create-definer my-leader-meghanada-def
  :prefix my-leader-meghanada)

(general-create-definer my-leader-help-def
  :prefix my-leader-help)

; evil tweaks
(global-set-key (kbd "M-/") 'helm-ag-this-file)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "zO") 'origami-open-all-nodes)
  (define-key evil-motion-state-map (kbd "zC") 'origami-close-all-nodes)
  )

;; emacs navigation
(global-set-key (kbd "C-d") '(evil-scroll-down 0))
(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u") (lambda () (interactive) (evil-scroll-up 0)))

;; magit enforce keybinding wtheck?
(global-set-key (kbd "C-x g") 'magit-status)

;; neotree
(use-package neotree
  :config
  (define-key neotree-mode-map (kbd "C-l") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "C-j") 'neotree-enter)
  (define-key neotree-mode-map (kbd "TAB") 'neotree-stretch-toggle)
  (define-key neotree-mode-map (kbd "<tab>") 'neotree-stretch-toggle)
  (define-key neotree-mode-map (kbd "RET") 'neotree-enter)
  (define-key neotree-mode-map (kbd "<return>") 'neotree-enter)
  
  )
;; Use C-c C-c in neotree mode to set dir to root dir

;;;;;;;;;;;;;;;;;;; EMACS ;;;;;;;;;;;;;;;;;;;

(my-leader-emacs-def
  :keymaps 'normal
  "q" 'save-buffers-kill-emacs
  "z" 'suspend-emacs
  )

;; restart emacs within emacs: https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs

;;;;;;;;;;;;;;;;;;; FILES ;;;;;;;;;;;;;;;;;;;

;; "SPC f"
(my-leader-files-def
 :keymaps 'normal
 "f" 'helm-find-files
 "s" 'save-buffer
 "t" 'neotree-toggle
 "r" 'helm-recentf
 "L" 'helm-locate
 )

;; todo helm recentfiles like spacemacs

;; "SPC f e"
(my-leader-files-edit-def
 :keymaps 'normal
 "d" 'my-open-dotfile
  )

(defun my-open-dotfile ()
  (interactive)
  (find-file my-dotfile-path)
  )

;;;;;;;;;;;;;;;;;;;; WINDOWS , frames and buffers    ;;;;;;;;;;;;;;,,

;; "SPC w"
(my-leader-windows-def
 :keymaps 'normal
 "j" 'windmove-down
 "k" 'windmove-up
 "h" 'windmove-left
 "l" 'windmove-right
 "/" 'split-window-horizontally
 "-" 'split-window-vertically
 "d" 'delete-window
 "w" 'ace-window
 )

;; "SPC F"
(my-leader-frames-def
 :keymaps 'normal
 "m" 'toggle-frame-maximized
 )

;; "SPC b"
(my-leader-buffers-def
 :keymaps 'normal
 "b" 'helm-buffers-list
 "d" 'kill-this-buffer
 "p" 'previous-buffer
 "n" 'next-buffer
 )

;;;;;;;;;;;;;;;; Projects with projectile package ;;;;;;;;;;;;;;

;; SPC p
(my-leader-projectile-def
 :keymaps 'normal
 "i" 'projectile-project-info
 "f" 'helm-projectile-find-file
 "F" 'helm-projectile-find-file-in-known-projects
 "p" 'helm-projectile-switch-project
 "t" 'neotree-projectile-action 
 "I" 'projectile-invalidate-cache
 "r" 'helm-projectile-recentf
 "R" 'projectile-remove-known-project
 "sa" 'helm-projectile-ag
 )



;;;;;;;;;;;;;;;;; imenu ;;;;;;;;;;;;;;;;
(my-leader-imenu-def
  :keymaps 'normal
  "i" 'helm-semantic-or-imenu
  )

;;;;;;;;;;;;;;; meghanada ;;;;;;;;;;;
(my-leader-meghanada-def
  :keymaps 'normal
  "b" 'meghanada-back-jump
  "c" 'meghanada-compile-project
  "o" 'meghanada-optimize-import
  "i" 'meghanada-reference ;; searches for references to symbol at point
  "K" 'meghanada-kill-running-process
  "rd" 'meghanada-debug
  "rr" 'meghanada-run-task
  "trt" 'meghanada-run-junit-test-case
  "trc" 'meghanada-run-junit-class
  "tdc" 'meghanada-debug-junit-class
  "jd" 'meghanada-jump-declaration
  "js" 'meghanada-jump-symbol
  "ts" 'meghanada-switch-testcase

)
  ;; meghanada-back-jump is M-,   NOTE: it saves a history, so it works more than once consecutively


;; Help

(my-leader-help-def
  :keymaps 'normal
  "t" 'info 
  )

;; to comment/uncomment use M-;

;; File navigation in helm-find-files
;; use C-l to go up a directory
;; use C-j to go into a selected directory

;; Folding
;; z-c  evil-close-fold
;; z-o  evil-open-fold

;; make a word upper case 
;; M-u

;; TODO neotree stretch toggle
;; TODO programming language specific configs 
;; TODO make tab indent multiple lines that are selected simultaneously
