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

;; neotree
(use-package neotree
  :config
  (define-key neotree-mode-map (kbd "C-l") 'neotree-select-up-node)
  (define-key neotree-mode-map (kbd "C-j") 'neotree-enter)
  (define-key neotree-mode-map (kbd "[?\\t]") 'neotree-stretch-toggle)
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
;; some of these dont work
(my-leader-projectile-def
 :keymaps 'normal
 "i" 'projectile-project-info
 "f" 'projectile-find-file
 "p" 'projectile-switch-project
 "t" 'neotree-projectile-action 
 )



;;;;;;;;;;;;;;;;; imenu ;;;;;;;;;;;;;;;;
(my-leader-imenu-def
  :keymaps 'normal
  "i" 'helm-semantic-or-imenu
  )

;;;;;;;;;;;;;;;;;;;; EDITING ;;;;;;;;;;;;;,,,,,

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
