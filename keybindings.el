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
(defconst my-leader-eval "SPC e")
(defconst my-leader-lsp "SPC l")
(defconst my-leader-search "SPC s")
(defconst my-leader-flycheck "SPC f")

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

(general-create-definer my-leader-help-def
  :prefix my-leader-help)

(general-create-definer my-leader-eval
  :prefix my-leader-eval)

(general-create-definer my-leader-lsp
  :prefix my-leader-lsp)

(general-create-definer my-leader-flycheck
  :prefix my-leader-flycheck)

(general-create-definer my-leader-search 
  :prefix my-leader-search)

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
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

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
  "e" 'eshell
  "z" 'suspend-emacs
  )

;; restart emacs within emacs: https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs

;;;;;;;;;;;;;;;;;;; FILES ;;;;;;;;;;;;;;;;;;;

;; "SPC f"
(my-leader-files-def
 :keymaps 'normal
 "f" 'helm-find-files
 "s" 'save-buffer
 "t" 'neotree-project-dir-toggle
 "r" 'helm-recentf
 "L" 'helm-locate
 "d" 'dired-jump
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
 "i" 'indent-buffer
 )

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
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
 "a" 'helm-projectile-ag ;; !
 "s" 'projectile-save-project-buffers
 "os" 'projectile-run-shell
 )


; SPC i
;;;;;;;;;;;;;;;;; imenu ;;;;;;;;;;;;;;;;
(my-leader-imenu-def
  :keymaps 'normal
  "i" 'helm-semantic-or-imenu
  "a" 'helm-imenu-in-all-buffers
  )

;;;;;;;;;;;;;;;,,
(my-leader-eval
 :keymaps 'normal
 "b" 'eval-buffer
 )

(my-leader-flycheck
  :keymaps 'normal
  "le" 'flycheck-list-errors
  "n" 'flycheck-next-error
  "p" 'flycheck-previous-error
  "q" 'flycheck-first-error
  )

(require 'lsp-mode)
(define-key lsp-mode-map (kbd "C-x m") 'lsp-java-extract-method)
(my-leader-lsp
  :keymaps 'normal
  "d" 'lsp-ui-doc-show
  "h" 'lsp-ui-doc-hide
  "i" 'lsp-ui-imenu
  "r" 'lsp-rename ;; symbol refactor
  "oi" 'lsp-java-organize-imports
  "gi" 'lsp-java-generate-overrides
  "ts" 'lsp-java-generate-to-string
  "ec" 'lsp-java-extract-to-constant
  "fd" 'lsp-find-definition
  "jb" 'lsp-ui-peek-jump-backward
  "jf" 'lsp-ui-peek-jump-forward
  "fl" 'lsp-ui-flycheck-list
  "fr" 'lsp-ui-peek-find-references
  "fi" 'lsp-ui-peek-find-implementation
  )
;; helm swoop M-i


;; to indent whole buffer:
;; Select whole buffer C-x h
;; Indent region C-M-\


;; peek mode keybindings defined already by ls-ui-peek
;; (define-key map (kbd "M-n") 'lsp-ui-peek--select-next-file)
;; (define-key map (kbd "<right>") 'lsp-ui-peek--select-next-file)
;; (define-key map (kbd "M-p") 'lsp-ui-peek--select-prev-file)
;; (define-key map (kbd "<left>") 'lsp-ui-peek--select-prev-file)
;; (define-key map (kbd "C-n") 'lsp-ui-peek--select-next)
;; (define-key map (kbd "n") 'lsp-ui-peek--select-next)
;; (define-key map (kbd "<down>") 'lsp-ui-peek--select-next)
;; (define-key map (kbd "C-p") 'lsp-ui-peek--select-prev)
;; (define-key map (kbd "p") 'lsp-ui-peek--select-prev)
;; (define-key map (kbd "<up>") 'lsp-ui-peek--select-prev)
;; (define-key map (kbd "TAB") 'lsp-ui-peek--toggle-file)
;; (define-key map (kbd "q") 'lsp-ui-peek--abort)
;; (define-key map (kbd "RET") 'lsp-ui-peek--goto-xref)
;; (define-key map (kbd "M-RET") 'lsp-ui-peek--goto-xref-other-window)

(my-leader-search
  :keymaps 'normal
  "f" 'evil-search-forward
  "b" 'evil-search-backward
  "p" 'isearch-forward-symbol-at-point
  )
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

