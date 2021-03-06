;; Create minor mode with some keybindings for modes like treemacs
(defvar custom-keybindings-keymap (make-keymap) "global custom keybindings.")
(define-minor-mode custom-keybindings-minor-mode
  "Global keybindings as a minor mode"
  nil
  ;; indicator for mode line
  " CustomKeybindingsMode"
  ;; minor mode keymap
  custom-keybindings-keymap
  ;; make mode global
;;  :global 1
  )

(add-hook 'treemacs-mode-hook (lambda () (custom-keybindings-minor-mode 1)))
(add-hook 'messages-buffer-mode-hook (lambda () (custom-keybindings-minor-mode 1)))

(defvar my-prefix-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map custom-keybindings-keymap)
    map))
(defvar my-windmove-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map my-prefix-map)
    map))
(define-key custom-keybindings-keymap (kbd "SPC") my-prefix-map)
(define-key my-prefix-map (kbd "w") my-windmove-map)
(define-key my-windmove-map (kbd "l") 'windmove-right)
(define-key my-windmove-map (kbd "h") 'windmove-left)
(define-key my-windmove-map (kbd "k") 'windmove-up)
(define-key my-windmove-map (kbd "j") 'windmove-down)


;; my general.el keybindings
(defconst my-leader "SPC")
(defconst my-leader-emacs "SPC q")
(defconst my-leader-emacs-theme "SPC q t")
(defconst my-leader-frames "SPC F")
(defconst my-leader-windows "SPC w")
(defconst my-leader-buffers "SPC b")
(defconst my-leader-files "SPC f")
(defconst my-leader-git "SPC g")
(defconst my-leader-files-edit "SPC f e")
(defconst my-leader-projectile "SPC p")
(defconst my-leader-imenu "SPC i")
(defconst my-leader-help "SPC I")
(defconst my-leader-eval "SPC e")
(defconst my-leader-lsp "SPC l")
(defconst my-leader-search "SPC s")
(defconst my-leader-flycheck "SPC y")

;; general.el leader key definers
;; :keymaps 'override in order to "to prevent your leader keybindings from ever being overridden"
(general-create-definer my-global-leader
  :prefix "S-SPC"
  :keymaps 'custom-keybindings-keymap)
(general-def '(normal visual emacs motion) my-leader (general-simulate-key "S-SPC"))
;;(general-create-definer my-leader-def :prefix my-leader :states '(normal visual emacs treemacs) :keymaps 'override :global-prefix "S-SPC")
(general-create-definer my-leader-emacs-def :prefix my-leader-emacs)
(general-create-definer my-leader-emacs-theme :prefix my-leader-emacs-theme)
(general-create-definer my-leader-files-def :prefix my-leader-files)
(general-create-definer my-leader-files-edit-def :prefix my-leader-files-edit)
(general-create-definer my-leader-windows-def :prefix my-leader-windows)
(general-create-definer my-leader-frames-def :prefix my-leader-frames)
(general-create-definer my-leader-buffers-def :prefix my-leader-buffers)
(general-create-definer my-leader-projectile-def :prefix my-leader-projectile)
(general-create-definer my-leader-imenu-def :prefix my-leader-imenu)
(general-create-definer my-leader-help-def :prefix my-leader-help)
(general-create-definer my-leader-eval :prefix my-leader-eval)
(general-create-definer my-leader-lsp :prefix my-leader-lsp)
(general-create-definer my-leader-flycheck :prefix my-leader-flycheck)
(general-create-definer my-leader-search  :prefix my-leader-search)
(general-create-definer my-leader-git :prefix my-leader-git)

(which-key-add-key-based-replacements my-leader-emacs "emacs")
(which-key-add-key-based-replacements my-leader-emacs-theme "themes")
(which-key-add-key-based-replacements my-leader-files "files")
(which-key-add-key-based-replacements my-leader-files-edit "files")
(which-key-add-key-based-replacements my-leader-windows "windows")
(which-key-add-key-based-replacements my-leader-frames "frames")
(which-key-add-key-based-replacements my-leader-buffers "buffers")
(which-key-add-key-based-replacements my-leader-projectile "projectile")
(which-key-add-key-based-replacements my-leader-imenu "imenu")
(which-key-add-key-based-replacements my-leader-help "help")
(which-key-add-key-based-replacements my-leader-eval "eval")
(which-key-add-key-based-replacements my-leader-lsp "lsp")
(which-key-add-key-based-replacements my-leader-flycheck "flycheck")
(which-key-add-key-based-replacements my-leader-search "search")
(which-key-add-key-based-replacements my-leader-git "git")
(which-key-add-key-based-replacements "SPC l o" "organize")
(which-key-add-key-based-replacements "SPC l g" "generate")
(which-key-add-key-based-replacements "SPC l e" "extract")
(which-key-add-key-based-replacements "SPC l f" "find")
(which-key-add-key-based-replacements "SPC l p" "peek")
(which-key-add-key-based-replacements "SPC l y" "flycheck")
(which-key-add-key-based-replacements "SPC p o" "shell")

(global-set-key (kbd "M-/") 'helm-ag-this-file)
;; Unbind <SPC> in evil-motion-state-map from <right> to nil so SPC can trigger general leader key
(define-key evil-motion-state-map (kbd "<SPC>") 'nil)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "zO") 'origami-open-all-nodes)
  (define-key evil-motion-state-map (kbd "zC") 'origami-close-all-nodes)
  )

(with-eval-after-load 'yasnippet
  (global-set-key (kbd "C-<return>") 'yas-expand)
  (global-set-key (kbd "C-<tab>") 'yas-expand)
  )

;; emacs navigation
(global-set-key (kbd "C-d") '(evil-scroll-down 0))
(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u") (lambda () (interactive) (evil-scroll-up 0)))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; emacs window resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(require 'dired)
(global-set-key (kbd "C-x C-j") 'dired-current-directory)
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "-") 'dired-create-empty-file)

;;;;;;;;;;;;;;;;;;; EMACS ;;;;;;;;;;;;;;;;;;;

(my-leader-emacs-def
  :keymaps 'normal
  "q" 'save-buffers-kill-emacs
  "e" 'eshell
  "z" 'suspend-emacs
  )

(require 'material-theme)
(my-leader-emacs-theme
 :keymaps 'normal
 "l" (lambda () (interactive) (load-theme 'material-light))
 "d" (lambda () (interactive) (load-theme 'material))
 )
;;;;;;;;;;;;;;;;;;; FILES ;;;;;;;;;;;;;;;;;;;

;; "SPC f"
(my-leader-files-def
  :keymaps 'normal
  "f" 'helm-find-files
  "s" 'save-buffer
  "t" 'treemacs
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
  :keymaps '(normal visual)
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
  "I" 'projectile-invalidate-cache
  "r" 'helm-projectile-recentf
  "R" 'projectile-remove-known-project
  "a" 'helm-projectile-ag ;; !
  "s" 'projectile-save-project-buffers
  "os" 'projectile-run-shell
  )


;;;;;;;;;;;;;;;;; imenu ;;;;;;;;;;;;;;;;
(my-leader-imenu-def
  :keymaps 'normal
  "i" 'helm-semantic-or-imenu
  "a" 'helm-imenu-in-all-buffers
  "t" 'package-name-for-buffer
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
  "gc" 'my-lsp-java-generate-constructors
  "gs" 'lsp-java-generate-to-string
  "ec" 'lsp-java-extract-to-constant 
  "fd" 'lsp-find-definition
  "fr" 'lsp-find-references
  "fi" 'lsp-find-implementation
  "fo" 'ff-find-other-file ;; to switch to .h file from .cpp or from .cpp to .h quickly
  "pb" 'lsp-ui-peek-jump-backward
  "pf" 'lsp-ui-peek-jump-forward
  "pr" 'lsp-ui-peek-find-references
  "pi" 'lsp-ui-peek-find-implementation
  "le" 'lsp-treemacs-errors-list
  "s" 'lsp-treemacs-symbols
  ;"yl" 'lsp-ui-flycheck-list ; do not use, does not pass the correct includes to clang for some reason
  )

;; helm swoop M-i

(defun my-lsp-java-generate-constructors ()
  "Generat (interactive)"
  (lsp-execute-code-action-by-kind "source.generate.constructors"))

(my-leader-git
  :keymaps 'normal
  "s" 'magit-status
  "b" 'magit-blame
  "l" 'magit-log
  )

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

