(use-package twilight-bright-theme :ensure t
  :config
  (progn
    (load-theme 'twilight-bright t)
    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(isearch ((t (:background "DodgerBlue1" :foreground "SlateGray1"))))
       `(lazy-highlight ((t (:background "LightBlue2" :weight bold))))
       `(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 5)))))
       `(company-scrollbar-bg ((t (:background ,(color-darken-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-darken-name bg 15)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
       `(company-scrollbar-bg ((t (:background "#ffffffffffff"))))
       `(company-scrollbar-fg ((t (:background "#ffffffffffff"))))
       `(neo-dir-link-face ((t (:foreground "DodgerBlue3" :weight bold))))
       `(neo-file-link-face ((t (:foreground "dark cyan"))))
       `(company-tooltip ((t (:inherit default :background "#ffffffffffff"))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(helm-header ((t (:inherit header-line :background "LightBlue4" :foreground "seashell2"))))
       `(helm-match ((t (:foreground "LightBlue3" :weight bold))))
       `(helm-selection ((t (:background "PaleTurquoise" :distant-foreground "black"))))
       `(helm-source-header ((t (:weight bold :height 1.3 :family "Sans Serif"))))
       `(lsp-ui-peek-header ((t (:background "dark cyan" :foreground "black"))))
       `(lsp-ui-sideline-code-action ((t nil)))
       `(telephone-line-projectile ((t (:inherit mode-line :foreground "dark cyan" :weight bold))))
       `(magit-diff-added ((t (:foreground "lime green"))))
       `(magit-diff-added-highlight ((t (:background "PaleGreen1" :foreground "sea green"))))
       `(magit-diff-base ((t (:foreground "gold3"))))
       `(magit-diff-context-highlight ((t (:background "gray70" :foreground "gray100"))))
       `(magit-diff-file-heading-highlight ((t (:inherit magit-diff-file-heading :background "gray85"))))
       `(magit-diff-hunk-heading ((t (:background "gray90" :foreground "grey70"))))
       `(magit-diff-hunk-heading-highlight ((t (:background "gray90" :foreground "gray0" :weight bold))))
       `(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight :foreground "chocolate3"))))
       `(magit-diff-lines-heading ((t (:background "wheat1" :foreground "salmon4"))))
       `(magit-diff-removed ((t (:background "#eecccc" :foreground "#553333"))))
       `(magit-diff-removed-highlight ((t (:background "#eecccc" :foreground "#553333" :weight bold))))
       `(magit-diff-whitespace-warning ((t (:background "IndianRed1"))))
       `(magit-section-highlight ((t (:background "gray90" :foreground "grey20" :weight bold))))))
    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 5)))))
       `(company-scrollbar-bg ((t (:background ,(color-darken-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-darken-name bg 15)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
    )
  )					

