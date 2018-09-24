;; Java emacs configuration

(require 'meghanada)
(meghanada-mode 1)
(setq projectile-indexing-method 'alien)

(require 'projectile)
(add-to-list 'projectile-globally-ignored-directories "app/.meghanada")

(require 'flycheck)
(flycheck-mode)
(flycheck-gradle-setup)

(add-to-list 'evil-buffer-regexps '("*meghanada-reference*" . normal))

