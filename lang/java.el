;; Java emacs configuration

(require 'meghanada)
(meghanada-mode 1)
(setq projectile-indexing-method 'alien)

(require 'projectile)
(add-to-list 'projectile-globally-ignored-directories "app/.meghanada")
