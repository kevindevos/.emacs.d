;; c# configuration

(require 'omnisharp)
(require 'company)
(omnisharp-mode)
(add-to-list 'company-backends 'company-omnisharp)

(message "csharp on")
