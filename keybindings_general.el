;; my general.el keybindings
(defconst my-leader "SPC")
(defconst my-leader-windows "SPC w")
(defconst my-leader-files "SPC f")
(defconst my-leader-files-edit "SPC f e")
(defconst my-leader-frames "SPC F")

(general-create-definer my-leader-def
  :prefix my-leader)

(general-create-definer my-leader-files-def
  :prefix my-leader-files)

(general-create-definer my-leader-files-edit-def
  :prefix my-leader-files-edit)

(general-create-definer my-leader-windows-def
  :prefix my-leader-windows)

(general-create-definer my-leader-frames-def
  :prefix my-leader-frames)


;;;;;;;;;;;;;;;;;;; FILES ;;;;;;;;;;;;;;;;;;;

;; "SPC f"
(my-leader-files-def
 :keymaps 'normal
 "f" 'helm-find-files
 "s" 'save-buffer
 )

;; "SPC f e"
(my-leader-files-edit-def
 :keymaps 'normal
 "d" 'my-open-dotfile
  )

(defun my-open-dotfile ()
  (interactive)
  (find-file my-dotfile-path)
  )

;;;;;;;;;;;;;;;;;;;; WINDOWS and frames ;;;;;;;;;;;;;;,,

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
 )

;; "SPC F"
(my-leader-frames-def
 :keymaps 'normal
 "m" 'toggle-frame-maximized
 )


(message "General.el keybindings defined")


