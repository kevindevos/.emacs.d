
(defconst my/eclipse-jdt-home "/Users/kevindevos/Documents/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins//org.eclipse.equinox.launcher_1.5.500.v20190715-1310.jar")

(defun test (a b)
  (+ a b))

(test 4 6)


;;;;;;;;;;;;,
(defun is-the-jar (path)
  (and (string-match-p "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$" path))
  )

(defun get-classpath ()
  (or (getenv "CLASSPATH") ":"))


(setenv "CLASSPATH" my/eclipse-jdt-home)
(split-string (get-classpath) ":")
(is-the-jar "org.eclipse.equinox.launcher_aklsjdflkasjdf.jar")
(file-exists-p my/eclipse-jdt-home)

(get-classpath)
(is-the-jar my/eclipse-jdt-home)

(string-match-p "\\." ".")





;;;
;; (defun projectile-file-cached-p (file project)
;;   "Check if FILE is already in PROJECT cache."
;;   (member file (gethash project projectile-projects-cache)))

;; (defun projectile-relevant-known-projects ()
;;   "Return a list of known projects."
;;   (pcase projectile-current-project-on-switch
;;    ('remove (projectile--remove-current-project projectile-known-projects))
;;    ('move-to-end (projectile--move-current-project-to-end projectile-known-projects))
;;    ('keep projectile-known-projects)))




;; (defun my-is-file-part-of-project (file) 
;;   (let ((projects (projectile-relevant-known-projects)))
;;     (dolist (x projects)
;;       (if (projectile-file-cached-p file x)
;; 	  (message "Is part of project")
;;       )
;;     )
;;   )
