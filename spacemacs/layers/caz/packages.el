(setq caz-packages
      '(
        org
        ))

(defun caz/init-caz-mode ()

  (hl-line-mode -1)
  (global-hl-line-mode -1)
  ;; Confirm before quitting
  (setq confirm-kill-emacs 'y-or-n-p)
  ;; Hopefully get path vars correctly
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  )
(defun caz/post-init-org ()

  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-global-properties
        '(("Effort" . "0:10 0:30 1:00 2:00 4:00 8:00 0:00")
          ("Writer" . "Caz")
          ("Author" . "vonkow")
          ))
  (setq org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w)
                        ("@home" . ?h)
                        (:endgroup . nil)
                        ("plants" . ?p)
                        ("tea" . ?t)))

  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "~/org/notes.org" "Tasks")
            "* TODO %?\n  %i\n  %a")
          ("p" "Programming Task" entry (file+headline "~/org/programming-notes.org" "Tasks")
            "* TODO %?\n  %i\n  %a")
          ("w" "Work Task" entry (file+headline "~/org/work-notes.org" "Tasks")
            "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
            "* %?\nAdded: %U\n  %i\n  %a")
          ("T" "Tea Journal" entry (file+headline "~/org/tea.org" "Tea Journal")
            "** %U\n%?\n %i")
          ("s" "Spirits Notes" entry (file+headline "~/org/spirits.org" "Random Notes")
            "** %?\nAdded: %U\n  %i")
          ))

  )
