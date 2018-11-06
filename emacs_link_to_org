(package-initialize)
(setq vc-follow-symlinks t)
(require 'org)

(setq org-confirm-babel-evaluate #'(lambda (lang body)
  (not (or (string= lang "emacs-lisp")
           (string= lang "python")))))
(org-babel-do-load-languages 
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (ditaa . t)))

(org-babel-load-file (expand-file-name "~/init.org"))
