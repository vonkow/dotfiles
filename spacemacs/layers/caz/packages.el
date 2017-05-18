(setq caz-packages
      '(
        ;;org
        ))

;;(defun caz/init-org ()
  ;;(use-package org
    ;;))

(defun caz/init-caz-mode ()
  '' Maybe do org stuff here once you learn how to?

  ;; Confirm before quitting
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Hopefully get path vars correctly
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
