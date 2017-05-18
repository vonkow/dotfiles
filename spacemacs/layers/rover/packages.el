(setq rover-packages
      '(
        ))

(defun rover/init-rover-mode ()
  ;; (Maybe set this only in the project proper
  ;;  (or be really cool and tunnel into the docker instance to use its env))
  (setq python-shell-virtualenv-path
        "/Users/caz/.virtualenvs/roverweb"))
