;;; packages.el --- vue layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: 纪清华 <jiqinghua@souche.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(setq vue-packages
  '(
    (vue-mode :location (recipe :fetcher github :repo "codefalling/vue-mode"))
  ))

(defun vue/init-vue-mode ()
    (use-package vue-mode))

;;; packages.el ends here
