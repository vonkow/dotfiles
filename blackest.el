;;; blackest.el --- Reformat python buffers using the "black" formatter

;; Copyright (C) 2018 Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; Homepage: https://github.com/proofit404/blackest
;; Version: 0.0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Blackest uses black to format a Python buffer.  It can be called
;; explicitly on a certain buffer, but more conveniently, a minor-mode
;; 'blackest-mode' is provided that turns on automatically running
;; black on a buffer before saving.
;;
;; Installation:
;;
;; Add blackest.el to your load-path.
;;
;; To automatically format all Python buffers before saving, add the
;; function blackest-mode to python-mode-hook:
;;
;; (add-hook 'python-mode-hook 'blackest-mode)
;;
;;; Code:


(defgroup blackest nil
  "Reformat Python code with \"black\"."
  :group 'python)

(defcustom blackest-executable "black"
  "Name of the executable to run."
  :type 'string)

(defcustom blackest-line-length nil
  "Line length to enforce."
  :type 'number
  :safe 'numberp)

(defun blackest-call-bin (input-buffer output-buffer error-buffer)
  "Call process black.

Send INPUT-BUFFER content to the process stdin.  Saving the
output to OUTPUT-BUFFER.  Saving process stderr to ERROR-BUFFER.
Return black process the exit code."
  (with-current-buffer input-buffer
    (let ((process (make-process :name "blackest"
                                 :command `(,blackest-executable ,@(blackest-call-args))
                                 :buffer output-buffer
                                 :stderr error-buffer
                                 :noquery t
                                 :sentinel (lambda (process event)))))
      (set-process-query-on-exit-flag (get-buffer-process error-buffer) nil)
      (set-process-sentinel (get-buffer-process error-buffer) (lambda (process event)))
      (save-restriction
        (widen)
        (process-send-region process (point-min) (point-max)))
      (process-send-eof process)
      (accept-process-output process nil nil t)
      (while (process-live-p process)
        (accept-process-output process nil nil t))
      (process-exit-status process))))

(defun blackest-call-args ()
  "Build black process call arguments."
  (append
   (when blackest-line-length
     (list "--line-length" (number-to-string blackest-line-length)))
   '("-")))

;;;###autoload
(defun blackest-buffer (&optional display)
  "Try to blackest the current buffer.

Show black output, if black exit abnormally and DISPLAY is t."
  (interactive (list t))
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (original-window-pos (window-start))
         (tmpbuf (get-buffer-create "*blackest*"))
         (errbuf (get-buffer-create "*blackest-error*")))
    ;; This buffer can be left after previous black invocation.  It
    ;; can contain error message of the previous run.
    (dolist (buf (list tmpbuf errbuf))
      (with-current-buffer buf
        (erase-buffer)))
    (condition-case err
        (if (not (zerop (blackest-call-bin original-buffer tmpbuf errbuf)))
            (error "Black failed, see %s buffer for details" (buffer-name errbuf))
          (unless (eq (compare-buffer-substrings tmpbuf nil nil original-buffer nil nil) 0)
            (with-current-buffer tmpbuf
              (copy-to-buffer original-buffer (point-min) (point-max))))
          (mapc 'kill-buffer (list tmpbuf errbuf))
          (goto-char original-point)
          (set-window-start (selected-window) original-window-pos))
      (error (message "%s" (error-message-string err))
             (when display
               (pop-to-buffer errbuf))))))

;;;###autoload
(define-minor-mode blackest-mode
  "Automatically run black before saving."
  :lighter " Black"
  (if blackest-mode
      (add-hook 'before-save-hook 'blackest-buffer nil t)
    (remove-hook 'before-save-hook 'blackest-buffer t)))

(provide 'blackest)

;;; blackest.el ends here
