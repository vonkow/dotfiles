(require 'cl-lib)
(require 'format-spec)
(require 'ox)
(require 'ox-publish)

(defun org-gdoc-bold (_bold contents _info)
  (format "<b>\n%s</b>" contents))

(defun org-gdoc-headline (headline contents info)
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info)))
      (concat title contents))))

(defun org-gdoc-paragraph (paragraph contents info)
  contents)

(defun org-gdoc-plain-text (text info)
  text)

(org-export-define-backend 'gdoc
  '((paragraph . org-gdoc-paragraph)
    (headline . org-gdoc-headline)
    (bold . org-gdoc-bold)
    (plain-text . org-gdoc-plain-text))
  :menu-entry
    '(?g "Export to gDoc"
         ((?g "As gDoc buffer" org-gdoc-export-as-gdoc)))
  )

(defun org-gdoc-export-as-gdoc
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'gdoc "*Org gdoc Export*"
    async subtreep visible-only body-only ext-plist
    (lambda ()
      ; TODO Change to html-mode or something
      (set-auto-mode t))))

(setq debug-on-error t)
