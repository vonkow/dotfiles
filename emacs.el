
;; 1. Init

;; Follow your symlinks frist and foremost
(setq vc-follow-symlinks t)

;; Add other package repos
(require 'package)
(add-to-list 'package-archives
             (cons "melpa" "https://melpa.org/packages/"))
(package-initialize)

;; Use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t)

;; Fix pathing
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Banish custom to its own file
(setq custom-file "~/init-custom.el")
(load-file custom-file)

;; No bars and stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Scroll better
(setq
  scroll-step 1
  scroll-conservatively 10000)

;; Don't beep
(setq ring-bell-function 'ignore)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Auto-refresh buffers if file changed on disk
(global-auto-revert-mode t)

;; Start here
(setq inhibit-splash-screen t
      initial-buffer-choice "~/.emacs.el")

;; utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Theme
(global-font-lock-mode 1)

(use-package white-theme)
(load-theme 'white t)
(custom-set-faces
  '(mode-line ((t (:foreground "white" :background "#585858" :box nil))))
  '(mode-line-inactive ((t (:foreground "#E8E8E8" :background "#B8B8B8" :box nil))))
  '(persp-selected-face ((t (:weight bold :foreground "#F8F8F8")))))
(set-face-attribute 'mode-line-buffer-id nil :background "#B8B8B8" :foreground "white")
(defface mode-line-directory
  '((t :background "#B8B8B8" :foreground "#F8F8F8"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

;; Font
(cond ((find-font (font-spec :name "Operator Mono"))
       (set-face-attribute 'default nil
         :family "Operator Mono"
         :height 160
         :weight 'book
         :width 'normal))
      ((find-font (font-spec :name "IBM Plex Mono"))
       (set-face-attribute 'default nil
         :family "IBM Plex Mono"
         :height 140
         :weight 'normal
         :width 'normal))
      (t
       (set-face-attribute 'default nil
         :family "PT Mono"
         :height 150
         :weight 'normal
         :width 'normal)))

;; Evil and friends
(use-package evil
  :demand
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Keybinding setup
(use-package which-key
  :demand
  :config
  (which-key-mode 1))

(use-package general
  :demand
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hyprid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-create-definer global-leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  (global-leader
    "" '(nil :which-key "global leader")
    "SPC" '(execute-extended-command :which-key "execute"))
  (general-create-definer local-leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix ",")
  (local-leader
    "" '(nil :which-key "local leader")
    "SPC" '(execute-extended-command :which-key "execute")))

;; Helm
(use-package helm-core)

(use-package helm
  :config (progn
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode 1)))
    ;(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
    ;(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
    ;(define-key helm-map (kbd "C-z") #'helm-select-action)

(use-package helm-descbinds)

(use-package helm-ag)

;; Buffers

(defvar buffer-global-map (make-sparse-keymap) "Buffer shortcuts.")
(global-leader "b" '(:keymap buffer-global-map :wk "buffers"))
(general-define-key
  :keymaps 'buffer-global-map
  :wk-full-keys nil
  "b" '(buffer-menu :wk "buffers")
  "s" '(switch-to-buffer :wk "switch")
  "k" '(kill-buffer :wk "kill buffer")
  "n" '(next-buffer :wk "next")
  "p" '(previous-buffer :wk "prev"))

;; Perspective

(use-package perspective
  :demand
  :config
  (persp-mode))

(global-leader "s" '(:keymap perspective-map :wk "perspective"))
(general-define-key
  :keymaps 'perspective-map
  :wk-full-keys nil
  "l" '(persp-switch-quick :wk "quick switch"))

;; Files

(defvar file-global-map (make-sparse-keymap) "file shortcuts")
(global-leader "f" '(:keymap file-global-map :wk "files"))
(general-define-key
  :keymaps 'file-global-map
  :wk-full-keys nil
  "f" '(find-file :wk "find file")
  "F" '(helm-projectile-find-file-dwim :wk "find file dwim"))

;; Directories

(defvar dirs-global-map (make-sparse-keymap) "directory shortcuts")
(global-leader "d" '(:keymap dirs-global-map :wk "directories"))
(general-define-key
  :keymaps 'dirs-global-map
  :wk-full-keys nil
  "d" '(dired :wk "dired"))

(add-hook 'dired-mode-hook
  (lambda ()
      (dired-hide-details-mode)))

;; Terminal

(use-package multi-term)
(setq multi-term-program "/bin/zsh")

(defvar term-local-map (make-sparse-keymap) "term local shortcuts")
(global-leader "t" '(:keymap term-local-map :wk "term"))
(general-define-key
  :keymaps 'term-local-map
  :package 'multi-term
  :wk-full-keys nil
  "n" '(multi-term-next :wk "next")
  "p" '(multi-term-prev :wk "prev")
  "c" '(multi-term :wk "create"))

;; Search/Ag

(use-package ag
  :config
  (add-hook 'ag-mode-hook 'toggle-truncate-lines)
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

;; Projectile

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile)
(helm-projectile-on)

(defvar project-global-map (make-sparse-keymap) "project shortcuts")
(global-leader "p" '(:keymap project-global-map :wk "projectile"))
(general-define-key
  :keymaps 'project-global-map
  :wk-full-keys nil
  "d" '(helm-projectile-find-dir :wk "find dir")
  "f" '(projectile-find-file :wk "find file")
  "s" '(helm-projectile-ag :wk "search")
  "w" '(helm-projectile-switch-project :wk "switch project"))

;; Magit

(use-package magit)
(use-package evil-magit)

(defvar git-global-map (make-sparse-keymap) "git shortcuts")
(global-leader "g" '(:keymap git-global-map :which-key "git"))
(general-define-key
  :keymaps 'git-global-map
  :wk-full-keys nil
  "b" '(magit-blame :wk "blame")
  "s" '(magit-status :wk "magit"))

(local-leader
  :package 'magit
  :definer 'minor-mode
  :states 'normal
  :keymaps 'with-editor-mode
  "," '(with-editor-finish :wk "commit")
  "c" '(with-editor-finish :wk "commit")
  "k" '(with-editor-cancel :wk "cancel"))


;; Python
(use-package company)

(use-package jedi
  :config
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package elpy
  :config
  (elpy-enable)
  (elpy-use-ipython))


;; Checking
(use-package flycheck
  :init
  (global-flycheck-mode)
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  ; python
  (setq flycheck-python-flake8-executable "flake8")
  ; js
  (setq flycheck-disabled-checkers
    (append flycheck-disabled-checkers
            '(javascript-jshint
              Json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'jsx-mode))

;; JS(X) CSS HTML
(use-package js2-mode)

(use-package rjsx-mode)

;;Clone https://github.com/an-sh/flow-minor-mode to .emacs.d
(add-to-list 'load-path "~/.emacs.d/flow-minor-mode/")
(require 'flow-minor-mode)
(add-hook 'rjsx-mode-hook 'flow-minor-enable-automatically)

;(defun flow-parse-xml-initializer (orig-fun gorig-fun)
  ;"Suppress XML parsing if < is just part of a flow type def."
  ;(if ("< is preceded by a alphanum(_,?), then suppress its highlighting")
      ;(flow-parse-top-xml)
    ;(apply orig-fun gorig-fun))
;  (apply orig-fun gorig-fun))

;(advice-add 'rjsx-parse-xml-initializer :around #'flow-parse-xml-initializer)

(defun flow-parse-top-xml ()
  "Parse a top level XML fragment."
  (message "It's a message!")
  (js2-set-face (js2-current-token-beg) (js2-current-token-end) 'rjsx-tag-bracket-face 'record)
  (let ((pn (make-flow-type-node)))
    pn))

;; To print Flow<Paramthings> like normal and nor jsx
;; we will create our own AST node/definition for them
;(cl-defstruct (flow-type-node
               ;(:include js2-node)
               ;(:constructor make-flow-type-node (&key type
                                                       ;(pos (js2-current-token-beg))
                                                       ;(len (- js2-ts-cursor pos)))))
  ;"AST node to represent <Things> in Flow.")

;(js2--struct-put 'flow-type-node 'js2-visitor 'js2-visit-none)
;(js2--struct-put 'flow-type-node 'js2-printer 'flow-print-type-node)

;(defun flow-print-type-node (n i)
  ;(insert (js2-make-pad i)
          ;(js2-node-string n)))


;; Uncomment when ready
(with-eval-after-load 'flycheck
  ;(flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  ;(flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
  )
;;
;;(with-eval-after-load 'company
  ;;(add-to-list 'company-backends 'company-flow))


;; PROLLY DELETE THIS
;; Clone https://github.com/CodyReichert/flowmacs to .emacs.d
;(add-to-list 'load-path "~/.emacs.d/flowmacs/")
;(require 'flowmacs)

;(add-hook 'rjsx-mode-hook 'flowmacs-mode)

;; org mode

(use-package org
  :ensure t
  :init ()
  :config
  (setq org-capture-templates
        '(("t" "TODO" entry (file+olp+datetree "~/org/rover.org")
           "**** TODO %?\n")
          ("i" "IP Review" entry (file+headline "~/org/rover.org" "IP Reviews")
           "** TODO %?\n")))
  (setq org-agenda-files '("~/org/rover.org"))
  (setq org-archive-location "%s_archive::datetree/")
  (general-define-key
    :package 'org
    :major-modes 'org-mode
    :states 'normal
    :keymaps 'org-mode-map
    "TAB" 'org-cycle
    "t" 'org-todo)
  )

(defvar org-global-map (make-sparse-keymap) "org shortcuts")

(global-leader "o" '(:keymap org-global-map :which-key "org"))
  (general-define-key
    :keymaps 'org-global-map
    :wk-full-keys nil
      "c" '(org-capture :wk "capture")
      "l" '(org-store-link :wk "store link")
      "a" '(org-agenda :wk "agenda")
      "b" '(org-iswitchb :wk "switch buffers"))

(defvar org-local-map (make-sparse-keymap) "org local shortcuts")

(general-define-key
  :keymaps 'org-local-map
  :package 'org
  :wk-full-keys nil
  "," '(org-ctrl-c-ctrl-c :wk "C-c C-c")
  "/" '(org-sparse-tree :wk "sparse tree")
  "f" '(helm-org-in-buffer-headings :wk "find heading") ; gonna do an
  "g" '(org-mark-ring-goto :wk "goto back") ; experiment here
  "p" '(org-set-property :wk "set property")
  "r" '(org-priority :wk "set priority")
  "t" '(org-set-tags-command :wk "set tags")
  "d" '(org-deadline :wk "Deadline")
  "s" '(org-archive-subtree :wk "Archive Subtree")
  "z" '(org-schedule :wk "Schedule")
  ">" '(outline-demote :wk "Outline Demote")
  "<" '(outline-promote :wk "Outline Promote")
  "e" '(org-export-dispatch :wk "Export"))

(local-leader
  :package 'org
  :major-modes '(org-mode t)
  :keymaps 'normal
  "" '(:keymap org-local-map :wk "org local"))

(local-leader
  :package 'org
  :definer 'minor-mode
  :states 'normal
  :keymaps 'org-capture-mode
  "," '(org-capture-finalize :wk "finish")
  "c" '(org-capture-finalize :wk "finish")
  "w" '(org-capture-refile :wk "refile")
  "k" '(org-capture-kill :wk "kill"))

(provide 'emacs)
;;; emacs.el ends here
