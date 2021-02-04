(require 'package)

;;; Code:

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Init

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "Your version of Emacs does not support SSL connections"))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Packages

; Load direnv current directory configuration into environemnt
(use-package direnv
  :config
  (direnv-mode))
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package smex
  :ensure t
  :init (setq smex-save-file "~/.emacs.d/.smex-items")
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))
(use-package magit
  :ensure t
  :bind ("C-x C-z" . magit-status))
(use-package goto-last-change
  :ensure t
  :bind ("C-x C-/" . goto-last-change))
(use-package paredit
  :ensure t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'scheme-interaction-mode-hook (lambda () (paredit-mode +1)))))
(use-package avy
  :ensure t
  :bind ("M-p" . avy-goto-char-timer))
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))
(use-package cider
  :ensure t
  :init (progn
          (setq cider-show-error-buffer nil)
          (setq cider-show-error-buffer 'only-in-repl)
          (setq cider-repl-display-help-banner nil)
          (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
          (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
          (setenv "LEIN_USE_BOOTCLASSPATH" "no")))
;; Helm, Projectile
(use-package helm
  :ensure t
  :bind ("C-x M-f" . helm-find-files))
(use-package helm-projectile
  :ensure t
  :bind ("C-x C-f" . helm-projectile-find-file))
(use-package helm-cider
  :ensure t
  :init (helm-cider-mode 1))
(use-package helm-swoop :ensure t)
(use-package flycheck
  :ensure flycheck
  :init (progn
          (global-flycheck-mode)
          (setq-default flycheck-disabled-checkers '(go-golint))))
;; (use-package flycheck-golangci-lint
;;   :ensure flycheck-golangci-lint
;;   :hook (go-mode . flycheck-golangci-lint-setup))
(use-package moe-theme :ensure t)
(use-package beacon :ensure t)
(use-package smart-tab :ensure t)
(use-package powerline
  :ensure t
  :init (powerline-default-theme))
(use-package  multiple-cursors
  :ensure t
  :bind ("C-S-<mouse-1>" . mc/add-cursor-on-click))
(use-package sublimity
  :ensure sublimity
  ;; :init (progn
  ;;         (sublimity-mode)
  ;;         ;;(require 'sublimity-scroll)
  ;;         (require 'sublimity-attractive)
  ;;         ;;(setq sublimity-auto-hscroll-mode 't)
  ;;         )
  )
(use-package git-timemachine :ensure t)
;; (use-package autopair
;;   :ensure t
;;   :init (autopair-global-mode))
(use-package smartparens :ensure t)
;; Languages
(use-package dockerfile-mode :ensure t)
(use-package js2-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package protobuf-mode :ensure t)
(use-package emmet-mode :ensure t)
(use-package rust-mode
  :ensure t
  ;; :init (progn
  ;;         (setq rust-format-on-save t)
  ;;         (racer-mode)
  ;;         (smartparens-mode)
  ;;         ;; (add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "<f10>") 'rust-format-buffer)))
  ;;         )
  )
(use-package racer
  :ensure t
  :init (global-eldoc-mode))
(use-package cargo
  :ensure t
  :init (let ((path (concat (getenv "HOME") "/.cargo/bin")))
          (setenv "PATH" (concat (getenv "PATH") ":" path))
          (setq exec-path (append exec-path (list path)))))
(use-package rustic
  :ensure t
  :init (progn
          (setq racer-rust-src-path (concat (shell-command-to-string "rustc +nightly --print sysroot") "/lib/rustlib/src/rust/library"))
          (setq racer-cmd "/home/alvatar/.carg/bin/racer")
          (add-hook 'rustic-mode-hook #'smartparens-mode)
          (add-hook 'rustic-mode-hook #'racer-mode)
          (add-hook 'rustic-mode-hook #'eldoc-mode)
          (setq company-tooltip-align-annotations t)
          ;;(remove-hook 'rustic-mode-hook 'flycheck-mode)
          ;;(push 'rustic-clippy flycheck-checkers)
          ;;(setq rustic-flycheck-clippy-params "--message-format=json")
          )
  :bind (("<f10>" . 'rustic-format-file)
         ("C-c C-v" . 'lsp-execute-code-action)
         ("TAB" . 'company-indent-or-complete-common)))
(use-package go-mode
  :ensure t
  :init (progn (setq gofmt-command "goimports")
               (add-hook 'go-mode-hook
                         (lambda ()
                           (setq gofmt-command "goimports")
                           (local-set-key (kbd "<f10>") 'gofmt)
                           (smartparens-mode)))
               ;; (let* ((home (getenv "HOME"))
               ;;        (go-path (concat home "/go")))
               ;;   (setenv "GOPATH" go-path)
               ;;   ;;(setenv "GOROOT" go-path)
               ;;   (setenv "PATH" (concat go-path "/bin:" (getenv "PATH")))
               ;;   (setenv "PATH" (concat home "/go/bin:" (getenv "PATH")))
               ;;   (setq exec-path (cons (concat go-path "/bin") exec-path)))
               ;; (setq exec-path (append
               ;;   (list
               ;;    "/usr/local/bin"
               ;;    ;; (or (getenv "GOVERSION") "1.14")
               ;;    (concat (getenv "HOME") "/go/" "/bin")
               ;;    (concat (getenv "HOME") "go/bin"))
               ;;   exec-path))
               ;; (setenv "PATH" (concat (getenv "HOME") "/go/bin:/usr/local/bin:/usr/local/Gambit/bin:" (getenv "PATH")))
               ))
;; (use-package go-autocomplete
;;   :ensure go-autocomplete)
;; Auto-completion
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 1))
(use-package company-go :ensure t)
(use-package company-fuzzy
  :ensure t
  :init (global-company-fuzzy-mode 1))
;; LSP
(use-package lsp-mode
  :ensure t
  :hook (go-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init (progn
          (setq lsp-gopls-staticcheck t)
          (setq lsp-eldoc-render-all t)
          (setq lsp-gopls-complete-unimported t)))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (("<f6>" . rust-test)
         ("<f7>" . rust-compile)
         ("<f8>" . 'lsp-treemacs-errors-list)
         ("C-c 1" . 'lsp-ui-peek-find-definitions)
         ("C-c 2" . 'lsp-ui-peek-find-references)))
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   )
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)
(use-package dap-mode :ensure t)
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))
(use-package posframe :ensure t)
(use-package dap-mode
  :ensure t
  :init (progn
          (dap-mode 1)
          (dap-ui-mode 1)
          (require 'dap-go)
          ;; enables mouse hover support
          (dap-tooltip-mode 1)
          ;; use tooltips for mouse hover
          ;; if it is not enabled `dap-mode' will use the minibuffer.
          (tooltip-mode 1)
          ;; displays floating panel with debug buttons
          (dap-ui-controls-mode 1)
          (add-hook 'dap-stopped-hook
                    (lambda (arg) (call-interactively #'dap-hydra)))))
(dap-register-debug-template
 "Launch Exchange"
 (list :type "go"
       :request "launch"
       :name "Launch Exchange"
       :mode "debug"
       :program "/Users/alvatar/projects/infra/cmd/myprogram/main.go"
       :buildFlags "-gcflags '-N -l'"
       :dlvToolPath "/Users/alvatar/go/bin/dlv"
       :args nil
       :env nil
       :envFile nil))
(use-package ein :ensure t)
;; Themes
(use-package clues-theme :ensure t)
(use-package night-owl-theme :ensure t)
(use-package gruvbox-theme :ensure t)


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; General Config

;; No backups
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)
;; Silent bell
(setq ring-bell-function 'ignore)
;; It makes indentation use spaces.
(setq-default indent-tabs-mode nil)
;; Sometimes the mini-buffer becomes multi-line, and it can be a bit annoying as
;; you type in it. This makes it stay one line.
(setq resize-mini-windows nil)
;; Startup screen
(setq inhibit-startup-buffer-menu t)
(setq inhibit-splash-screen t)
;; Show matching parenthesis globaly.
(show-paren-mode 1)
;; Use UTF-8 by default
(set-language-environment "UTF-8")
;; Show  trailing whitespace
(add-hook 'find-file-hook (lambda () (setf show-trailing-whitespace t)))
;; Confirmation menu
(fset 'yes-or-no-p 'y-or-n-p)
;; Transient mark
(transient-mark-mode 1)
;; Allow downcase region (?)
(put 'downcase-region 'disabled nil)
;; Look & feel
(menu-bar-mode 0)
(column-number-mode 1)
(line-number-mode 1)
(set-default 'truncate-lines t)
(blink-cursor-mode 0)
(global-font-lock-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)
(setq ns-right-alternate-modifier nil)
(set-fringe-mode '(1 . 1))
(if (display-graphic-p)
    ;; GUI
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      ;;(color-theme-sanityinc-tomorrow-day)
      ;; Fonts
      (if (eq system-type 'darwin)
          ;; OSX
          (progn
            ;; default Latin font (e.g. Consolas)
            (set-face-attribute 'default nil :family "Hack")
            ;; default font size (point * 10)
            ;; WARNING!  Depending on the default font,
            ;; if the size is not supported very well, the frame will be clipped
            ;; so that the beginning of the buffer may not be visible correctly.
            (set-face-attribute 'default nil :height 115)
            ;; Prevent opening a dialog on OSX (buggy)
            (defadvice yes-or-no-p (around prevent-dialog activate)
              "Prevent yes-or-no-p from activating a dialog"
              (let ((use-dialog-box nil)) ad-do-it))
            (defadvice y-or-n-p (around prevent-dialog-yorn activate)
              "Prevent y-or-n-p from activating a dialog"
              (let ((use-dialog-box nil)) ad-do-it)))
        ;; Linux
        (progn
          ;(set-default-font "Hack")
          (set-face-attribute 'default t :font "Hack")
          (set-face-attribute 'default nil :height 100))))
  ;; Console
  (progn
    (custom-set-variables
     '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
     '(custom-safe-themes
       (quote
        ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default))))
    (color-theme-sanityinc-tomorrow-bright)
    (custom-set-faces
     '(font-lock-comment-face ((t (:background "#666666" :foreground "black" :slant italic))))
     '(font-lock-comment-delimiter-face ((t (:background "#666666" :foreground "black" :slant italic)))))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Custom functions

(defun rm-trailing-spaces ()
  "Remove spaces at ends of all lines."
  (interactive)
  (save-excursion
    (let ((current (point)))
      (goto-char 0)
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match "" nil nil))
      (goto-char current))))

(defun filter-with-shell-command (command arg)
  "Run a command with the buffer as input and replace it."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (shell-command-on-region (point-min) (point-max) command t t))

(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Global Key bindings

(global-unset-key (kbd "C-x C-c"))

(global-set-key (kbd "<C-up>") 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x t") 'treemacs)
;; (global-set-key (kbd "<f8>") 'flycheck-list-errors)
(global-set-key (kbd "<f9>") 'flycheck-next-error)
;; (global-set-key (kbd "<f9>") 'flymake-goto-next-error)
(global-set-key (kbd "C-c 3") 'helm-do-grep-ag)


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Languages

;; C
(setq c-default-style "linux"
      c-basic-offset 4)

;; Shell
;; (add-hook 'shell-mode-hook (lambda ()
;;                              (compilation-shell-minor-mode 1)
;;                              (setq compilation-auto-jump-to-first-error 1)))

;; Scheme
;; (add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (local-unset-key (kbd "C-c C-c"))
;;             (setq scheme-program-name "/usr/local/Gambit/bin/gsc")))
;; (font-lock-add-keywords 'scheme-mode
;;                         '(("(\\(lambda\\)\\>" (0 (prog1 ()
;;                                                    (compose-region (match-beginning 1)
;;                                                                     (match-end 1)
;;                                                                    ?λ))))))
;; (global-set-key "\C-c\C-qr" 'run-scheme)
;; ;; (add-hook 'scheme-mode-hook (define-key scheme-mode-map "\C-c\C-i" 'scheme-import-file))
;; ;; Load remote SchemeSpheres remote debugging if installed
;; (let ((sense-emacs "~/Dropbox/projects/sphere-energy/src/remote/sense-emacs.el"))
;;   (when (file-exists-p sense-emacs)
;;     (load-file sense-emacs)
;;     (message "Emacs Sense loaded")))

;;------------------------------------------------------------------------------

(provide '.emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" default))
 '(package-selected-packages
   '(company-racer direnv edit-server xwwp flymake-cursor flymake-diagnostic-at-point dap-go posframe uml-mode night-owl-theme gruvbox-theme abyss-theme clues-theme gotham-theme smartparens cargo helm-swoop protobuf-mde yasnippet helm-cider dap-mode helm-lsp helm-imenu lsp-mode moe-theme tron-theme company-fuzzy company-go company flycheck-golangci-lint emmet-mode go-mode yaml-mode markdown-mode js2-mode dockerfile-mode autopair git-timemachine sublimity multiple-cursors powerline smart-tab beacon flycheck helm-projectile helm cider ace-window avy paredit goto-last-change smex expand-region use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

(load-theme 'gruvbox-dark-hard)
