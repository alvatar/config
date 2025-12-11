(require 'package)

;;; Code:

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Init

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl (warn "Your version of Emacs does not support SSL connections"))
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;;(add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
;;   ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; (package-initialize)

;; (if (not (package-installed-p 'use-package))
;;     (progn
;;       (package-refresh-contents)
;;       (package-install 'use-package)))

;; (require 'use-package)

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))
;; (quelpa
;;  '(quelpa-use-package
;;    :fetcher git
;;    :url "https://github.com/quelpa/quelpa-use-package.git"))
;; (require 'quelpa-use-package)

(use-package quelpa)
(use-package quelpa-use-package)


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Manually loaded packages
(add-to-list 'load-path "~/.emacs.d/external/")


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Tree sitter grammers

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     ))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Basic configuration

(use-package diminish)
;; (use-package direnv ; Load direnv current directory configuration into environemnt
;;   :config (direnv-mode))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package smex
  :init (setq smex-save-file "~/.emacs.d/.smex-items")
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))
(use-package magit
  :bind ("C-x C-z" . magit-status))
(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))
(use-package avy
  :bind ("M-p" . avy-goto-char-timer))
(use-package ace-window
  :bind ("C-x o" . ace-window))
;;
;; Navigation
;;
(use-package beacon :init (beacon-mode))
(use-package powerline :init (powerline-default-theme))
(use-package goto-last-point
 :bind (("C-x w" . goto-last-point-record)
	("C-x e" . goto-last-point)))
(use-package  multiple-cursors
  :bind ("C-S-<mouse-1>" . mc/add-cursor-on-click))
(use-package bm
  :demand t
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  (setq temporary-bookmark-p t)
  :bind (("C-c C-h C-h" . bm-next)
         ("C-c C-j C-j" . bm-previous)
         ("C-c C-g C-g" . bm-toggle)))
(use-package sublimity
  :disabled
  :init
  (progn
    ;; (sublimity-mode)
    ;; (require 'sublimity-scroll)
    (require 'sublimity-attractive)
    ;;(setq sublimity-auto-hscroll-mode 't)
    ))
(use-package git-timemachine)
(use-package autopair
  :disabled
  :init (autopair-global-mode))
(use-package emojify
  :disabled
  :hook (after-init . global-emojify-mode))
(use-package window-purpose
  :config (purpose-mode))
(use-package helm
  :bind ("C-x M-f" . helm-find-files))
(use-package helm-ag)
(use-package projectile
  :init (setq projectile-indexing-method 'hybrid))
(use-package helm-swoop)
(use-package helm-projectile
  :bind (("C-c C-f" . helm-projectile-find-file)
	 ("C-c C-x C-f" . helm-projectile-find-file)
	 ("C-c 3" . 'helm-multi-swoop-projectile)
	 ("C-c 4" . 'helm-do-grep-ag)
	 ("C-c 5" . 'helm-projectile-ag)))
;; Themes
(use-package clues-theme)
(use-package night-owl-theme)
(use-package gruvbox-theme)
(use-package humanoid-themes)
(use-package one-themes)

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Window management magic

;; Use window-purpose!
;; https://github.com/emacsmirror/window-purpose
;; C-c , b	purpose-switch-buffer-with-purpose: switch to a buffer with the same purpose as the current one
;; C-u C-x b	switch-buffer-without-purpose: switch to a buffer, but don't use Purpose for it. Handy for changing the current layout.
;; C-c , d	purpose-toggle-window-purpose-dedicated
;; C-c , D	purpose-toggle-window-buffer-dedicated
;; C-c , 1	purpose-delete-non-dedicated-windows

;; (defun rk/open-compilation-buffer (&optional buffer-or-name shackle-alist shackle-plist)
;;   "Helper for selecting window for opening *compilation* buffers."
;;   ;; find existing compilation window left of the current window or left-most window
;;   (let ((win (or (loop for win = (if win (window-left win) (get-buffer-window))
;;                        when (or (not (window-left win))
;;                                 (string-prefix-p "*compilation" (buffer-name (window-buffer win))))
;;                        return win)
;;                  (get-buffer-window))))
;;     ;; if the window is dedicated to a non-compilation buffer, use the current one instead
;;     (when (window-dedicated-p win)
;;       (let ((buf-name (buffer-name (window-buffer win))))
;;         (unless (string-prefix-p "*compilation" buf-name)
;;           (setq win (get-buffer-window)))))
;;     (set-window-buffer win (get-buffer buffer-or-name))
;;     (set-frame-selected-window (window-frame win) win)))

;; ;; Use the same buffer for opening errors in compilation window
;; (defvar display-buffer-same-window-commands
;;   '(occur-mode-goto-occurrence compile-goto-error))
;; (add-to-list 'display-buffer-alist
;;              '((lambda (&rest _)
;;                  (memq this-command display-buffer-same-window-commands))
;;                (display-buffer-reuse-window
;;                 display-buffer-same-window)
;;                (inhibit-same-window . nil)))

(use-package shackle
  :diminish
  :init (shackle-mode)
  :custom
  (shackle-rules '((compilation-mode :custom rk/open-compilation-buffer :select t)
		   ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
		   ("\\*magit" :regexp t :same t :select t)
		   ("\\*shell.*" :regexp t :same t :select t)
		   ("\\*PowerShell.*" :regexp t :same t :select t)
		   ("\\*Cargo.*" :regexp t :other t :select nil)
		   ("\\*rustic.*" :regexp t :same t :select nil :align 'right)
		   ("*Messages*" :select nil :other t)
		   ("*go-guru-output*" :select t :same t)
		   ("*Proced*" :select t :same t)
		   ("*Buffer List*" :select t :same t)
		   ("\\*Pp Eval" :regexp t :same nil :select t :other t)
		   ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)
		   ;; slime
		   ("*slime-source*" :select nil :same nil :other t)
		   ("*slime-description*" :select nil :other t :inhibit-window-quit t)
		   ("\\*slime-repl" :regexp t :same nil :select nil :other t)
		   ;; ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
		   ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
		   ("*slime-scratch*" :same nil :select t :other t)
		   ;; ert
		   ("*ert*" :select nil :same nil :other t)
		   ;; clojure
		   ("*sesman CIDER browser*" :inhibit-window-quit t :select t :same t)
		   ("\\*cider-repl" :regexp t :same nil :other t)))
  (shackle-default-rule nil))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Language tools

;; Completion
(use-package company
  ;; :diminish
  ;; :bind (:map company-mode-map
  ;; 	      ("<tab>". tab-indent-or-complete)
  ;; 	      ("TAB". tab-indent-or-complete))
  )
(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))
(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))
(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; LSP
(use-package lsp-mode
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; these were nil
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package flycheck) ; (setq-default flycheck-disabled-checkers '(go-golint))
(use-package yasnippet
  :diminish
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (add-hook 'lsp-mode 'yas-minor-mode))
(use-package posframe)

;; ---- Python
(use-package jedi
  :disabled)
(use-package elpy
  :mode ("\\.py\\'" . elpy-enable)
  :disabled
  :init
  (progn
    ;; Note: use pyvenv-activate and point to .venv folder
    ;; C-c C-c evaluates the current python script (or region if something is selected) in an interactive python shell. The python shell is automatically displayed aside of your script.
    ;; C-RET evaluates the current statement (current line plus the following nested lines).
    ;; C-c C-z switches between your script and the interactive shell.
    ;; C-c C-d displays documentation for the thing under cursor. The documentation will pop in a different buffer, that can be closed with q
    ;; (elpy-enable)
    (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
    (setq elpy-shell-starting-directory 'current-directory)))

;; ---- Clojure
(use-package paredit
  :init (progn
          (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'scheme-interaction-mode-hook (lambda () (paredit-mode +1)))))
(use-package cider
  :disabled
  :init (progn
          (setq cider-show-error-buffer nil)
          (setq cider-show-error-buffer 'only-in-repl)
          (setq cider-repl-display-help-banner nil)
          (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
          (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
          (setenv "LEIN_USE_BOOTCLASSPATH" "no")))
(use-package helm-cider
  :disabled
  :init (helm-cider-mode 1))

;; ---- Go
(use-package go-mode
  :disabled
  :config
  (setq gofmt-command "goimports")
  (setq exec-path (append
                   (list
                    "/usr/local/bin"
                    ;; (or (getenv "GOVERSION") "1.14")
                    (concat (getenv "HOME") "/go/" "/bin")
                    (concat (getenv "HOME") "go/bin"))
                   exec-path))
  ;; (smartparens-mode)
  :bind (:map go-mode-map
	      ("<f10>" . 'gofmt)))

;; ---- Rust
(use-package rustic
  :ensure
  :bind
  (:map rustic-mode-map
	("C-c C-c l" . flycheck-list-errors)
        ("C-c C-c a" . lsp-execute-code-action)
        ("C-c C-c r" . lsp-rename)
        ("C-c C-c q" . lsp-workspace-restart)
        ("C-c C-c Q" . lsp-workspace-shutdown)
        ("C-c C-c s" . lsp-rust-analyzer-status)
        ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
        ("C-c C-c d" . dap-hydra)
        ("C-c C-c h" . lsp-ui-doc-glance)
	("C-c 9" . 'flycheck-next-error)
	("C-c 0" . 'next-error)
	;; Maintain the "q" binding for closing the compilation mode, since we are using the
	;; "same" window and otherwise it closes
	:map rustic-compilation-mode-map
	("q" . 'kill-this-buffer))
  :config
  (setq rustic-indent-method-chain nil)
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  ;; uncomment to enable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (("C-c 1" . 'lsp-ui-peek-find-definitions)
         ("C-c 2" . 'lsp-ui-peek-find-references)
	 ("C-c 6" . 'lsp-ui-imenu))
  :custom

  ;; Testing--
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  ;;----
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-delay 1)
  (lsp-ui-doc-enable t))

(use-package cargo
  :init (let ((path (concat (getenv "HOME") "/.cargo/bin")))
          (setenv "PATH" (concat (getenv "PATH") ":" path))
          (setq exec-path (append exec-path (list path)))))

;; Debugging Rust
(use-package exec-path-from-shell ; used by dap-mode
  :init (exec-path-from-shell-initialize))
(use-package dap-mode
  :config
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  ;; (dap-register-debug-template
  ;;  "Rust::GDB Run Configuration"
  ;;  (list :type "gdb"
  ;;        :request "launch"
  ;;        :name "GDB::Run"
  ;; 	 :gdbpath "rust-gdb"
  ;;        :target "target/debug/osmosis_cex_bot --enable-osmosis-binance-atom-stables"
  ;;        :cwd nil))
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
	 ;; uncomment if lldb-mi is not in PATH
         ;; :lldbmipath "path/to/lldb-mi"
         :target "target/debug/osmosis_cex_bot --enable-osmosis-binance-atom-stables"
         :cwd nil)))

;; ---- C
(setq c-default-style "linux"
      c-basic-offset 4)

;; ---- HTML
(use-package emmet-mode
  ;;:disabled
  :mode ("\\.html\\'" . emmet-mode))

;; ---- Javascript / Typescript

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'"; "\\.tsx\\'"
         )
  :hook (typescript-mode . my/typescript-mode-hook)
  :config
  (defun my/typescript-mode-hook ()
    (setq typescript-indent-level 4)))
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;;(before-save . tide-format-before-save)
	 ))
(use-package js2-mode
  :disabled
  :mode ("\\.js\\'" . js2-mode))
(use-package js3-mode
  :disabled)

;; TSX mode
;; To make it work:
;; 1. Install both https://github.com/orzechowskid/tsx-mode.el and https://github.com/orzechowskid/tree-sitter-css-in-js
;; 2. Make sure you've installed tree-sitter for tsx & typescript (treesit-install-language-grammar)
;; 3. Call (css-in-js-mode-fetch-shared-library t)

;; required by tsx-mode
(use-package coverlay :ensure t)
(use-package origami :ensure t)
(require 'tsx-mode)
(add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-mode))
;; C-c t f	origami-toggle-node	toggle code-folding for current region
;; C-c t F	origami-toggle-all-nodes	toggle code-folding for all regions
;; C-c t c	tsx-mode-coverage-toggle	toggle code-coverage overlay


;; ---- Other (TODO: setup mode for automatic loading (see python))
(use-package dockerfile-mode)
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))
(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode))
(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))
(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))
(use-package solidity-mode
  :mode ("\\.sol\\'" . solidity-mode))
(use-package wgsl-mode
  :mode ("\\.wgsl\\'" . wgsl-mode))

;; ---- Shell
;; (add-hook 'shell-mode-hook (lambda ()
;;                              (compilation-shell-minor-mode 1)
;;                              (setq compilation-auto-jump-to-first-error 1)))

;; ---- Scheme
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
;;------------------------------------------------------------------------------

;; Misc config

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
;; Show trailing whitespace
(add-hook 'find-file-hook (lambda () (setf show-trailing-whitespace t)))
;; Confirmation menu
(fset 'yes-or-no-p 'y-or-n-p)
;; Transient mark
(transient-mark-mode 1)
;; Allow downcase region (?)
(put 'downcase-region 'disabled nil)
;; Default enable linum
;; (global-linum-mode 1)
(global-display-line-numbers-mode)
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
      ;; Fonts
      (if (eq system-type 'darwin)
          ;; OSX
          (progn
            (set-face-attribute 'default nil :font "Fira Code")
            (set-face-attribute 'default nil :height 140)
            ;; default Latin font (e.g. Consolas)
            ;;(set-face-attribute 'default nil :family "Hack")
            ;; default font size (point * 10)
            ;; WARNING!  Depending on the default font,
            ;; if the size is not supported very well, the frame will be clipped
            ;; so that the beginning of the buffer may not be visible correctly.
            ;; Prevent opening a dialog on OSX (buggy)
            (defadvice yes-or-no-p (around prevent-dialog activate)
              "Prevent yes-or-no-p from activating a dialog"
              (let ((use-dialog-box nil)) ad-do-it))
            (defadvice y-or-n-p (around prevent-dialog-yorn activate)
              "Prevent y-or-n-p from activating a dialog"
              (let ((use-dialog-box nil)) ad-do-it)))
        ;; Linux
        (progn
	  (print "Running on Linux")
          (set-face-attribute 'default nil :font "FiraCode")
          (set-face-attribute 'default nil :height 90))))
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
;; Copilot

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :ensure t
  :hook (('prog-mode-hook 'copilot-mode)
	 ('define-key copilot-completion-map (kbd "S-<tab>") 'copilot-accept-completion)
	 ('define-key copilot-completion-map (kbd "S-TAB") 'copilot-accept-completion))
  :bind (:map company-mode-map
	      ("C-<tab>". 'copilot-accept-completion)
	      ("C-TAB". 'copilot-accept-completion)))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Aider

;; (use-package aider
;;   :ensure t
;;   :config
;;   ;; For latest claude sonnet model
;;   ;; (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
;;   ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;;   ;; Or gemini model
;;   (setq aider-args '("--model" "gemini"))
;;   (setenv "GEMINI_API_KEY" "AIzaSyA_btYmNwW4I-p-fTe0iGxxhet-TIKjsHk")
;;   ;; Or chatgpt model
;;   ;; (setq aider-args '("--model" "o4-mini"))
;;   ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
;;   ;; Or use your personal config file
;;   ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
;;   ;; ;;
;;   ;; Optional: Set a key binding for the transient menu
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Interactive functions

(defun set-large-font ()
  (interactive)
  (set-face-attribute 'default nil :height 100))

(defun set-small-font ()
  (interactive)
  (set-face-attribute 'default nil :height 70))

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

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Global Key bindings

(global-unset-key (kbd "C-x C-c"))

(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))
(global-set-key (kbd "<C-up>") 'toggle-maximize-buffer)

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x t") 'treemacs)
(global-set-key (kbd "C-x K") (lambda () (interactive) (kill-this-buffer) (delete-window)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Notes of installation

;; -- Go
;; go get golang.org/x/tools/gopls@latest
;; go get golang.org/x/tools/cmd/goimports
;; Add to workspace with lsp-workspace-folders-add
;; -- Rust
;; git clone https://github.com/rust-analyzer/rust-analyzer.git && cd rust-analyzer && cargo xtask install --server















;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(provide '.emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378"
     "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176"
     "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "b73a23e836b3122637563ad37ae8c7533121c2ac2c8f7c87b381dd7322714cd0"
     "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
     "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138"
     "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "1d89fcf0105dd8778e007239c481643cc5a695f2a029c9f30bd62c9d5df6418d"
     "41c478598f93d62f46ec0ef9fbf351a02012e8651e2a0786e0f85e6ac598f599"
     "0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197"
     "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a"
     "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940"
     "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8"
     "47e6f8c23eaea064b89ed1361b5824ee4f9562a8c4a30774ee9ee69f9b9d4f69"
     "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7"
     "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7"
     "f028e1985041fd072fa9063221ee9c9368a570d26bd6660edbd00052d112e8bb"
     "969a67341a68becdccc9101dc87f5071b2767b75c0b199e0ded35bd8359ecd69"
     "511a437aad4bcf848317753f26f35b5a7cd416667122c00e3d8e62a8944bb2c7"
     "147fcba1e6277e4b9a3d07ba90d822dabc0510d6576514967a55afd71393000d"
     "8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16"
     "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62"
     "f99318b4b4d8267a3ee447539ba18380ad788c22d0173fc0986a9b71fd866100"
     "30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e"
     "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588"
     "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1"
     default))
 '(global-company-mode t)
 '(global-emojify-mode t)
 '(package-selected-packages
   '(abyss-theme ace-window aider autopair avy beacon bm cargo cider
                 clues-theme company company-fuzzy company-go
                 company-racer copilot coverlay ctrlf cuda-mode dap-go
                 dap-mode diminish direnv dockerfile-mode e2wm
                 edit-server editorconfig elpy emmet-mode emojify
                 exec-path-from-shell expand-region flycheck
                 flycheck-golangci-lint flymake-cursor
                 flymake-diagnostic-at-point git-timemachine go-mode
                 gotham-theme goto-last-change goto-last-point
                 gruvbox-theme helm helm-ag helm-cider helm-imenu
                 helm-lsp helm-projectile helm-swoop humanoid-themes
                 jedi js2-mode lsp-mode magit markdown-mode
                 material-theme moe-theme multi-web-mode
                 multiple-cursors night-owl-theme one-themes origami
                 paredit posframe powerline protobuf-mde python-black
                 quelpa quelpa-use-package rustic shackle smart-tab
                 smex solidity-mode sublimity tron-theme uml-mode
                 use-package web-mode wgsl-mode window-purpose
                 writeroom-mode xwwp yaml-mode yasnippet))
 '(warning-suppress-types '((comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;;(load-theme 'gruvbox-dark-hard)
(load-theme 'one-dark)
(set-background-color "#101423")

