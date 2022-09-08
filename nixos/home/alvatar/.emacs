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
  ;;(add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
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

;;------------------------------------------------------------------------------
;; Basic

(use-package direnv ; Load direnv current directory configuration into environemnt
  :ensure t
  :config (direnv-mode))
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
(use-package avy
  :ensure t
  :bind ("M-p" . avy-goto-char-timer))
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))
;; Helm, Projectile
(use-package helm
  :ensure t
  :bind ("C-x M-f" . helm-find-files))
(use-package helm-projectile
  :ensure t
  :bind (("C-x C-f" . helm-projectile-find-file)
	 ("C-c 3" . 'helm-do-grep-ag)
	 ("C-c 4" . 'helm-projectile-ag)
	 ("C-c 5" . 'helm-multi-swoop-projectile)))
(use-package helm-swoop :ensure t)
(use-package beacon :ensure t :init (beacon-mode))
(use-package powerline :ensure t :init (powerline-default-theme))
(use-package  multiple-cursors
  :ensure t
  :bind ("C-S-<mouse-1>" . mc/add-cursor-on-click))
(use-package sublimity
  :ensure sublimity
  :init (progn
          ;; (sublimity-mode)
          ;; (require 'sublimity-scroll)
          (require 'sublimity-attractive)
          ;;(setq sublimity-auto-hscroll-mode 't)
          )
  )
(use-package git-timemachine :ensure t)
;; (use-package autopair :ensure t :init (autopair-global-mode))
;; Themes
(use-package clues-theme :ensure t)
(use-package night-owl-theme :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package humanoid-themes :ensure t)
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(defun rk/open-compilation-buffer (&optional buffer-or-name shackle-alist shackle-plist)
  "Helper for selecting window for opening *compilation* buffers."
  ;; find existing compilation window left of the current window or left-most window
  (let ((win (or (loop for win = (if win (window-left win) (get-buffer-window))
                       when (or (not (window-left win))
                                (string-prefix-p "*compilation" (buffer-name (window-buffer win))))
                       return win)
                 (get-buffer-window))))
    ;; if the window is dedicated to a non-compilation buffer, use the current one instead
    (when (window-dedicated-p win)
      (let ((buf-name (buffer-name (window-buffer win))))
        (unless (string-prefix-p "*compilation" buf-name)
          (setq win (get-buffer-window)))))
    (set-window-buffer win (get-buffer buffer-or-name))
    (set-frame-selected-window (window-frame win) win)))
(use-package shackle
  :ensure
  :diminish
  :custom
  (shackle-rules '((compilation-mode :custom rk/open-compilation-buffer :select t)
		   ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
		   ("\\*magit" :regexp t :same t :select t)
		   ("\\*shell.*" :regexp t :same t :select t)
		   ("\\*PowerShell.*" :regexp t :same t :select t)
		   ("\\*Cargo.*" :regexp t :other t :select nil)
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
;; Language tools

;; General
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  ;; (company-minimum-prefix-length 1)
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)
(use-package exec-path-from-shell ; used by dap-mode
  :ensure
  :init (exec-path-from-shell-initialize))
;; LSP
(use-package lsp-mode
  :ensure t
  :commands lsp
  :commands (lsp lsp-deferred)
  ;;:hook (go-mode . lsp-deferred))
  :custom
  ;;;; Rust
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;;;; Go
  (lsp-gopls-staticcheck t)
  (lsp-gopls-complete-unimported t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'go-mode 'lsp-deferred))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (("C-c 1" . 'lsp-ui-peek-find-definitions)
         ("C-c 2" . 'lsp-ui-peek-find-references))
  :custom

  ;; Testing--
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  ;;----
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
(use-package flycheck :ensure t) ; (setq-default flycheck-disabled-checkers '(go-golint))
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
(use-package posframe :ensure t)

;; ---- Python
(use-package jedi :ensure t)
(use-package elpy
  :ensure t
  :init
  (progn
    ;; Note: use pyvenv-activate and point to .venv folder
    ;; C-c C-c evaluates the current python script (or region if something is selected) in an interactive python shell. The python shell is automatically displayed aside of your script.
    ;; C-RET evaluates the current statement (current line plus the following nested lines).
    ;; C-c C-z switches between your script and the interactive shell.
    ;; C-c C-d displays documentation for the thing under cursor. The documentation will pop in a different buffer, that can be closed with q
    (elpy-enable)
    (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
    (setq elpy-shell-starting-directory 'current-directory)))

;; ---- Clojure
(use-package paredit
  :ensure t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
          (add-hook 'scheme-interaction-mode-hook (lambda () (paredit-mode +1)))))
(use-package cider
  :ensure t
  :init (progn
          (setq cider-show-error-buffer nil)
          (setq cider-show-error-buffer 'only-in-repl)
          (setq cider-repl-display-help-banner nil)
          (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
          (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
          (setenv "LEIN_USE_BOOTCLASSPATH" "no")))
(use-package helm-cider
  :ensure t
  :init (helm-cider-mode 1))


;; ---- Go
(use-package go-mode
  :ensure t
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
;; (use-package company-go :ensure t)
;; (use-package go-autocomplete
;;   :ensure go-autocomplete)

;; ---- Rust
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
	      ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance)
              ("<f6>" . rust-test)
              ("<f7>" . rust-compile)
	      ("<f8>" . 'next-error)
	      ("<f10>" . 'rustic-format-file))
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
    (setq-local buffer-save-without-query t)))

(use-package cargo
  :ensure t
  :init (let ((path (concat (getenv "HOME") "/.cargo/bin")))
          (setenv "PATH" (concat (getenv "PATH") ":" path))
          (setq exec-path (append exec-path (list path)))))

;; ---- Other
(use-package dockerfile-mode :ensure t)
(use-package js2-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package protobuf-mode :ensure t)
(use-package emmet-mode :ensure t)
(use-package solidity-mode :ensure t)
(use-package toml-mode :ensure)

;; ---- C
(setq c-default-style "linux"
      c-basic-offset 4)

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

;; ---- Debugging
(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
	   :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           ))))

;; Go config
;; (use-package dap-mode
;;   :ensure t
;;   :init (progn
;;           (dap-mode 1)
;;           (dap-ui-mode 1)
;;           (require 'dap-go)
;;           ;; enables mouse hover support
;;           (dap-tooltip-mode 1)
;;           ;; use tooltips for mouse hover
;;           ;; if it is not enabled `dap-mode' will use the minibuffer.
;;           (tooltip-mode 1)
;;           ;; displays floating panel with debug buttons
;;           (dap-ui-controls-mode 1)
;;           (add-hook 'dap-stopped-hook
;;                     (lambda (arg) (call-interactively #'dap-hydra)))))
;; (dap-register-debug-template
;;  "Launch Exchange"
;;  (list :type "go"
;;        :request "launch"
;;        :name "Launch Exchange"
;;        :mode "debug"
;;        :program "/Users/alvatar/projects/infra/cmd/myprogram/main.go"
;;        :buildFlags "-gcflags '-N -l'"
;;        :dlvToolPath "/Users/alvatar/go/bin/dlv"
;;        :args nil
;;        :env nil
;;        :envFile nil))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

;; Extra Config

;; No backups
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)
;; Silent bell
(setq ring-bell-function 'ignore)
;; It makes indentation use spaces.
;;(setq-default indent-tabs-mode nil)
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

;; Tab: force just tab
;; Ref: http://ergoemacs.org/emacs/emacs_tabs_space_indentation_setup.html
(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
;; (global-set-key (kbd "TAB") 'my-insert-tab-char)

;; Tab: yasnippet, indent or complete
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

;;------------------------------------------------------------------------------
;; Global Key bindings

(global-unset-key (kbd "C-x C-c"))

(global-set-key (kbd "<C-up>") 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x t") 'treemacs)
(global-set-key (kbd "C-x K") (lambda () (interactive) (kill-this-buffer) (delete-window)))

;;------------------------------------------------------------------------------
;; Snippets

(defun set-large-font ()
  (interactive)
  (set-face-attribute 'default nil :height 100))

(defun set-small-font ()
  (interactive)
  (set-face-attribute 'default nil :height 70))


;;------------------------------------------------------------------------------
;; Notes of installation

;; -- Go
;; go get golang.org/x/tools/gopls@latest
;; go get golang.org/x/tools/cmd/goimports
;; Add to workspace with lsp-workspace-folders-add
;; -- Rust
;; git clone https://github.com/rust-analyzer/rust-analyzer.git && cd rust-analyzer && cargo xtask install --server




;;; hoon-mode.el --- Major mode for editing hoon files for urbit

;; Copyright (C) 2014–2016 Urbit

;; Author:
;;    * Adam Bliss        https://github.com/abliss         <abliss@gmail.com>
;; Contributors:
;;    * N Gvrnd           https://github.com/ngvrnd
;;    * TJamesCorcoran    https://github.com/TJamesCorcoran <jamescorcoran@gmail.com>
;;    * Rastus Vernon     https://github.com/rastus-vernon  <rastus.vernon@protonmail.ch>
;;    * Elliot Glaysher   https://github.com/eglaysher      <erg@google.com>
;;    * David Kerschner   https://github.com/baudtack       <dkerschner@hcoop.net>
;;    * Johnathan Maudlin https://github.com/jcmdln         <jcmdln@gmail.com>
;;
;; URL: https://github.com/urbit/hoon-mode.el
;; Version: 0.1
;; Keywords: extensions, hoon, nock, urbit, Mars

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is my first Major Mode, so don't expect much. It's heavily based on
;; SampleMode from the emacs wiki.

;;; Code:

(require 'cl-lib)

(defvar hoon-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Basic quoting support
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; Hoon comments. Also mark ':' as a normal punctuation character.
    (modify-syntax-entry ?: ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Add dash to the symbol class since it can be part of identifier.
    (modify-syntax-entry ?- "_" st)

    ;; Put all other characters which can be part of runes in the punctuation
    ;; class so that forward and backward work properly.
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry '(?\# . ?\&) "." st)
    (modify-syntax-entry '(?* . ?\,) "." st)
    (modify-syntax-entry '(?. . ?/) "." st)
    (modify-syntax-entry '(?\; . ?@) "." st)
    (modify-syntax-entry '(?^ . ?_) "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?~ "." st)
    st)
  "Syntax table for `hoon-mode'.")

(eval-and-compile
  (defconst hoon-rx-constituents
    `((gap . ,(rx (and space (one-or-more space))))
      (identifier . ,(rx (and lower (zero-or-more (or lower digit "-")))))
      (mold . ,(rx (or "*"
                       "?"
                       "^"
                       (and "@" (zero-or-more word))
                       (and (opt "$-")
                            "("
                            (one-or-more
                             (or (or alphanumeric "(" ")" "*" "?" "@" "-" ":"
                                     "^" "_")
                                 ;; Spaces must be single.
                                 (and space (or alphanumeric "(" ")" "*" "?"
                                                "@" "-" ":" "^" "_"))))
                            ")")
                       (and lower (one-or-more (or lower digit "-" ":" "^")))
                       "$-"
                       )))
      (wing . ,(rx (one-or-more (or "." lower digit "-" "+" "<" ">"))))
      )
    "Common patterns used in font locking hoon code.")

  (defmacro hoon-rx (&rest regexps)
    "Hoon mode specialized rx macro."
    (let ((rx-constituents (append hoon-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))



(defconst hoon-font-lock-arm-declarations-rx
  (hoon-rx (and (group "+" (or "+" "-" "$" "*")) gap
                (group (or "$" identifier))))
  "Regexp of declarations")


(defconst hoon-font-lock-face-mold-old-rx
  (hoon-rx
   (and (group word (zero-or-more (or word "-")))
        "/"
        (group mold)))
  "Regexp to old style name/mold in declarations.")

(defconst hoon-font-lock-tisfas-rx
  (hoon-rx (and "=/" gap (group wing) (opt "=") (opt (group mold))))
  "Regexp to match =/.")

(defconst hoon-font-lock-bar-mold-rx
  (hoon-rx (group (or "|=" "=|")))
  "Regexp to match |= or =|. Used for syntax highlighting the molds on
lines like |=  [a=@t b=wire].")

(defconst hoon-font-lock-face-mold-rx
  (hoon-rx
   (and (group word (zero-or-more (or word "-")))
        "="
        (group mold)))
  "Regexp to match name=mold in declarations")

(defconst hoon-font-lock-kethep-rx
  (hoon-rx (and "^-  "
                (opt "{")
                (group (or mold) (zero-or-more space (or mold)))
                (opt "}")))
  "Regexp to match ^- in long form. Note the `or' around
  `mold'. We need to wrap the imported stuff in that context.")

(defconst hoon-font-lock-kethep-irregular-rx
  (hoon-rx (and "`" (group mold) "`")))

(defconst hoon-font-lock-kettis-rx
  (hoon-rx (and "^=" gap (group identifier))))

(defconst hoon-font-lock-kettis-irregular-rx
  (hoon-rx (and (group identifier) "="))
  "Regexp of faces.")

(defconst hoon-font-lock-mold-shorthand-rx
  (hoon-rx (and (or "[" "(" line-start space)
                (group (and (and "=" identifier)
                            (zero-or-more (or "." ":" identifier))))))
  "Regexp to match =same-name-as-mold in declarations")

(defconst hoon-font-lock-tis-wing-rx
  (hoon-rx (and (or "=." "=?" "=*") gap (group wing)))
  "Several runes start with <rune> <gap> term/wing. Combine these into one
regexp. Because of =/, this rule must run after the normal mold rule.")

(defconst hoon-font-lock-tisket-rx
  (hoon-rx (and "=^" gap (group-n 1 wing) (opt "=") (opt (group-n 3 mold)) gap (group-n 2 wing))))

(defconst hoon-font-lock-symbols-rx
  (rx (and "%" (or (and word (zero-or-more (any word "-")))
                   "|" "&" "$" ".n" ".y")))
  "Regexp of symbols. This must be run before runes, or %.n and %.y will
 partially be highlighted as runes.")

(defconst hoon-font-lock-runes-rx
  ;; This could be `regexp-opt' and added statically for more speed
  (rx (or
       "$@" "$_" "$:" "$%" "$-" "$^" "$?" "$=" "$|" "$," "$&" "$+"
       "|_" "|:" "|%" "|." "|^" "|-" "|~" "|*" "|=" "|?" "|$"
       ":_" ":^" ":-" ":+" ":~" ":*"
       "%_" "%." "%-" "%*" "%^" "%+" "%~" "%="
       ".^" ".+" ".*" ".=" ".?"
       "^|" "^." "^+" "^-" "^&" "^~" "^=" "^?"
       "~|" "~_" "~%" "~/" "~<" "~>" "~$" "~+" "~&" "~=" "~?" "~!"
       ";:" ";/" ";~" ";;"
       "=|" "=:" "=/" "=;" "=." "=?" "=<" "=-" "=>" "=^" "=+" "=~" "=*" "=,"
       "?|" "?-" "?:" "?." "?^" "?<" "?>" "?+" "?&" "?@" "?~" "?=" "?!"
       "!," "!>" "!;" "!=" "!?" "!^" "!:" "!<"
       "+|"
       ;; Not technically runes, but we highlight them like that.
       "=="
       "--"
       ))
  "Regexp of runes.")

(defconst hoon-font-lock-preprocessor-rx
  (rx (or "/?" "/-" "/+" "//" "/="))
  "Ford preprocessor 'runes'.")

(defconst hoon-font-lock-zapzap-rx
  (rx "!!")
  "Highlight the crash rune in red.")

(defconst hoon-font-lock-numbers-rx
  ;; Numbers are in decimal, binary, hex, base32, or base64, and they must
  ;; contain dots (optionally followed by whitespace), as in the German manner.
  (rx (or
       (and "0w"
            (repeat 1 5 (in "-~0-9a-zA-Z"))
            (zero-or-more "." (repeat 5 (in "-~0-9a-zA-Z"))))
       (and "0v"
            (repeat 1 5 (in "0-9a-v"))
            (zero-or-more "." (repeat 5 (in "0-9a-v"))))
       (and "0b"
            (repeat 1 4 (in "0-1"))
            (zero-or-more "." (repeat 4 (in "0-1"))))
       (and "0x"
            (repeat 1 4 hex)
            (zero-or-more "." (repeat 4 hex)))
       (and (repeat 1 3 digit)
            (zero-or-more "." (repeat 3 digit)))
       ))
  "Regexp of numbers")

(defconst hoon-font-lock-todos-rx
  (rx (or "XX" "XXX" "TODO" "FIXME"))
  "Regexp of todo notes.")

;; This is a start, but we still occasionally miss some complex mold declarations.
(defvar hoon-font-lock-keywords
  `(
    (,hoon-font-lock-arm-declarations-rx ;; "++  arm"
     (1 font-lock-constant-face)
     (2 font-lock-function-name-face))
    (,hoon-font-lock-face-mold-old-rx    ;; name/mold
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face))
    (,hoon-font-lock-bar-mold-rx         ;; (=| |=)  name=mold
     (1 font-lock-constant-face)
     (,hoon-font-lock-face-mold-rx
      nil
      nil
      (1 font-lock-variable-name-face)
      (2 font-lock-type-face)))
    (,hoon-font-lock-mold-shorthand-rx   ;; =same-name-as-mold
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-kethep-rx           ;; ^-  mold
     (1 font-lock-type-face))
    (,hoon-font-lock-kethep-irregular-rx ;; `mold`
     (1 font-lock-type-face))
    (,hoon-font-lock-kettis-rx           ;; ^=  face
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-kettis-irregular-rx ;; face=
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-tisfas-rx           ;; =/  wing=@t
     (1 font-lock-variable-name-face
     (2 font-lock-type-face nil t)))
    (,hoon-font-lock-tis-wing-rx         ;; (=. =?)  wing
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-tisket-rx           ;; =^  wing=@t  wing
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-type-face nil t))

    (,hoon-font-lock-symbols-rx . font-lock-keyword-face)

    ;; Highlights all other runes in other contexts.
    (,hoon-font-lock-runes-rx . font-lock-constant-face)
    (,hoon-font-lock-preprocessor-rx . font-lock-preprocessor-face)
    (,hoon-font-lock-zapzap-rx . font-lock-warning-face)

    ;; Highlight any auras in any other contexts. This must happen after all
    ;; the above because it would otherwise stop the previous rules' execution.
    ;; TODO: This rule causes false positives, highlighting ^ in contexts where
    ;; it's used to reach up one namespace instead of being a mold.
    ("\\(@\\w*\\)\\|\\^" . font-lock-type-face)

    ;; These highlights don't have any issues.
    (,hoon-font-lock-numbers-rx . font-lock-constant-face)
    (,hoon-font-lock-todos-rx . font-lock-warning-face))
  "Keyword highlighting specification for `hoon-mode'.")

(defvar hoon-imenu-generic-expression ".*")

(defvar hoon-outline-regexp ":::")

;;;###autoload
(define-derived-mode hoon-mode prog-mode "Hoon"
  "A major mode for editing Hoon files."
  :syntax-table hoon-mode-syntax-table
  (set (make-local-variable 'comment-start) "::")
  (set (make-local-variable 'comment-padding) 2)
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 56)   ;; zero based columns
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(::+\\)\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(hoon-font-lock-keywords))
  (set (make-local-variable 'indent-tabs-mode) nil) ;; tabs zutiefst verboten
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (set (make-local-variable 'fill-paragraph-function) 'hoon-fill-paragraph)
  (set (make-local-variable 'imenu-generic-expression)
       hoon-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) hoon-outline-regexp)

  ;; Hoon files shouldn't have empty lines, but emacs expects them for
  ;; navigation. Treat lines which are just `comment-start' at any margin as
  ;; blank lines for paragraph navigation purposes.
  (set (make-local-variable 'paragraph-start) "\\([ \t]*\:\:\\)*[ \t\f]*$")

  ;; Hoon files often have the same file name in different
  ;; directories. Previously, this was manually handled by hoon-mode instead of
  ;; just setting the right variables and letting Emacs handle it.
  (set (make-local-variable 'uniquify-buffer-name-style) 'forward)
  (set (make-local-variable 'uniquify-strip-common-suffix) nil))

(defun hoon-fill-paragraph (&optional justify)
  "Only fill inside comments. (It might be neat to auto-convert short to long
form syntax, but that would take parsing.)"
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; Never return nil; `fill-paragraph' will perform its default behavior
      ;; if we do.
      t))

;;; Indentation

(defun hoon-indent-line ()
  "Indent current line of Hoon code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (hoon-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun hoon-calculate-indentation ()
  "Return the column to which the current line should be indented."
  0) ;;TODO

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hoon$" . hoon-mode))

(defgroup hoon nil
  "hoon mode for emacs"
  :prefix "hoon-"
  :group 'tools)

(defcustom hoon-herb-path "/usr/bin/herb"
  "Path to herb"
  :group 'hoon
  :type 'string)

(defcustom hoon-herb-args "-d"
  "args for herb"
  :group 'hoon
  :type 'string)

(defcustom hoon-lsp-enable nil
  "Enable hoon-language-server support. NOTE: requires lsp-mode and hoon-language-server to be installed"
  :group 'hoon
  :type 'boolean)

(defcustom hoon-lsp-port "8080"
  "Port for language server"
  :group 'hoon
  :type 'string)
(defcustom hoon-lsp-code "miswyt-palnet-palnet-palnet"
  "+code for planet running language-server"
  :group 'hoon
  :type 'string)
(defcustom hoon-lsp-planet "sampel-palnet"
  "Planet name running language-server"
  :group 'hoon
  :type 'string)

(defcustom hoon-lsp-delay "0"
  "Delay for language server"
  :group 'hoon
  :type 'string)

(eval-after-load "lsp-mode"
  (if hoon-lsp-enable
    '(progn
      (add-to-list 'lsp-language-id-configuration '(hoon-mode . "hoon"))
      (lsp-register-client
        (make-lsp-client :new-connection
                        (lsp-stdio-connection `("hoon-language-server"
                                                ,(concat "-p=" hoon-lsp-port)
                                                ,(concat "-s=" hoon-lsp-planet)
                                                ,(concat "-c=" hoon-lsp-code)
                                                ,(concat "-d=" hoon-lsp-delay)))
                         :major-modes '(hoon-mode)
                         :server-id 'hoon-ls))
      (add-hook 'hoon-mode-hook #'lsp))
  '()))

(defun hoon-eval-region-in-herb ()
  (interactive)
  (shell-command
   (concat hoon-herb-path " " hoon-herb-args " "
	   (shell-quote-argument (buffer-substring (region-beginning) (region-end)))
	   " &")))

(defun hoon-eval-buffer-in-herb ()
  (interactive)
  (shell-command
   (concat hoon-herb-path " " hoon-herb-args " "
	   (shell-quote-argument (buffer-substring-no-properties (point-min) (point-max)))
	   " &")))

(provide 'hoon-mode)
;;; hoon-mode.el ends here



















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
   '("f028e1985041fd072fa9063221ee9c9368a570d26bd6660edbd00052d112e8bb" "969a67341a68becdccc9101dc87f5071b2767b75c0b199e0ded35bd8359ecd69" "511a437aad4bcf848317753f26f35b5a7cd416667122c00e3d8e62a8944bb2c7" "147fcba1e6277e4b9a3d07ba90d822dabc0510d6576514967a55afd71393000d" "8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62" "f99318b4b4d8267a3ee447539ba18380ad788c22d0173fc0986a9b71fd866100" "30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" default))
 '(global-company-mode t)
 '(global-emojify-mode t)
 '(package-selected-packages
   '(writeroom-mode jedi helm-ag elpy python-black emojify solidity-mode humanoid-themes shackle exec-path-from-shell company-racer direnv edit-server xwwp flymake-cursor flymake-diagnostic-at-point dap-go posframe uml-mode night-owl-theme gruvbox-theme abyss-theme clues-theme gotham-theme cargo helm-swoop protobuf-mde yasnippet helm-cider dap-mode helm-lsp helm-imenu lsp-mode moe-theme tron-theme company-fuzzy company-go company flycheck-golangci-lint emmet-mode go-mode yaml-mode markdown-mode js2-mode dockerfile-mode autopair git-timemachine sublimity multiple-cursors powerline smart-tab beacon flycheck helm-projectile helm cider ace-window avy paredit goto-last-change smex expand-region use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

(load-theme 'gruvbox-dark-hard)
