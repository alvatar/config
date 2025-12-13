;;------------------------------------------------------------------------------
;; Package Management
;;------------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Suppress cl deprecation warnings from old packages
(setq byte-compile-warnings '(cl-functions))

;; Refresh package contents on first run if needed
(when (not package-archive-contents)
  (package-refresh-contents))

;; Move custom-set-variables to separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Manually loaded packages
(add-to-list 'load-path "~/.emacs.d/external/")

;;------------------------------------------------------------------------------
;; Core Packages
;;------------------------------------------------------------------------------

(use-package diminish)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Which-key - shows available keybindings
(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
        which-key-prefix-prefix "◉ "
        which-key-sort-order 'which-key-key-order-alpha))

;; Helpful - better help buffers
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;;------------------------------------------------------------------------------
;; Completion Framework - Ivy/Counsel/Swiper
;;------------------------------------------------------------------------------

(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-c C-r" . counsel-recentf)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate))
  :config
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\(?:\\(?:\\.\\(?:aux\\|bbl\\|blg\\|fdb_latexmk\\|fls\\|log\\|synctex\\.gz\\|toc\\)\\)\\)\\'")))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

;;------------------------------------------------------------------------------
;; Navigation & Editing
;;------------------------------------------------------------------------------

(use-package magit
  :bind ("C-x C-z" . magit-status))

(use-package git-timemachine)

(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package avy
  :bind ("M-p" . avy-goto-char-timer))

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package goto-last-point
  :ensure nil  ;; Loaded from external
  :if (locate-library "goto-last-point")
  :bind (("C-x w" . goto-last-point-record)
         ("C-x e" . goto-last-point)))

(use-package multiple-cursors
  :bind ("C-S-<mouse-1>" . mc/add-cursor-on-click))

(use-package bm
  :demand t
  :config
  (setq bm-cycle-all-buffers t
        temporary-bookmark-p t)
  :bind (("C-c C-h C-h" . bm-next)
         ("C-c C-j C-j" . bm-previous)
         ("C-c C-g C-g" . bm-toggle)))

;;------------------------------------------------------------------------------
;; Project Management
;;------------------------------------------------------------------------------

(use-package projectile
  :diminish
  :init (setq projectile-indexing-method 'hybrid)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode +1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package treemacs
  :defer t
  :bind ("C-x t" . treemacs))

(use-package treemacs-projectile
  :after (treemacs projectile))

;;------------------------------------------------------------------------------
;; Window Management
;;------------------------------------------------------------------------------

(use-package window-purpose
  :config (purpose-mode))

;; Custom window toggle
(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

;;------------------------------------------------------------------------------
;; UI & Appearance
;;------------------------------------------------------------------------------

;; Theme
(use-package gruvbox-theme)
(load-theme 'gruvbox-dark-hard t)

;; Modern modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))

;; Beacon - highlight cursor on jump
(use-package beacon
  :config (beacon-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :config (global-hl-line-mode 1))

;; Font configuration
(if (display-graphic-p)
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (if (eq system-type 'darwin)
          ;; macOS
          (progn
            (set-face-attribute 'default nil :font "Fira Code" :height 140))
        ;; Linux
        (progn
          (set-face-attribute 'default nil :font "JetBrains Mono" :height 100)))))

;; UI Settings
(global-display-line-numbers-mode)
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

;;------------------------------------------------------------------------------
;; General Settings
;;------------------------------------------------------------------------------

;; No backups
(setq backup-inhibited t
      auto-save-default nil
      make-backup-files nil)

;; Silent bell
(setq ring-bell-function 'ignore)

;; Indentation
(setq-default indent-tabs-mode nil)

;; Startup
(setq inhibit-startup-buffer-menu t
      inhibit-splash-screen t)

;; UTF-8
(set-language-environment "UTF-8")

;; Show trailing whitespace
(add-hook 'find-file-hook (lambda () (setf show-trailing-whitespace t)))

;; Confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; Misc
(transient-mark-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;;------------------------------------------------------------------------------
;; AI Tools
;;------------------------------------------------------------------------------

;; Copilot
(use-package copilot
  :ensure nil  ;; Loaded from external directory
  :if (locate-library "copilot")
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "S-<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "S-TAB") 'copilot-accept-completion)
  :bind (:map company-mode-map
              ("C-<tab>" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion)))

;; Aider (commented - configure as needed)
;; (use-package aider
;;   :ensure t
;;   :config
;;   ;; Configure with your preferred model and API key via environment
;;   ;; export ANTHROPIC_API_KEY=your-key
;;   ;; export GEMINI_API_KEY=your-key
;;   (setq aider-args '("--model" "sonnet"))
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;------------------------------------------------------------------------------
;; Development Tools
;;------------------------------------------------------------------------------

;; Tree-sitter grammars
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
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Company - code completion
(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t))

;; Flycheck - syntax checking
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; Yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; LSP Mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rustic-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; General LSP settings
  (lsp-keymap-prefix "C-c l")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-enable-snippet t)
  (lsp-prefer-flymake nil)
  ;; Rust-specific settings
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (("C-c 1" . lsp-ui-peek-find-definitions)
         ("C-c 2" . lsp-ui-peek-find-references)
         ("C-c 6" . lsp-ui-imenu))
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-delay 1)
  (lsp-ui-doc-enable t))

;; DAP Mode (Debugger)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

;; Exec path from shell (macOS/GUI only)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;;------------------------------------------------------------------------------
;; Language Modes
;;------------------------------------------------------------------------------

;; Python
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python3"))

(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode))

;; Rust
(use-package rustic
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance)
              ("C-c 9" . flycheck-next-error)
              ("C-c 0" . next-error)
              :map rustic-compilation-mode-map
              ("q" . kill-this-buffer))
  :config
  (setq rustic-indent-method-chain nil
        rustic-lsp-server 'rust-analyzer)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (when buffer-file-name
                (setq-local buffer-save-without-query t))
              (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode))

;; C
(setq c-default-style "linux"
      c-basic-offset 4)

;; HTML
(use-package emmet-mode
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)))

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; TOML
(use-package toml-mode
  :mode "\\.toml\\'")

;; Dockerfile
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; Solidity
(use-package solidity-mode
  :mode "\\.sol\\'")

;; WGSL
(use-package wgsl-mode
  :mode "\\.wgsl\\'")

;; Paredit for Lisp/Scheme
(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (clojure-mode . paredit-mode)))

;; Scheme
;; (add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
;; (setq scheme-program-name "gsi")  ;; Adjust for your Gambit installation
;; (font-lock-add-keywords 'scheme-mode
;;   '(("(\\(lambda\\)\\>" (0 (prog1 ()
;;                             (compose-region (match-beginning 1)
;;                                           (match-end 1) ?λ))))))

;;------------------------------------------------------------------------------
;; Custom Functions
;;------------------------------------------------------------------------------

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
  "Run COMMAND with the buffer as input and replace it."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (shell-command-on-region (point-min) (point-max) command t t))

;;------------------------------------------------------------------------------
;; Global Keybindings
;;------------------------------------------------------------------------------

(global-unset-key (kbd "C-x C-c"))  ;; Prevent accidental quit

(global-set-key (kbd "<C-up>") 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-x C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x K") (lambda () (interactive) (kill-this-buffer) (delete-window)))

;;------------------------------------------------------------------------------
;; Installation Notes
;;------------------------------------------------------------------------------

;; Install language servers and tools:
;;
;; Python: pip install python-lsp-server black
;; Rust:   rustup component add rust-analyzer
;;
;; Tree-sitter grammars: M-x treesit-install-language-grammar

;;; .emacs ends here
