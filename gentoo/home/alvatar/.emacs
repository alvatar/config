;; el-get

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;(setq user-emacs-directory "~/.emacs.d")

(unless (require 'el-get nil t)
(url-retrieve
 "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
 (lambda (s)
   (end-of-buffer)
   (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; set local recipes, el-get-sources should only accept PLIST element
(setq
 el-get-sources
 '((:name buffer-move                   ; have to add your own keys
          :after (progn
                   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
                   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
                   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
                   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
   (:name smex                          ; a better (ido like) M-x
          :after (progn
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   (:name magit                        ; git meet emacs, and a binding
          :after (progn
                   (global-set-key (kbd "C-x C-z") 'magit-status)
                   (setq with-editor-file-name-history-exclude 1)))
   ;; (:name switch-window                 ; takes over C-x o
   ;;        :after (progn
   ;;                 (global-set-key (kbd "C-x o") 'switch-window)))
   (:name goto-last-change          ; move pointer back to last change
          :after (progn
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name paredit
          :after (progn
                   (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
                   (add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
                   (add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
                   (add-hook 'scheme-interaction-mode-hook (lambda () (paredit-mode +1)))))
   (:name linum-ex
          :after (global-linum-mode 1))
   ;; search capable of interactive string replace
   (:name phi-search
          :after (setq phi-search-case-sensitive  t))
   ;; fast navigation
   (:name avy
          :after (progn
                   (global-set-key (kbd "M-p") 'avy-goto-char-timer)
                   ;;(global-set-key (kbd "C-:") 'avy-goto-char)
                   ;;(global-set-key (kbd "C-\"") 'avy-goto-char-2)
                   ))
   (:name ace-window
          :after (global-set-key (kbd "C-x o") 'ace-window))
   (:name js2-mode
          :after (setq js-indent-level 2))
   ;; (:name go-mode
   ;;        :after (progn
   ;;                 (setq gofmt-command "goimports")
   ;;                 (add-hook 'before-save-hook 'gofmt-before-save)
   ;;                 ;; C-? global binding
   ;;                 (global-set-key (kbd "C-?") 'godef-jump)))
   (:name cider
          :after (progn
                   (setq cider-show-error-buffer nil)
                   (setq cider-show-error-buffer 'only-in-repl)
                   (setq cider-repl-display-help-banner nil)))
   (:name helm
          :after (progn
                   (global-set-key (kbd "C-x M-f") 'helm-find-files)))
   (:name helm-projectile
          :after (progn
                   (helm-projectile-on)
                   (global-set-key (kbd "C-x C-f") 'helm-projectile-find-file)))
   (:name helm-ag
          :after (progn
                   (global-set-key (kbd "C-x C-g") 'helm-do-ag-project-root)
                   (global-set-key (kbd "C-x C-h") 'xref-find-references)))
   ;;(:name helm-gtags
   ;;       :after (progn
   ;;                (global-set-key (kbd "C-x C-t") 'helm-gtags-tags-in-this-function)))
   (:name flycheck
          :after (progn
                   (global-flycheck-mode)
                   (setq-default flycheck-disabled-checkers '(go-golint))))))

(setq
 my:el-get-packages
 (append
  '(
    el-get ; el-get is self-hosting
    ;; Themes
    sublime-themes
    color-theme-solarized
    color-theme-sanityinc
    color-theme-sanityinc-tomorrow
    ;; Misc
    git-timemachine
    flx ; Fuzzy IDO
    projectile ; Project navigation
    flycheck-color-mode-line
    ;;flycheck
    ;; Languages
    dockerfile-mode
    lua-mode
    markdown-mode
    yaml-mode
    solidity-mode
    less-css-mode
    ;; Python
    elpy
    jedi
    ;; Clojure
    helm-cider
    ;; Completion
    auto-complete ; complete as you type with overlays
    ac-cider
    company-mode
    ;;cscope
    ;; Go
    ;; go-flymake
    go-autocomplete ; go get -u github.com/nsf/gocode
    go-errcheck
    go-imports
    go-projectile
    go-rename
    go-lint
    ;; Web
    livedown ; npm install -g livedown
    emmet-mode ; zencoding evolved
    autopair ; automatically close {([
    )

  (mapcar 'el-get-source-name el-get-sources)))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;;-----------------

;; General Config

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)
(setq browse-url-generic-program (executable-find "firefox")
      browse-url-browser-function 'browse-url-generic)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-buffer-menu t)
(add-hook 'find-file-hook (lambda () (setf show-trailing-whitespace t)))
(ac-config-default)

;;; Look & feel
(menu-bar-mode 0)
(if (display-graphic-p)
    ;; GUI
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (color-theme-sanityinc-tomorrow-day)
      ;; Fonts
      (if (eq system-type 'darwin)
          ;; OSX
          (progn
            ;; default Latin font (e.g. Consolas)
            (set-face-attribute 'default nil :family "Courier")
            ;; default font size (point * 10)
            ;; WARNING!  Depending on the default font,
            ;; if the size is not supported very well, the frame will be clipped
            ;; so that the beginning of the buffer may not be visible correctly.
            (set-face-attribute 'default nil :height 110)
            ;; Prevent opening a dialog on OSX (buggy)
            (defadvice yes-or-no-p (around prevent-dialog activate)
              "Prevent yes-or-no-p from activating a dialog"
              (let ((use-dialog-box nil)) ad-do-it))
            (defadvice y-or-n-p (around prevent-dialog-yorn activate)
              "Prevent y-or-n-p from activating a dialog"
              (let ((use-dialog-box nil)) ad-do-it)))
        ;; Linux
        (progn
          (set-default-font "Hack")
          (set-face-attribute 'default nil :height 85))))
  ;; Console
  (progn
    ;; (color-theme-sanityinc-dark)
    (set-face-attribute 'font-lock-comment-face nil :foreground "#cccccc")
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#cccccc")
    (set-face-attribute 'font-lock-string-face nil :foreground "#4f004f" :weight 'normal)
    (set-face-attribute 'font-lock-constant-face nil :foreground "#4f004f")
    (set-face-attribute 'font-lock-keyword-face nil :foreground "#00003f")
    (set-face-attribute 'font-lock-builtin-face nil :foreground "#008080")
    (set-face-attribute 'font-lock-type-face nil :foreground "#eae374")
    (set-face-attribute 'font-lock-function-name-face nil :foreground "#008080" :weight 'bold)
    (set-face-attribute 'font-lock-variable-name-face nil :foreground "#008080" :weight 'bold)))

(column-number-mode 1)
(line-number-mode 1)
(set-default 'truncate-lines t)
(blink-cursor-mode 0)
(global-font-lock-mode 1)
(transient-mark-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)
(setq ns-right-alternate-modifier nil)


;; Shell

(add-hook 'shell-mode-hook (lambda ()
                             (compilation-shell-minor-mode 1)
                             (setq compilation-auto-jump-to-first-error 1)))

;; Clojure (Cider)

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; C

(setq c-default-style "linux"
      c-basic-offset 4)

;; Go

(let ((projects-home "/home/alvatar/"))
  (let ((go-root (concat projects-home "/go/go" (getenv "GOVERSION")))
        (go-path (concat projects-home "/go")))
    (setenv "GOPATH" go-path)
    (setenv "GOROOT" go-root)
    (setenv "PATH" (concat go-path "/bin:"
                           go-root "/bin:"
                           (getenv "PATH")))
    (setq exec-path (cons (concat go-root "/bin") exec-path))
    (setq exec-path (cons (concat go-path "/bin")
                          exec-path))))

(add-hook 'go-mode-hook #'autopair-mode)

;; Scheme

(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
(add-hook 'scheme-mode-hook (lambda () (setq scheme-program-name "/usr/local/Gambit/bin/gsc")))
(font-lock-add-keywords 'scheme-mode
                        '(("(\\(lambda\\)\\>" (0 (prog1 ()
                                                   (compose-region (match-beginning 1)
                                                                    (match-end 1)
                                                                   ?λ))))))
(global-set-key "\C-c\C-qr" 'run-scheme)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\M-c" 'uncomment-region)
(add-hook 'scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map "\C-c\C-i" 'scheme-import-file)))
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (linum-mode 0)))
(add-hook 'inferior-scheme-mode-hook
          (lambda ()
            (define-key scheme-mode-map "\C-c\C-c" 'comment-region)
            (define-key scheme-mode-map "\C-c\M-c" 'uncomment-region)))

;; Load remote SchemeSpheres remote debugging if installed
(let ((sense-emacs "~/Dropbox/projects/sphere-energy/src/remote/sense-emacs.el"
                   ;;"/usr/local/Gambit/spheres/energy/src/remote/sense-emacs.el"
                   ))
  (when (file-exists-p sense-emacs)
    (load-file sense-emacs)
    (message "Emacs Sense loaded")))

;;-----------------

;; Custom functions

(defun rm-trailing-spaces ()
  "Remove spaces at ends of all lines"
  (interactive)
  (save-excursion
    (let ((current (point)))
      (goto-char 0)
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match "" nil nil))
      (goto-char current))))

(put 'downcase-region 'disabled nil)

(defun filter-with-shell-command (command arg)
  "Run a command with the buffer as input and replace it"
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (shell-command-on-region (point-min) (point-max) command t t))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

(global-set-key (kbd "<C-up>") 'toggle-maximize-buffer)

;; Custom Key bindings

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-S-k") 'delete-region)

(global-set-key (kbd "<f8>") 'flycheck-list-errors)
(global-set-key (kbd "<f9>") 'flycheck-next-error)
(global-set-key (kbd "<f10>") 'gofmt)
(global-set-key (kbd "C-2") 'xref-find-references)
(global-set-key (kbd "C-3") 'helm-do-grep-ag)


(provide '.emacs)
;;; .emacs ends here
