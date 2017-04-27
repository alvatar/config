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
                   (global-set-key (kbd "C-'") 'avy-goto-char-timer)
                   (global-set-key (kbd "C-:") 'avy-goto-char)
                   (global-set-key (kbd "C-\"") 'avy-goto-char-2)))
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
                   (setq cider-repl-display-help-banner nil)))))

;; now set our own packages
(setq
 my:el-get-packages
 (append
  '(
    sublime-themes
    auto-complete ; complete as you type with overlays
    ac-cider
    color-theme-solarized
    color-theme-sanityinc
    color-theme-sanityinc-tomorrow
    company-mode
    cscope
    dockerfile-mode
    el-get ; el-get is self-hosting
    emmet-mode ; zencoding evolved
    flx ; Fuzzy IDO
    go-autocomplete ; go get -u github.com/nsf/gocode
    go-errcheck
    go-flymake
    go-imports
    go-projectile
    go-lint
    livedown ; npm install -g livedown
    lua-mode
    markdown-mode
    projectile ; Project navigation
    yaml-mode
    solidity-mode
    less-css-mode
    git-timemachine
    helm
    helm-projectile
    helm-cider
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
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(add-hook 'find-file-hook
          (lambda ()
            (setf show-trailing-whitespace t)))
(if (eq system-type 'darwin)
    (setq browse-url-generic-program "~/.emacs.d/run-firefox.sh"))

;;; Look & feel

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
 '(custom-enabled-themes (list '(sanityinc-tomorrow-day)
                               '(sanityinc-tomorrow-night)
                               '(sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(fci-rule-color "#515151")
 '(frame-background-mode (quote light))
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(org-agenda-files nil)
 '(package-selected-packages (quote (queue)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
              (let ((use-dialog-box nil))
                ad-do-it))
            (defadvice y-or-n-p (around prevent-dialog-yorn activate)
              "Prevent y-or-n-p from activating a dialog"
              (let ((use-dialog-box nil))
                ad-do-it)))
        ;; Linux
        (progn
          (set-default-font "Hack")
          (set-face-attribute 'default nil :height 85))))
  ;; Console
  (progn
    (color-theme-sanityinc-tomorrow-bright)))
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

;; Aspell

(setq ispell-program-name "aspell")
(setq exec-path (cons "/usr/local/bin/" exec-path))

;; Etags

(defun create-tags (dir-name)
  "Create tags file."
  (interactive
   "DDirectory:")
  (shell-command
   (format "cd %s && find . -type f | grep \".*\\.\\(c\\|h\\|cpp\\|hpp\\|scm\\|sld\\|ss\\)$\" | xargs etags"
           (directory-file-name dir-name))))

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

(let ((projects-home (getenv "PROJECTS_HOME")))
  (let ((go-root (concat projects-home "/go/go1.7.4"))
        (go-path (concat projects-home "/go")))
    (setenv "GOPATH" go-path)
    (setenv "GOROOT" go-root)
    (setenv "PATH" (concat go-path "/bin:"
                           go-root "/bin:"
                           (getenv "PATH")))
    (setq exec-path (cons (concat go-root "/bin") exec-path))
    (setq exec-path (cons (concat go-path "/bin")
                          exec-path))))

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
  (message "Emacs Sense loaded")
  (if (file-exists-p sense-emacs)
      (load-file sense-emacs)))

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

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (set-register '_ (list (current-window-configuration)))
           (delete-other-windows))))

(global-set-key (kbd "<C-up>") 'toggle-maximize-buffer)

;; (define-key key-translation-map [?\C-h] [?\C-?]) ; Unmask 'delete' as backspace

;; (let ((translations
;;        '( 229 [?\M-a]  230 [?\M-b]   231 [?\M-c]  8706 [?\M-d]   nil [?\M-e]
;;           402 [?\M-f]  169 [?\M-g]   729 [?\M-h]   nil [?\M-i]  8710 [?\M-j]
;;           730 [?\M-k]  172 [?\M-l]   181 [?\M-m]   nil [?\M-n]   248 [?\M-o]
;;           960 [?\M-p]  339 [?\M-q]   174 [?\M-r]   223 [?\M-s]  8224 [?\M-t]
;;           nil [?\M-u] 8730 [?\M-v]  8721 [?\M-w]  8776 [?\M-x]   165 [?\M-y]
;;           937 [?\M-z]
;;           96 [?\M-~]  161 [?\M-1]   162 [?\M-4]   163 [?\M-3]   167 [?\M-6]
;;           170 [?\M-9]  171 [?\M-\\]  175 [?\M-<]   176 [?\M-*]   177 [?\M-+]
;;           182 [?\M-7]  183 [?\M-\(]  186 [?\M-0]   187 [?\M-|]   191 [?\M-\?]
;;           198 [?\M-\"] 230 [?\M-']   247 [?\M-/]   728 [?\M->]  8211 [?\M-\-]
;;           8212 [?\M-_] 8216 [?\M-\]] 8217 [?\M-}]  8218 [?\M-\)] 8220 [?\M-\[]
;;           8221 [?\M-{] 8225 [?\M-&]  8226 [\?M-8]  8249 [?\M-#]  8250 [?\M-$]
;;           8260 [?\M-!] 8364 [\?M-@]  8482 [?\M-2]  8734 [\?M-5]  8800 [?\M-=]
;;           8804 [?\M-,] 8805 [?\M-.] 64257 [?\M-%] 64258 [?\M-^])))
;;   (while translations
;;     (let ((key (car translations)) (def (cadr translations)))
;;       (if key
;;           (define-key key-translation-map (make-string 1 key) def)))
;;     (setq translations (cddr translations))))
