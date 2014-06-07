;; el-get
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
             (global-set-key (kbd "C-x C-z") 'magit-status)))
   (:name switch-window          ; takes over C-x o
    :after (progn
             (global-set-key (kbd "C-x o") 'switch-window)))
   (:name goto-last-change          ; move pointer back to last change
          :after (progn
                   ;; when using AZERTY keyboard, consider C-x C-_
                   (global-set-key (kbd "C-x C-/") 'goto-last-change)))
   (:name paredit
    :after (progn
             (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
             (add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
             (add-hook 'scheme-interaction-mode-hook (lambda () (paredit-mode +1)))))
   (:name linum-ex
    :after (progn
             (global-linum-mode 1)))
   (:name color-theme-solarized ; Color theme
    :after (progn
             (when (display-graphic-p)
               (color-theme-solarized 'light))))))

;; now set our own packages
(setq
 my:el-get-packages
 (append
  '(el-get                  ; el-get is self-hosting
    auto-complete           ; complete as you type with overlays
    zencoding-mode          ; http://www.emacswiki.org/emacs/ZenCoding
    color-theme             ; nice looking emacs
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

;;; Look & feel

(custom-set-variables
 '(inhibit-startup-screen t)
 ;;'(initial-buffer-choice "/")
 '(load-home-init-file t t))
(custom-set-faces)
(menu-bar-mode 0)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      ;; Fonts
      (if (eq system-type 'darwin)
          (progn
            ;; default Latin font (e.g. Consolas)
            (set-face-attribute 'default nil :family "Courier")
            ;; default font size (point * 10)
            ;; WARNING!  Depending on the default font,
            ;; if the size is not supported very well, the frame will be clipped
            ;; so that the beginning of the buffer may not be visible correctly. 
            (set-face-attribute 'default nil :height 110))
        (progn
          (set-default-font "-*-montecarlo-medium-*-normal-*-*-*-*-*-*-*-*-*")
          (set-face-attribute 'default nil :height 110)
          (set-face-font 'font-lock-comment-face "-*-montecarlo-medium-*-normal-*-*-*-*-*-*-*-*-*")
          (set-face-foreground 'font-lock-comment-delimiter-face "DimGrey")
          (set-face-font 'font-lock-comment-delimiter-face "-*-montecarlo-medium-*-normal-*-*-*-*-*-*-*-*-*")
          (set-face-foreground 'font-lock-comment-face "DimGrey")))))
(column-number-mode 1)
(line-number-mode 1)
(blink-cursor-mode 0)
(global-font-lock-mode 1)
(transient-mark-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; Scheme

(setq scheme-program-name "gsc")
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

