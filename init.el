(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


(package-initialize)

(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(electric-pair-mode 1)
(menu-bar-mode 0)
(show-paren-mode t)

(server-start)
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(mapc                                 
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   c-mode-common-hook        
   emacs-lisp-mode-hook              
   ))
(mapc
   (lambda (hook)
     (add-hook hook
                      '(lambda () (flyspell-mode 1))))
   '(
     yatex-mode-hook     
                                   
     ))

(require 'cl)

(defvar installing-package-list
  '(
    undo-tree
    igrep
    grep-a-lot
    google-c-style
    afternoon-theme
    git-gutter
    magit
    multiple-cursors
    smartrep
    company
    jedi-core
    company-jedi
    irony
    company-irony
    company-irony-c-headers
    clang-format
    yasnippet
    ))

(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))) )

(require 'afternoon-theme)
(load-theme 'afternoon t)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'igrep)
(igrep-define lgrep (igrep-use-zgrep nil)(igre-regex-option "-n -Ou8"))
(igrep-find-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -Ou8"))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-r")

(smartrep-define-key global-map "C-r"
  '(("C-r"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

  (use-package company
    ;; complete anything
    :ensure t
    :init
    (add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'c-mode-common-hook 'company-mode)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
    :config
    (setq company-async-timeout 1) ;; set timeout to 10 seconds
    (setq company-idle-delay 0.2)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-search-map (kbd "C-n") 'company-select-next)
    (define-key company-search-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)

    (set-face-attribute 'company-tooltip nil
			:foreground "black" :background "lightgrey")
    (set-face-attribute 'company-tooltip-common nil
			:foreground "black" :background "lightgrey")
    (set-face-attribute 'company-tooltip-common-selection nil
			:foreground "white" :background "steelblue")
    (set-face-attribute 'company-tooltip-selection nil
			:foreground "black" :background "steelblue")
    (set-face-attribute 'company-preview-common nil
			:background nil :foreground "lightgrey" :underline t)
    (set-face-attribute 'company-scrollbar-fg nil
			:background "orange")
    (set-face-attribute 'company-scrollbar-bg nil
			:background "gray40")
    )
  
  (use-package jedi-core
     :ensure t
     :init
     (add-hook 'python-mode-hook 'jedi:setup)
     :config
     (setq jedi:complete-on-dot t)
     (setq jedi:use-shortcuts t)
     (eval-after-load 'company '(add-to-list 'company-backends '(company-jedi))) )

  (use-package irony
    :ensure t
    :defer t
    :init
    (add-hook 'c-mode-common-hook 'irony-mode)
    :config
    ;; replace completion-at-point and complete-symbol by irony-mode's asynchronous functions
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
  'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
  'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (setq irony-additional-clang-options '(;; use C++11
             "-std=c++11"))
    (use-package company-irony
      :ensure t
      :init
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
      )
    ;; enable C/C++ header completion
    (use-package company-irony-c-headers
      :ensure t
      :config
      (eval-after-load 'company '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))) )

(load-file "$HOME/.emacs.d/google-c-style.el")
(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c++-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent) )

(use-package clang-format
  :ensure t
  :config
  (require 'clang-format)
  (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer)
  (setq clang-format-style-option "google") )

(eval-after-load "yasnippet"
  '(progn
     (define-key yas-keymap (kbd "<tab>") nil)
     (yas-global-mode 1)))
