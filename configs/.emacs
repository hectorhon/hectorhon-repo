(setenv "PATH" (concat "c:/Program Files/Git/usr/bin;"
                       (getenv "PATH")))
(setq exec-path (cons "C:/Program Files/Git/usr/bin" exec-path))

(windmove-default-keybindings)
(global-set-key [M-down] 'scroll-up-line)
(global-set-key [M-up] 'scroll-down-line)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

(defun open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))
(global-set-key (kbd "C-o") 'open-next-line)

(defun open-previous-line (arg)
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(global-set-key (kbd "M-o") 'open-previous-line)

(defun format-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace))
(global-set-key (kbd "C-c f") (quote format-buffer))

(defun copy-buffer-file-name ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message filename))))
(global-set-key (kbd "C-c z") (quote copy-buffer-file-name))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (use-package highlight-thing
;;   :init (global-highlight-thing-mode))

(use-package savehist
  :init (savehist-mode))

(use-package vertico
  :init (vertico-mode)
  :config (setq completion-in-region-function #'consult-completion-in-region))

(use-package orderless
  :init (setq completion-styles '(orderless basic)
	      completion-category-defaults nil
	      completion-category-overrides nil
              completion-ignore-case t))

(use-package corfu
  :init (global-corfu-mode))

(use-package project
  :config
  (advice-add
   #'project-try-vc
   :around
   (lambda (orig-fun &rest args)
     (let ((res (apply orig-fun args)))
       (when res
	 (setf (nth 1 res) 'Git)
	 res)))))

(use-package js
  :config
  (define-key js-mode-map (kbd "M-.") nil))

(use-package eglot
  :bind
  ("C-." . eglot-code-actions)
  ("C-c C-r" . eglot-rename))

(use-package flymake
  :bind
  ("M-p" . flymake-goto-prev-error)
  ("M-n" . flymake-goto-next-error))

(use-package flymake-kondor
  :hook (clojure-mode . flymake-kondor-setup)
  :hook (clojure-mode . flymake-mode))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(cider-connection-message-fn nil)
 '(cider-eldoc-display-for-symbol-at-point nil)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-prompt-function 'cider-repl-prompt-lastname)
 '(cider-save-file-on-load t)
 '(cider-use-tooltips nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(corfu-auto t)
 '(create-lockfiles nil)
 '(default-frame-alist '((width . 90) (height . 40)))
 '(dired-free-space nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-ignored-server-capabilities
   '(:codeLensProvider :documentOnTypeFormattingProvider :documentLinkProvider :colorProvider :inlayHintProvider))
 '(eldoc-echo-area-use-multiline-p 1)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(make-backup-files nil)
 '(package-selected-packages
   '(highlight zones magit highlight-thing flymake-kondor cider clojure-mode corfu leuven-theme doom-themes vertico modus-themes rust-mode consult orderless))
 '(project-vc-extra-root-markers '("project.clj" "package.json" "Cargo.toml"))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(typescript-ts-mode-indent-offset 4))
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :foundry "outline" :slant normal :weight regular :height 102 :width normal))))
 '(cider-fringe-good-face ((t (:foreground "black"))))
 '(cider-test-failure-face ((t (:background "orange red" :foreground "white")))))
(put 'downcase-region 'disabled nil)
