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

(defun clojure-find-test-file (src-file-path)
  (let ((other-file-directory
         (replace-regexp-in-string "/src/"
                                   "/test/"
                                   (file-name-directory src-file-path)))
        (other-file-name-base
         (concat (file-name-base src-file-path) "_test"))
        (other-file-extension
         ".clj"))
    (list (concat other-file-directory
                  other-file-name-base
                  other-file-extension))))

(defvar clj-other-file-alist
  (list (list "\\.clj\\'" 'clojure-find-test-file)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package savehist
  :init (savehist-mode))

(use-package marginalia
  :config (marginalia-mode))

(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :init (vertico-mode)
  :config (setq completion-in-region-function #'consult-completion-in-region))

(use-package corfu
  :init (global-corfu-mode))

(use-package orderless
  :init (setq completion-styles '(orderless basic)
	      completion-category-defaults nil
	      completion-category-overrides nil
              completion-ignore-case t))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (define-key hs-minor-mode-map (kbd "C-c <left>") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c <right>") 'hs-show-block))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "<f8>") 'symbol-overlay-remove-all))

(use-package bm
  :config
  (global-set-key (kbd "<f5>") 'bm-toggle)
  (global-set-key (kbd "S-<f5>") 'bm-show-all)
  (global-set-key (kbd "<f6>") 'bm-previous)
  (global-set-key (kbd "<f7>") 'bm-next))

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
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js-ts-mode-map (kbd "M-.") nil)
  (setq js--declaration-keyword-re ""))

(use-package eglot
  :bind
  ("C-." . eglot-code-actions)
  ("C-c C-r" . eglot-rename))

(use-package flymake
  :bind
  ("M-p" . flymake-goto-prev-error)
  ("M-n" . flymake-goto-next-error))

;; (use-package flymake-kondor
;;   :hook (clojure-mode . flymake-kondor-setup)
;;   :hook (clojure-mode . flymake-mode))

;; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))

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
 '(cider-test-fail-fast nil)
 '(cider-use-tooltips nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(ef-light))
 '(custom-safe-themes t)
 '(dired-free-space nil)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-ignored-server-capabilities
   '(:codeLensProvider :documentOnTypeFormattingProvider :documentLinkProvider :colorProvider :inlayHintProvider))
 '(eldoc-echo-area-use-multiline-p 1)
 '(ff-other-file-alist '(("\\.clj\\'" clojure-find-test-file)))
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(make-backup-files nil)
 '(package-selected-packages
   '(bm cider clojure-mode consult corfu ef-themes embark embark-consult flymake leuven-theme magit marginalia modus-themes orderless solarized-theme spacemacs-theme symbol-overlay vertico))
 '(project-vc-extra-root-markers '("project.clj" "package.json" "Cargo.toml"))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight regular :height 108 :width normal))))
 '(cider-fringe-good-face ((t (:foreground "black"))))
 '(cider-test-error-face ((t (:background "orange red" :foreground "white"))))
 '(cider-test-failure-face ((t (:background "red" :foreground "white"))))
 '(symbol-overlay-face-1 ((t (:background "dodger blue" :foreground "white"))))
 '(symbol-overlay-face-2 ((t (:background "hot pink" :foreground "white"))))
 '(symbol-overlay-face-3 ((t (:background "sea green" :foreground "white")))))
(put 'downcase-region 'disabled nil)
