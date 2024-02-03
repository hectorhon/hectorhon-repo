(setenv "PATH" (concat "c:/Program Files/Git/usr/bin;"
                       (getenv "PATH")))
(setq exec-path (cons "C:/Program Files/Git/usr/bin" exec-path))

(add-to-list 'auto-mode-alist '("\\.js[mx]?\\'" . js-ts-mode))

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

(use-package dired
  :config
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :hook (prog-mode . corfu-mode))

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
  (setq js--declaration-keyword-re "")
  (let ((js-rules (alist-get 'javascript js--treesit-indent-rules)))
    (setf (alist-get '(node-is #1="switch_\\(?:case\\|default\\)")
                     js-rules nil nil 'equal)
          '(parent-bol 2))))

(use-package flymake
  :bind
  ("M-p" . flymake-goto-prev-error)
  ("M-n" . flymake-goto-next-error))

(use-package flymake-eslint
  :hook (eglot-managed-mode . (lambda ()
                                (when (derived-mode-p 'js-ts-mode)
                                  (flymake-eslint-enable)))))

(defun browse-current-clojure-ns ()
  (interactive)
  (let ((namespace (cider-current-ns)))
    (with-current-buffer
        (cider-popup-buffer cider-browse-ns-buffer 'select nil 'ancillary)
      (cider-browse-ns--list
       (current-buffer)
       namespace
       (cider-browse-ns--combined-vars-with-meta namespace)
       namespace))))

(use-package cider
  :bind
  ("C-h n" . browse-current-clojure-ns))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(cider-connection-message-fn 'cider-random-tip)
 '(cider-repl-display-help-banner nil)
 '(cider-save-file-on-load t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
 '(make-backup-files nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(flymake-eslint clojure-mode corfu magit modus-themes orderless cider clojure-ts-mode vertico))
 '(project-vc-extra-root-markers '("project.clj" "package.json" "Cargo.toml"))
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Sans Mono" :foundry "outline" :slant normal :weight regular :height 113 :width normal)))))
