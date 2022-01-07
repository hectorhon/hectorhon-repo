(setq read-process-output-max (* 1024 1024))

(windmove-default-keybindings)
(global-set-key [M-down] (quote scroll-up-line))
(global-set-key [M-up] (quote scroll-down-line))
(global-set-key [M-right] (lambda ()
                            (interactive)
                            (set-window-hscroll (selected-window)
                                                (1+ (window-hscroll)))))
(global-set-key [M-left] (lambda ()
                           (interactive)
                           (set-window-hscroll (selected-window)
                                               (1- (window-hscroll)))))

(defun format-buffer ()
  "Indent the entire buffer and deletes all trailing whitespace."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace))
(global-set-key (kbd "C-c f") (quote format-buffer))

(defadvice yank (after indent-region activate)
  (if (derived-mode-p 'prog-mode)
      (indent-region (region-beginning) (region-end) nil)))

(defun copy-buffer-file-name ()
  "Copy the current 'buffer-file-name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message filename))))
(global-set-key (kbd "C-c z") (quote copy-buffer-file-name))

(defun show-popup-yank-menu ()
  (interactive)
  (popup-menu 'yank-menu))
(global-set-key (kbd "C-c y") (quote show-popup-yank-menu))

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package solarized-theme
  :ensure t
  ;; :config (load-theme 'solarized-light t)
  )

(use-package doom-themes
  :ensure t
  ;; :config (load-theme 'doom-one-light t)
  )

(use-package leuven-theme
  :ensure t
  ;; :config (load-theme 'leuven t)
  )

(use-package modus-themes
  :ensure t
  :init (modus-themes-load-themes)
  :config (modus-themes-load-operandi))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-mode-map
              ("C-c TAB" . company-complete)))

(use-package magit
  :ensure t)

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package rust-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package eglot
  :ensure t
  :config (progn
            ;; Remove eglot from mode line
            (setq mode-line-misc-info (cdr mode-line-misc-info))

            ;; Configure lsp servers
            (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))
  :hook ((rust-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-." . eglot-code-actions)
              ("C-c r" . eglot-rename)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(desktop-save-mode nil)
 '(display-time-mode t)
 '(eglot-confirm-server-initiated-edits nil)
 '(fido-mode t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(mode-line-format
   '("%e" " " mode-line-misc-info mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-end-spaces))
 '(package-selected-packages
   '(eglot modus-themes leuven-theme solarized-theme doom-themes undo-tree company magit projectile typescript-mode rust-mode use-package))
 '(projectile-globally-ignored-directories
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd" "target" "node_modules"))
 '(projectile-project-root-functions
   '(projectile-root-local projectile-root-top-down-recurring projectile-root-top-down projectile-root-bottom-up))
 '(ring-bell-function 'ignore)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
