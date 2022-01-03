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
  :config (load-theme 'solarized-light t))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :bind (:map company-mode-map
              ("C-c TAB" . company-complete)))

(use-package helm
  :ensure t
  :config (helm-mode 1)
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)))

(use-package magit
  :ensure t)

(use-package flycheck
  :ensure t
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package helm-projectile
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :config (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :bind (:map lsp-mode-map
              ("C-." . lsp-execute-code-action)))

(use-package lsp-ui
  :ensure t)

(use-package helm-lsp
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package lsp-treemacs
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-minimum-prefix-length 2)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(desktop-save-mode t)
 '(display-time-mode t)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lsp-lens-enable nil)
 '(lsp-modeline-code-actions-enable nil)
 '(lsp-modeline-diagnostics-enable nil)
 '(lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"])
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-enable nil)
 '(make-backup-files nil)
 '(mode-line-format
   '("%e" " " mode-line-misc-info mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-end-spaces))
 '(package-selected-packages
   '(solarized-theme undo-tree use-package tree-sitter rust-mode magit helm-projectile helm-lsp flycheck company))
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
 '(default ((t (:family "Roboto Mono" :foundry "outline" :slant normal :weight normal :height 102 :width normal)))))
