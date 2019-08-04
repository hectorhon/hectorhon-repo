(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (dichromacy)))
 '(dired-listing-switches "-al --sort=extension")
 '(display-battery-mode t)
 '(display-time-mode t)
 '(eshell-visual-subcommands (quote (("npm" "install" "run"))))
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode nil)
 '(global-mark-ring-max 1)
 '(grep-command "grep --color -nHr --null --exclude-dir=node_modules -e ")
 '(helm-grep-default-command "xxx")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(magit-save-repository-buffers nil)
 '(make-backup-files nil)
 '(mark-ring-max 1)
 '(package-selected-packages
   (quote
    (magit helm-projectile projectile helm web-mode slime)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(slime-load-failed-fasl (quote never))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t))



;; windmove mode
(windmove-default-keybindings)



;; format code
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)))
(global-set-key (kbd "C-S-F") 'indent-buffer)



;; copy current file name
(defun copy-filename ()
  (interactive)
  (let ((filename (buffer-file-name (window-buffer (minibuffer-selected-window)))))
    (message filename)
    (kill-new filename)))
(global-set-key (kbd "C-c z") 'copy-filename)



;; scroll window line by line
(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)



;; jump to scratch buffer
(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "<f1>") 'switch-to-scratch-buffer)



;; jump to shell buffer
(defun switch-to-eshell-buffer ()
  (interactive)
  (switch-to-buffer "*eshell*"))
(global-set-key (kbd "<f2>") 'switch-to-eshell-buffer)



;; Shortcut key for hs-minor-mode toggle hiding
(global-set-key (kbd "C-`") 'hs-toggle-hiding)



;; browse kill ring
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))



;; set up melpa repository
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)



;; set up magit
(global-set-key (kbd "C-x g") 'magit-status)



;; set up projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)



;; set up helm
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "M-s o") 'helm-occur)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)



;; set up slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))



;; set up web mode
(require 'web-mode)
(defun my-web-mode-hook ()
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
