;;; package --- Summary

;;; Commentary:

;;; Code:

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
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" default)))
 '(dired-listing-switches "-al --sort=extension")
 '(eshell-visual-subcommands (quote (("npm" "install" "run"))))
 '(fci-rule-color "#383838")
 '(flycheck-display-errors-delay 0.1)
 '(flycheck-global-modes (quote (not emacs-lisp-mode)))
 '(flycheck-idle-change-delay 0.1)
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode nil)
 '(global-mark-ring-max 1)
 '(grep-command "grep --color -nHr --null --exclude-dir=node_modules -e ")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-enabled-frameworks nil)
 '(js-indent-level 2)
 '(magit-save-repository-buffers nil)
 '(make-backup-files nil)
 '(mark-ring-max 1)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (flycheck zenburn-theme magit helm-projectile projectile helm web-mode slime)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules")))
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(slime-load-failed-fasl (quote never))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(web-mode-enable-auto-pairing nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))



;; windmove mode
(windmove-default-keybindings)



(defun indent-buffer ()
  "Format the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)))
(global-set-key (kbd "C-S-F") 'indent-buffer)
;; fix "declaration with chaining" formatting for javascript mode
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))



(defun copy-filename ()
  "Copy the current file name."
  (interactive)
  (let ((filename (buffer-file-name (window-buffer (minibuffer-selected-window)))))
    (message filename)
    (kill-new filename)))
(global-set-key (kbd "C-c z") 'copy-filename)



;; scroll window line by line
(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)



(defun switch-to-scratch-buffer ()
  "Switch to the *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "<f1>") 'switch-to-scratch-buffer)



(defun switch-to-eshell-buffer ()
  "Switch to the *eshell* buffer."
  (interactive)
  (switch-to-buffer "*eshell*"))
(global-set-key (kbd "<f2>") 'switch-to-eshell-buffer)



;; Shortcut key for hs-minor-mode toggle hiding
(global-set-key (kbd "C-`") 'hs-toggle-hiding)



;; set up magit
(global-set-key (kbd "C-x g") 'magit-status)



;; set up projectile
(require 'projectile)
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
(setq projectile-completion-system 'helm)
(helm-projectile-on)



;; set up slime
(require 'slime)
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))



;; set up web mode
(require 'web-mode)
(defun my-web-mode-hook ()
  "Customize 'web-mode."
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil))
(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))



;; set up flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck use locally installed eslint
(defvar node-modules-bin-path nil
  "When not nil, it means node_modules/.bin/ has been added to the 'exec-path.")
(defun projectile-remove-node-modules-bin-from-exec-path ()
  "To be used as a projectile hook to remove node_modules/.bin/ from the 'exec-path."
  (when node-modules-bin-path
    (message (concat "Switch project, removing " node-modules-bin-path " from exec-path"))
    (setq exec-path (delete node-modules-bin-path exec-path))
    (setq node-modules-bin-path nil)))
(add-hook 'projectile-before-switch-project-hook
          #'projectile-remove-node-modules-bin-from-exec-path)
(defun use-eslint-if-available ()
  "To be used as a 'js-mode hook to enable 'flycheck-mode 'javascript-eslint if available."
  (when (and (not node-modules-bin-path)
             (string-equal "npm" (projectile-project-type)))
    (setq node-modules-bin-path (concat (projectile-project-root) "node_modules/.bin/"))
    (message (concat "npm project, adding " node-modules-bin-path " to exec-path"))
    (setq exec-path (cons node-modules-bin-path exec-path)))
  (flycheck-reset-enabled-checker 'javascript-eslint)
  (if (flycheck-may-enable-checker 'javascript-eslint)
      (progn (message "Enabling Flycheck javascript-eslint")
             (flycheck-select-checker 'javascript-eslint))
    (message "Flycheck javascript-eslint not available")))
(add-hook 'js-mode-hook #'use-eslint-if-available)

(provide 'emacs)

;;; .emacs ends here
