;;; package --- Summary

;;; Commentary:

;;; Code:

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
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (leuven)))
 '(display-time-mode t)
 '(elpy-modules nil)
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "dist")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.map")))
 '(helm-grep-truncate-lines nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (elpy json-mode flycheck helm-projectile helm magit projectile web-mode)))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values (quote ((engine . jsx) (engine . django))))
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(web-mode-code-indent-offset 2)
 '(web-mode-comment-formats
   (quote
    (("java" . "/*")
     ("javascript" . "//")
     ("php" . "/*")
     ("css" . "/*")
     ("jsx" . "//"))))
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-engine-detection t)
 '(web-mode-markup-indent-offset 2))
(cond
 ((string-equal system-type "windows-nt")
  (customize-set-variable 'ediff-diff-program "C:\\Program Files\\Git\\usr\\bin\\diff.exe")
  (customize-set-variable 'projectile-use-git-grep t)))
(cond
 ((string-equal system-type "windows-nt")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))))
 ((string-equal system-type "gnu/linux")
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 113 :width normal)))))))



(windmove-default-keybindings)
(global-set-key [M-down] (quote scroll-up-line))
(global-set-key [M-up] (quote scroll-down-line))
(defun format-buffer ()
  "Indent the entire buffer and deletes all trailing whitespace."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace))
(global-set-key (kbd "C-c f") (quote format-buffer))
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



(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-s o") 'helm-occur)



(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)



(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)



(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
;; (setq projectile-switch-project-action 'magit-status)



(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))



(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Customize flycheck checkers for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
(defun customize-flycheck-checkers-for-web-mode ()
  "Customize flycheck checkers for web-mode."
  (when (string-equal "django" web-mode-engine)
    (push 'javascript-eslint flycheck-disabled-checkers)))
(add-hook 'web-mode-hook 'customize-flycheck-checkers-for-web-mode)

(defvar node-modules-path nil)
(defun update-node-modules-path ()
  "Hook to add/remove node_modules/.bin to 'exec-path (for flycheck eslint)."
  (when node-modules-path           ; was previously set, unset it first
    (setq exec-path (delete node-modules-path exec-path))
    (setq node-modules-path nil))
  (when (string-equal "npm" (projectile-project-type))
    (setq node-modules-path
          (concat (projectile-project-root) "node_modules/.bin"))
    (setq exec-path (cons node-modules-path exec-path))))
(add-hook 'js-mode-hook 'update-node-modules-path)
(add-hook 'web-mode-hook 'update-node-modules-path)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))



(require 'elpy)
(elpy-enable)
(define-key elpy-mode-map (kbd "<M-up>") nil)
(define-key elpy-mode-map (kbd "<M-down>") nil)
;; https://elpy.readthedocs.io/en/latest/customization_tips.html
;; Use flycheck instead of flymake
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'pyvenv-post-activate-hooks
          (lambda ()
            (flycheck-reset-enabled-checker 'python-flake8)
            (flycheck-buffer)))



(provide 'emacs)
;;; .emacs ends here
