;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)



(require 'slime)
(setq inferior-lisp-program "sbcl")



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

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-o") 'smart-open-line-above)


(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))



(ivy-mode 1)
(global-set-key "\C-s" 'swiper)



(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)



(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)



(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)



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
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))



(add-hook 'after-init-hook #'global-flycheck-mode)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default)))
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-magic-tilde nil)
 '(ivy-sort-functions-alist
   (quote
    ((read-file-name-internal . ido-file-extension-lessp)
     (t . ivy-string<))))
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (swiper pyvenv slime web-mode magit flycheck solarized-theme zenburn-theme projectile ivy)))
 '(projectile-project-root-files-bottom-up
   (quote
    ("package.json" ".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs")))
 '(python-indent-offset 2)
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide '.emacs)

;;; .emacs ends here
