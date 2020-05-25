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



(setq hi-lock-face-defaults
      (list "hi-black-hb"
            "hi-yellow"
            "hi-pink"
            "hi-green"
            "hi-blue"
            "hi-black-b"
            "hi-blue-b"
            "hi-red-b"
            "hi-green-b"))



(global-undo-tree-mode)



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



(require 'smex)
(smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c M-x") 'execute-extended-command)



(require 'ivy)
(require 'swiper)
(require 'counsel)
(counsel-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "M-s o") 'swiper)
(global-set-key (kbd "M-s O") 'occur)



(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)



(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
(defun projectile-root-bottom-up-for-project-el (dir)
  (let ((root (projectile-root-bottom-up dir)))
    (and root (cons 'projectile root))))
(cl-defmethod project-roots ((project (head projectile)))
  (list (cdr project)))
(add-to-list 'project-find-functions 'projectile-root-bottom-up-for-project-el)
(projectile-register-project-type 'python-virtualenv (list "pyvenv.cfg"))



(defvar node-modules-path nil)
(defun update-node-modules-path ()
  "Hook to add/remove node_modules/.bin to 'exec-path (for flycheck eslint)."
  (when node-modules-path           ; was previously set, unset it first
    (message "Removing %s from exec-path" node-modules-path)
    (setq exec-path (delete node-modules-path exec-path))
    (setq node-modules-path nil))
  (when (string-equal "npm" (projectile-project-type))
    (setq node-modules-path (concat (projectile-project-root) "node_modules/.bin"))
    (message "Adding %s to exec-path" node-modules-path)
    (add-to-list 'exec-path node-modules-path)))
(add-hook 'js-mode-hook 'update-node-modules-path)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))



;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (require 'lsp-mode)
;; (add-hook 'js-mode-hook #'lsp-deferred)
;; (global-set-key (kbd "M-.") 'lsp-find-type-definition)

(require 'flymake)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)

(require 'eglot)
(add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))

(require 'js)
(define-key js-mode-map (kbd "M-.") 'eglot-find-typeDefinition)
(add-hook 'js-mode-hook 'eglot-ensure)

(require 'rjsx-mode)
;; (with-eval-after-load 'rjsx-mode
;;   (define-key rjsx-mode-map "<" nil)
;;   (define-key rjsx-mode-map (kbd "C-d") nil)
;;   (define-key rjsx-mode-map ">" nil))



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
 '(dired-listing-switches "-alX")
 '(eglot-ignored-server-capabilites
   (quote
    (:hoverProvider :documentHighlightProvider :signatureHelpProvider :codeActionProvider)))
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-initial-inputs-alist
   (quote
    ((counsel-minor . "^+")
     (counsel-package . "^+")
     (counsel-org-capture . "^")
     (counsel-M-x . "")
     (counsel-describe-function . "")
     (counsel-describe-variable . "")
     (org-refile . "^")
     (org-agenda-refile . "^")
     (org-capture-refile . "^")
     (Man-completion-table . "^")
     (woman . "^"))))
 '(ivy-magic-tilde nil)
 '(ivy-sort-functions-alist
   (quote
    ((counsel-minor . ivy-string<)
     (counsel-colors-web . ivy-string<)
     (counsel-unicode-char . ivy-string<)
     (counsel-register . ivy-string<)
     (counsel-mark-ring . ivy-string<)
     (counsel-file-register . ivy-string<)
     (counsel-faces . ivy-string<)
     (counsel-describe-face . ivy-string<)
     (counsel-info-lookup-symbol . ivy-string<)
     (counsel-apropos . ivy-string<)
     (counsel-describe-function . ivy-string<)
     (counsel-describe-variable . ivy-string<)
     (read-file-name-internal . ido-file-extension-lessp)
     (t . ivy-string<))))
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (undo-tree rjsx-mode eglot flymake counsel smex pyvenv slime web-mode magit solarized-theme zenburn-theme projectile)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "*node_modules" "*site-packages" "*dist" "*vendor")))
 '(projectile-indexing-method (quote hybrid))
 '(projectile-project-root-files-bottom-up
   (quote
    ("pyvenv.cfg" "package.json" ".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs")))
 '(projectile-sort-order (quote recently-active))
 '(python-indent-offset 2)
 '(python-shell-interpreter "python3")
 '(ring-bell-function (quote ignore))
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(sql-server "/var/run/postgresql")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(vc-follow-symlinks t)
 '(web-mode-code-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((t (:weight normal))))
 '(font-lock-constant-face ((t (:weight normal))))
 '(font-lock-keyword-face ((t (:weight normal))))
 '(js2-error ((t (:underline (:style wave :color "yellow green")))))
 '(show-paren-match ((((class color) (min-colors 89)) (:foreground "#d33682" :background "green" :weight normal))))
 '(swiper-match-face-2 ((t (:background "#b58900" :foreground "#fdf6e3" :weight normal))))
 '(web-mode-html-attr-value-face ((t (:slant normal)))))

(provide '.emacs)

;;; .emacs ends here
