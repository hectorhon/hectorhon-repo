(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)



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

(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

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



(global-undo-tree-mode)



(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)



(require 'js)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))
;; (add-to-list 'auto-mode-alist '(".*\\.tsx\\'" . typescript-mode))



(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))



;; (require 'rjsx-mode)
;; (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '(".*\\.tsx\\'" . rjsx-mode))



(require 'ivy)
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)



(require 'projectile)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(defvar node-modules-path nil)
(defun update-node-modules-path ()
  "Hook to add/remove node_modules/.bin to 'exec-path."
  (when node-modules-path           ; was previously set, unset it first
    (message "Removing %s from exec-path" node-modules-path)
    (setq exec-path (delete node-modules-path exec-path))
    (setq node-modules-path nil))
  (when (string-equal "npm" (projectile-project-type))
    (setq node-modules-path (concat (projectile-project-root) "node_modules/.bin"))
    (message "Adding %s to exec-path" node-modules-path)
    (add-to-list 'exec-path node-modules-path)))
(add-hook 'js-mode-hook 'update-node-modules-path)
(add-hook 'web-mode-hook 'update-node-modules-path)
(add-hook 'typescript-mode-hook 'update-node-modules-path)

(global-set-key (kbd "<f5>") 'compile)
(require 'ansi-color)
(add-hook 'compilation-filter-hook
          (lambda()
            (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'typescript-mode-hook
          (lambda ()
            (when (string-equal "npm" (projectile-project-type))
              (set (make-local-variable 'compile-command) "npm run build "))))

(setq lsp-keymap-prefix "C-`")
(require 'lsp-mode)
;; (add-hook 'rjsx-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'lsp-deferred)
(define-key js-mode-map (kbd "M-.") 'lsp-find-type-definition)
(define-key js-mode-map (kbd "M-?") 'lsp-find-references)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(define-key lsp-mode-map (kbd "C-S-SPC") 'completion-at-point)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(global-auto-revert-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "coverage" ".log" "build"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "package-lock.json"))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-magic-tilde nil)
 '(ivy-sort-functions-alist
   '((projectile-completing-read . file-newer-than-file-p)
     (read-file-name-internal . ido-file-extension-lessp)
     (t . ivy-string<)))
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(web-mode smex typescript-mode counsel undo-tree magit projectile lsp-mode))
 '(projectile-indexing-method 'hybrid)
 '(projectile-project-root-files-bottom-up
   '("package.json" ".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs"))
 '(projectile-sort-order 'recentf)
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(typescript-indent-level 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 105 :width normal))))
 '(region ((t (:background "moccasin")))))
