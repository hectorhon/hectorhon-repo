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
(global-set-key (kbd "M-s M-o") 'occur)



(defun format-buffer ()
  "Indent the entire buffer and deletes all trailing whitespace."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace))
(global-set-key (kbd "C-c f") (quote format-buffer))



(defadvice yank (after indent-region activate)
  (if (member major-mode '(typescript-mode))
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



(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-`") 'hs-toggle-hiding))



(with-eval-after-load 'highlight-symbol
  (global-set-key (kbd "M-[") 'highlight-symbol-prev)
  (global-set-key (kbd "M-]") 'highlight-symbol-next))



(global-undo-tree-mode)



(global-set-key (kbd "C-x g") 'magit-status)



(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))



(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))



(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)



(projectile-mode +1)
(with-eval-after-load 'projectile
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))



(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-asdf))
(add-hook 'slime-mode-hook 'highlight-symbol-mode)
(with-eval-after-load 'slime-repl
  (define-key slime-repl-mode-map (kbd "C-c C-v") nil)
  (define-key slime-mode-indirect-map (kbd "C-c C-t") 'slime-trace-dialog-toggle-trace)
  (global-set-key (kbd "C-c T") 'slime-trace-dialog)
  (defun ora-slime-completion-in-region (_fn completions start end)
    (funcall completion-in-region-function start end completions))
  (advice-add
   'slime-display-or-scroll-completions
   :around #'ora-slime-completion-in-region))



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



(global-set-key (kbd "<f5>") 'recompile)
(add-hook 'compilation-filter-hook
          (lambda ()
            (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'typescript-mode-hook
          (lambda ()
            (when (string-equal "npm" (projectile-project-type))
              (set (make-local-variable 'compile-command) "npm run build "))))



(setq lsp-keymap-prefix "C-c l")
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook
            (lambda ()
              (define-key lsp-mode-map (kbd "M-n") 'flymake-goto-next-error)
              (define-key lsp-mode-map (kbd "M-p") 'flymake-goto-prev-error))))
(add-hook 'js-mode-hook
          (lambda ()
            (unless (and (stringp buffer-file-name)
                         (string-match "\\.json\\'" buffer-file-name))
              (lsp-deferred))))
(add-hook 'web-mode-hook
          (lambda ()
            (if (and (stringp buffer-file-name)
                     (string-match "\\.tsx\\'" buffer-file-name))
                (lsp-deferred))))
(add-hook 'typescript-mode-hook #'lsp-deferred)

(defun trigger-lsp-update (window)
  (if lsp-mode (run-with-timer 1 nil 'lsp-on-change 0 1 1)))
(defun add-trigger-lsp-update ()
  (make-local-variable 'window-state-change-functions)
  (add-to-list 'window-state-change-functions 'trigger-lsp-update))
(add-hook 'typescript-mode-hook #'add-trigger-lsp-update)
(add-hook 'web-mode-hook #'add-trigger-lsp-update)



(defun open-tasks-file ()
  "Open my tasks file."
  (interactive)
  (require 'org)
  (let ((my-todo-file
         (concat (file-name-as-directory org-directory)
                 "todo.org")))
    (find-file my-todo-file)))
(global-set-key (kbd "<f12>") (quote open-tasks-file))

(with-eval-after-load 'org
  ;; Make windmove work in Org mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  ;; Keep my scroll-up/down-line
  (define-key org-mode-map [M-down] (quote scroll-up-line))
  (define-key org-mode-map [M-up] (quote scroll-down-line)))



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
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(dired-listing-switches "-alX")
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "coverage" ".log" "build"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "package-lock.json"))
 '(highlight-symbol-idle-delay 0)
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
 '(lsp-enable-snippet nil)
 '(magit-auto-revert-mode nil)
 '(magit-bury-buffer-function 'magit-mode-quit-window)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(org-capture-templates
   '(("t" "TODO" entry
      (file+headline "todo.org" "Tasks")
      "** TODO %?
   %u")))
 '(org-startup-folded nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(highlight-symbol slime web-mode smex typescript-mode counsel undo-tree magit projectile lsp-mode))
 '(projectile-globally-ignored-file-suffixes '(".fasl"))
 '(projectile-indexing-method 'hybrid)
 '(projectile-project-root-files-bottom-up
   '("package.json" "packages.lisp" ".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs"))
 '(projectile-sort-order 'recentf)
 '(projectile-use-git-grep t)
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(slime-load-failed-fasl 'never)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t)
 '(typescript-indent-level 2)
 '(vc-follow-symlinks t)
 '(vc-git-grep-template
   "git --no-pager grep --untracked -In <C> -e <R> -- <F> ':!package-lock.json'")
 '(web-mode-code-indent-offset 2)
 '(web-mode-comment-formats
   '(("jsx" . "//")
     ("java" . "/*")
     ("javascript" . "/*")
     ("typescript" . "//")
     ("php" . "/*")
     ("css" . "/*")))
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 105 :width normal))))
 '(lazy-highlight ((t (:background "yellow"))))
 '(region ((t (:background "moccasin")))))
