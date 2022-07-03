;; (load-theme 'modus-operandi t)
(load-theme 'leuven t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(windmove-default-keybindings)

(global-set-key [M-down] (quote scroll-up-line))
(global-set-key [M-up] (quote scroll-down-line))
(global-set-key (kbd "C-c >") 'indent-rigidly-right)
(global-set-key (kbd "C-c <") 'indent-rigidly-left)

(require 'org)
;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(define-key org-mode-map (kbd "<M-down>") 'scroll-up-line)
(define-key org-mode-map (kbd "<M-up>") 'scroll-down-line)
(define-key org-mode-map (kbd "C-c >") 'indent-rigidly-right)
(define-key org-mode-map (kbd "C-c <") 'indent-rigidly-left)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

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

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x B") 'helm-bookmarks)

(defun python-project (dir)
  (let ((p (locate-dominating-file dir "venv")))
    (if p (cons 'python-venv p) nil)))
(cl-defmethod project-root ((project (head python-venv)))
  (cdr project))
(add-hook 'project-find-functions #'python-project)

;; (require 'highlight-thing)
;; (add-hook 'prog-mode-hook 'highlight-thing-mode)
;; (add-hook 'restclient-mode 'highlight-thing-mode)

;; (require 'rego-mode)
;; (add-hook 'rego-mode-hook 'highlight-thing-mode)

(require 'magit)

;; (require 'restclient)
;; (add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

(require 'undo-tree)
(global-undo-tree-mode)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(require 'helm-projectile)
(helm-projectile-on)

(require 'eglot)
(define-key eglot-mode-map (kbd "C-.") 'eglot-code-actions)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c !") 'flymake-show-buffer-diagnostics)

(add-hook 'python-mode-hook
          (lambda ()
            (let ((root (locate-dominating-file "." "venv")))
              (when root
                (message "Activating pyvenv %s%s" root "venv")
		(pyvenv-mode)
                (pyvenv-activate (concat root "venv"))
                (when (projectile-project-root)
		  (eglot-ensure)
                  (company-mode))))))
(add-hook 'go-mode-hook
          (lambda ()
            (when (projectile-project-root)
              (eglot-ensure))))
(add-hook 'typescript-mode-hook
          (lambda ()
            (when (projectile-project-root)
              (eglot-ensure)
              (company-mode))))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(global-auto-revert-mode t)
 '(grep-find-ignored-directories
   '("node_modules" ".angular" "venv" "SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-status-sections-hook
   '(magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpushed-to-pushremote magit-insert-unpushed-to-upstream magit-insert-recent-commits magit-insert-unpulled-from-pushremote magit-insert-unpulled-from-upstream))
 '(make-backup-files nil)
 '(org-agenda-files '("~/datetree.org"))
 '(org-capture-templates
   '(("d" "datetree" entry
      (file+olp+datetree "~/datetree.org")
      "* %i%?" :jump-to-captured t :tree-type week)))
 '(package-selected-packages
   '(json-mode company web-mode typescript-mode yaml-mode dockerfile-mode markdown-mode go-mode rego-mode highlight-thing leuven-theme python-black restclient modus-themes helm-projectile undo-tree eglot helm magit projectile pyvenv))
 '(projectile-project-root-functions
   '(projectile-root-local projectile-root-top-down projectile-root-bottom-up projectile-root-top-down-recurring))
 '(rego-format-at-save nil)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(undo-tree-auto-save-history nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
