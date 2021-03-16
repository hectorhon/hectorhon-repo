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
  (if lsp-mode
      (lsp-format-buffer)
    (progn
      (save-excursion
        (indent-region (point-min) (point-max) nil))
      (delete-trailing-whitespace))))
(global-set-key (kbd "C-c f") (quote format-buffer))



(defadvice yank (after indent-region activate)
  (if (member major-mode '(js-mode lisp-mode rustic-mode java-mode))
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



(if (fboundp 'highlight-thing-mode)
    (add-hook 'prog-mode-hook 'highlight-thing-mode))
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
;; (with-eval-after-load 'flycheck
;;   (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
;;   (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))
(add-hook 'js-mode-hook (lambda ()
                          (unless (or (and (stringp buffer-file-name)
                                           (or (string-match "\\.json\\'" buffer-file-name)))
                                      (string-match "*HTTP Response*" (buffer-name)))
                            (flymake-eslint-enable))))
(add-hook 'web-mode-hook (lambda ()
                           (when (and (stringp buffer-file-name)
                                      (or (string-match "\\.jsx\\'" buffer-file-name)))
                             (flymake-eslint-enable))))



(if (fboundp 'global-undo-tree-mode)
    (global-undo-tree-mode))



(global-set-key (kbd "C-x g") 'magit-status)



(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))



(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))



(with-eval-after-load 'rustic
  (defun rustic-project-buffer-list ()
    "(Replace the existing function.)"
    (when-let ((pr (project-current)))
      (if (fboundp 'project--buffer-list)
          (project--buffer-list pr)
        (let ((root (car (project-roots pr)))
              bufs)
          (dolist (buf (buffer-list))
            (let ((filename (or (buffer-file-name buf)
                                (buffer-local-value 'default-directory buf))))
              (when (and filename (file-in-directory-p filename root))
                (push buf bufs))))
          (nreverse bufs)))))
  (advice-add 'rustic-insert-errno-button :override (lambda () ()))
  (add-hook 'rustic-mode-hook (lambda () (setq buffer-save-without-query t))))



(add-hook 'java-mode-hook #'lsp-deferred)
(add-hook 'java-mode-hook #'yas-minor-mode)



(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'lsp-deferred)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'yas-minor-mode))



(defvar project-marker-types
  '(("\\.asd" . asdf)
    ("package.json" . npm)
    ("Cargo.toml" . rust)
    ("pom.xml" . java)))
(defun project-try-marker (dir)
  (let* ((regex (string-join (mapcar 'car project-marker-types) "\\|"))
         (root (locate-dominating-file dir
                                       (lambda (name)
                                         (directory-files name nil regex t)))))
    (when root
      (let* ((marker-file-name (car (directory-files root nil regex t)))
             (project-type (alist-get marker-file-name project-marker-types nil nil 'string-match)))
        (list 'marker project-type root)))))
(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'project-try-marker)
  (cl-defmethod project-roots ((project (head marker)))
    (list (caddr project)))
  (cl-defmethod project-ignores ((project (head marker)) dir)
    ;; Fix .gitignore in subdirectories being ignored by emacs vc
    (let* ((default-directory dir)
           (git-output (shell-command-to-string "git ls-files -z --others --ignored --exclude-standard --directory")))
      (butlast (split-string git-output "\0")))))
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p g") 'project-find-regexp)
(defun project--read-file-cpd-relative-2 (prompt all-files &optional predicate hist default)
  "Like `project--read-file-cpd-relative', but with sorting by modified date."
  (let* ((all-files-sorted (sort all-files 'file-newer-than-file-p))
         (common-parent-directory
          (let ((common-prefix (try-completion "" all-files-sorted)))
            (if (> (length common-prefix) 0)
                (file-name-directory common-prefix))))
         (cpd-length (length common-parent-directory))
         (prompt (if (zerop cpd-length)
                     prompt
                   (concat prompt (format " in %s" common-parent-directory))))
         (substrings (mapcar (lambda (s) (substring s cpd-length)) all-files-sorted))
         (new-collection (project--file-completion-table substrings))
         (res (project--completing-read-strict prompt
                                               new-collection
                                               predicate
                                               hist (car substrings))))
    (concat common-parent-directory res)))


(if (fboundp 'ivy-mode)
    (ivy-mode 1))
(if (fboundp 'counsel-M-x)
    (global-set-key (kbd "M-x") 'counsel-M-x))



(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-asdf))
(with-eval-after-load 'slime
  (setq common-lisp-hyperspec-root "file:///home/hectorhon/Downloads/HyperSpec-7-0/HyperSpec/")
  (advice-add 'hyperspec-lookup
              :around
              (lambda (orig-fun &rest args)
                (setq-local browse-url-browser-function 'eww-browse-url)
                (apply orig-fun args)))
  (defun slime-inspect-symbol-at-point (string)
    "Make slime-inspect take a symbol instead of string."
    (interactive
     (list (slime-read-from-minibuffer "Inspect symbol: " (slime-sexp-at-point))))
    (slime-eval-async `(swank:init-inspector ,(concat "'" string)) 'slime-open-inspector))
  (define-key slime-mode-map (kbd "C-c I") 'slime-inspect-symbol-at-point)
  (define-key slime-mode-map (kbd "C-c i") 'slime-inspect))
(with-eval-after-load 'slime-repl
  (define-key slime-repl-mode-map (kbd "C-c C-v") nil)
  ;; (define-key slime-mode-indirect-map (kbd "C-c C-t") 'slime-trace-dialog-toggle-trace)
  (defun ora-slime-completion-in-region (_fn completions start end)
    (funcall completion-in-region-function start end completions))
  (advice-add
   'slime-display-or-scroll-completions
   :around #'ora-slime-completion-in-region))



(defun update-node-modules-path ()
  "Hook to add/remove node_modules/.bin to 'exec-path."
  (let ((project (project-current)))
    (when (eq 'marker (car project))
      (let ((project-type (cadr project))
            (project-root (file-name-as-directory (caddr project))))
        (when (eq 'npm project-type)
          (make-local-variable 'node-modules-path)
          (make-local-variable 'exec-path)
          (setq node-modules-path (concat project-root "node_modules/.bin"))
          (message "Adding %s to local exec-path" node-modules-path)
          (add-to-list 'exec-path node-modules-path))))))
(add-hook 'js-mode-hook 'update-node-modules-path)
(add-hook 'web-mode-hook 'update-node-modules-path)
(add-hook 'typescript-mode-hook 'update-node-modules-path)



(global-set-key (kbd "<f5>") 'recompile)
;; (add-hook 'compilation-filter-hook
;;           (lambda ()
;;             (ansi-color-apply-on-region compilation-filter-start (point))))
;; (add-hook 'typescript-mode-hook
;;           (lambda ()
;;             (when (string-equal "npm" (projectile-project-type))
;;               (set (make-local-variable 'compile-command) "npm run build "))))



(setq read-process-output-max (* 1024 1024)) ;; 1mb - lsp performance
(add-hook 'js-mode-hook
          (lambda ()
            (unless (and (stringp buffer-file-name)
                         (or (string-match "\\.json\\'" buffer-file-name)
                             (string-match "\\.js\\'" buffer-file-name)
                             (string-match "\\.jsx\\'" buffer-file-name)))
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
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-message-face 'default)
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(dired-listing-switches "-alX")
 '(display-time-mode t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "coverage" ".log" "build"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "package-lock.json"))
 '(highlight-thing-case-sensitive-p t)
 '(highlight-thing-delay-seconds 0.0)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-magic-tilde nil)
 '(ivy-sort-functions-alist
   '((read-file-name-internal . file-newer-than-file-p)
     (t . ivy-string<)))
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-java-server-install-dir "c:/Users/hectorhon/eclipse.jdt.ls/")
 '(lsp-keymap-prefix "C-c l")
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-enable-snippet nil)
 '(magit-auto-revert-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-capture-templates
   '(("t" "TODO" entry
      (file+headline "todo.org" "Tasks")
      "** TODO %?
   %u")))
 '(org-src-block-faces 'nil)
 '(org-startup-folded nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(rustic go-mode yasnippet lsp-java doom-themes company restclient color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow modus-themes solarized-theme highlight-thing flymake-eslint slime web-mode smex typescript-mode counsel undo-tree magit lsp-mode))
 '(project-read-file-name-function 'project--read-file-cpd-relative-2)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(slime-compile-file-options '(:fasl-directory "/home/hectorhon/tmp/fasl/"))
 '(slime-load-failed-fasl 'never)
 '(tab-width 4)
 '(temporary-file-directory "c:/Users/hectorhon/AppData/Local/Temp/")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines nil)
 '(vc-annotate-background-mode nil)
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
 '(web-mode-markup-indent-offset 2)
 '(window-divider-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 102 :width normal))))
 '(fixed-pitch ((t (:inherit default))))
 '(rustic-compilation-column ((t (:inherit compilation-column-number))))
 '(rustic-compilation-error ((t (:inherit compilation-error))))
 '(rustic-compilation-info ((t (:inherit compilation-info))))
 '(rustic-compilation-line ((t (:inherit compilation-line-number))))
 '(rustic-compilation-warning ((t (:inherit compilation-warning))))
 '(rustic-message ((t (:foreground "blue")))))

(setenv "PATH" (concat
                "C:\\Program Files\\Git\\usr\\bin" ";"
                (getenv "PATH")))
