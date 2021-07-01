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



(with-eval-after-load 'hideshow
  (define-key hs-minor-mode-map (kbd "C-`") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "M-`") 'hs-show-block))



;; (if (fboundp 'highlight-thing-mode)
;;     (add-hook 'prog-mode-hook 'highlight-thing-mode))
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)



(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))



(if (fboundp 'global-undo-tree-mode)
    (global-undo-tree-mode))



(global-set-key (kbd "C-x g") 'magit-status)



(advice-add 'js--multi-line-declaration-indentation :around
            (lambda (orig-fun &rest args) nil))
(advice-add 'js-find-symbol :around
            (lambda (orig-fun &rest args)
              (if (and (boundp 'lsp-mode) lsp-mode)
                  (xref-find-definitions)
                (apply orig-fun nil))))



(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))

(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))

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



(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-check)
  (define-key rust-mode-map (kbd "C-c C-k") 'rust-compile)
  (define-key rust-mode-map (kbd "C-c f") 'rust-format-buffer))
(add-hook 'rust-mode-hook 'company-mode)



(require 'wgrep)
(require 'wgrep-helm)
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(helm-autoresize-mode 1)
(helm-mode 1)

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(setq projectile-completion-system 'helm)
(helm-projectile-on)



(require 'eglot)
(add-to-list 'eglot-server-programs
             '(web-mode . ("typescript-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
             '(rust-mode . ("rust-analyzer")))
(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)



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
             (project-type (alist-get marker-file-name project-marker-types
                                      nil nil 'string-match)))
        (list 'marker project-type root)))))
(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'project-try-marker)
  (cl-defmethod project-roots ((project (head marker)))
    (list (caddr project)))
  (cl-defmethod project-ignores ((project (head marker)) dir)
    ;; Fix .gitignore in subdirectories being ignored by emacs vc
    (let* ((default-directory dir)
           (git-output (shell-command-to-string "git ls-files -z --others --ignored --exclude-standard --directory")))
      (append (butlast (split-string git-output "\0"))
              (list "vendor")))))
;; (global-set-key (kbd "C-c p f") 'project-find-file)
;; (global-set-key (kbd "C-c p g") 'project-find-regexp)
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



(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-asdf))
(with-eval-after-load 'slime
  (setq common-lisp-hyperspec-root
        "file:///home/hectorhon/Downloads/HyperSpec-7-0/HyperSpec/")
  (advice-add 'hyperspec-lookup
              :around
              (lambda (orig-fun &rest args)
                (setq-local browse-url-browser-function 'eww-browse-url)
                (apply orig-fun args)))
  (defun slime-inspect-symbol-at-point (string)
    "Make slime-inspect take a symbol instead of string."
    (interactive
     (list (slime-read-from-minibuffer "Inspect symbol: " (slime-sexp-at-point))))
    (slime-eval-async
     `(swank:init-inspector ,(concat "'" string)) 'slime-open-inspector))
  (define-key slime-mode-map (kbd "C-c I") 'slime-inspect-symbol-at-point)
  (define-key slime-mode-map (kbd "C-c i") 'slime-inspect))
(with-eval-after-load 'slime-repl
  (define-key slime-repl-mode-map
    (kbd "C-c C-v")
    nil)
  ;; (define-key slime-mode-indirect-map
  ;;   (kbd "C-c C-t")
  ;;   'slime-trace-dialog-toggle-trace)
  (defun ora-slime-completion-in-region (_fn completions start end)
    (funcall completion-in-region-function start end completions))
  (advice-add
   'slime-display-or-scroll-completions
   :around #'ora-slime-completion-in-region))



(global-set-key (kbd "<f5>") 'recompile)
(add-hook 'compilation-filter-hook
          (lambda ()
            (ansi-color-apply-on-region compilation-filter-start (point))))



(defun open-org-file ()
  "Open my org file."
  (interactive)
  (require 'org)
  (let ((my-org-file
         (concat (file-name-as-directory org-directory)
                 "notes.org")))
    (find-file my-org-file)))
(global-set-key (kbd "<f12>") (quote open-org-file))
(global-set-key (kbd "C-c a") 'org-agenda)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-error-regexp-alist
   '(absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek gradle-kotlin iar ibm irix java jikes-file maven jikes-line clang-include gcc-include ruby-Test::Unit gmake gnu lcc makepp mips-1 mips-2 omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line
            ("ERROR in \\(.*\\)(\\([0-9]+\\),\\([0-9]+\\))" 1 2 3)))
 '(compilation-message-face 'default)
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(dired-listing-switches "-alX")
 '(display-time-mode t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(grep-command "grep  -IirnH ")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "coverage" ".log" "build" "vendor"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "package-lock.json"))
 '(grep-use-null-device nil)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(highlight-thing-case-sensitive-p t)
 '(highlight-thing-delay-seconds 0.0)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((vertical-scroll-bars) (width . 90)))
 '(js-indent-level 4)
 '(js-switch-indent-offset 4)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-missing-semi-warning nil)
 '(magit-auto-revert-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-line-format
   '("%e" "  " mode-line-misc-info mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-end-spaces))
 '(org-agenda-files '("~/org"))
 '(org-capture-templates '(("t" "Todo" entry (file "~/org/notes.org") "* TODO %?")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-refile-targets '((org-agenda-files :level . 1)))
 '(org-src-block-faces 'nil)
 '(org-startup-folded nil)
 '(org-todo-keywords '((sequence "TODO" "ACTIVE" "DONE")))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(wgrep-helm helm-projectile projectile helm eglot wgrep spacemacs-theme rjsx-mode rust-mode pug-mode yasnippet doom-themes company restclient color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow modus-themes solarized-theme highlight-thing slime web-mode typescript-mode undo-tree magit))
 '(project-read-file-name-function 'project--read-file-cpd-relative-2)
 '(projectile-globally-ignored-directories
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd" "node_modules" "target"))
 '(projectile-project-root-files-top-down-recurring
   '(".svn" "CVS" "Makefile" "package.json" "Cargo.toml" ".git"))
 '(projectile-project-root-functions
   '(projectile-root-local projectile-root-top-down projectile-root-top-down-recurring))
 '(projectile-sort-order 'recentf)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 4)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(slime-compile-file-options '(:fasl-directory "/home/hectorhon/tmp/fasl/"))
 '(slime-load-failed-fasl 'never)
 '(tab-width 4)
 '(temporary-file-directory "c:/Users/hectorhon/AppData/Local/Temp/")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines nil)
 '(vc-follow-symlinks t)
 '(vc-git-grep-template
   "git --no-pager grep --untracked -In <C> -e <R> -- <F> ':!package-lock.json'")
 '(web-mode-code-indent-offset 4)
 '(web-mode-comment-formats
   '(("jsx" . "//")
     ("java" . "/*")
     ("javascript" . "/*")
     ("typescript" . "//")
     ("php" . "/*")
     ("css" . "/*")))
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-markup-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "outline" :slant normal :weight normal :height 102 :width normal))))
 '(eglot-highlight-symbol-face ((t (:inherit highlight))))
 '(fixed-pitch ((t (:inherit default)))))


(setenv "PATH" (concat
                "C:\\Program Files\\Git\\usr\\bin" ";"
                (getenv "PATH")))
