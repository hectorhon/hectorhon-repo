(setenv "PATH" (concat "c:/Program Files/Git/usr/bin;" (getenv "PATH")))
(setq exec-path (cons "C:/Program Files/Git/usr/bin" exec-path))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(windmove-default-keybindings)
(global-set-key [M-down] 'scroll-up-line)
(global-set-key [M-up] 'scroll-down-line)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

(defun open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))
(global-set-key (kbd "C-o") 'open-next-line)

(defun open-previous-line (arg)
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))
(global-set-key (kbd "M-o") 'open-previous-line)

(defun format-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace))
(global-set-key (kbd "C-c f") (quote format-buffer))

(defun copy-buffer-file-name ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message filename))))
(global-set-key (kbd "C-c z") (quote copy-buffer-file-name))

(setq completion-category-defaults nil)
(setq completion-category-overrides nil)
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(require 'project)

(cl-defmethod project-root (project)
  (cdr project))

(cl-defmethod project-files (project &optional dirs)
  (let* ((dir (car (or dirs
                       (list (project-root project)))))
         (root-command (format "git -C %s rev-parse --show-toplevel" dir))
         (root-path (file-name-as-directory
                     (replace-regexp-in-string
                      "\n\\'" "" (shell-command-to-string root-command))))
         (command (format "git -C %s ls-files -oc --exclude-standard --full-name" dir))
         (output (replace-regexp-in-string
                  "\n\\'" "" (shell-command-to-string command)))
         (lines (split-string output "\n" t))
         (deleted-paths
          (let* ((command
                  (format "git -C %s ls-files -d --exclude-standard --full-name" dir))
                 (output (replace-regexp-in-string
                          "\n\\'" "" (shell-command-to-string command)))
                 (lines (split-string output "\n" t)))
            lines))
         (result (mapcar (lambda (s) (concat root-path s))
                         (seq-difference lines deleted-paths))))
    result))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "project.clj")))
              (if p (cons 'clojure-project-clj (expand-file-name p)) nil))))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "package.json")))
              (if p (cons 'nodejs-package-json (expand-file-name p)) nil))))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "Cargo.toml")))
              (if p (cons 'rust-cargo-toml (expand-file-name p)) nil))))

(require 'js)
(define-key js-mode-map (kbd "M-.") nil)
(advice-add 'js--multi-line-declaration-indentation :override #'ignore)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(require 'eglot)
(add-hook 'js-mode-hook (lambda ()
                          (if (project-current)
                              (eglot-ensure))))

(require 'clojure-mode)
(put-clojure-indent 'match 1)
(add-hook 'clojure-mode-hook #'paredit-mode)
(define-fringe-bitmap 'cider-filled-rectangle
  (make-vector 100 #b01111110)
  nil nil 'top)
(with-eval-after-load 'cider
  (setf cider--fringe-overlay-good
        (propertize " " 'display '(left-fringe
                                   cider-filled-rectangle
                                   cider-fringe-good-face))))
(defun hectorhon/clojure-grep-defmethods ()
  (interactive)
  (project-find-regexp
   (concat "defmethod"
           "[[:space:]]"
           (car (last (split-string (thing-at-point 'symbol) "/"))))))
(global-set-key (kbd "C-c g") 'hectorhon/clojure-grep-defmethods)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(cider-connection-message-fn nil)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-prompt-function 'cider-repl-prompt-lastname)
 '(cider-repl-require-ns-on-set t)
 '(cider-save-file-on-load t)
 '(cider-special-mode-truncate-lines nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(completion-styles '(orderless basic))
 '(create-lockfiles nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-selected-packages
   '(paredit solarized-theme rust-mode magit yaml-mode leuven-theme spacemacs-theme consult orderless vertico doom-themes cider clojure-mode eglot))
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vertico-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Roboto Mono" :foundry "outline" :slant normal :weight normal :height 102 :width normal))))
 '(cider-fringe-good-face ((t (:inherit font-lock-keyword-face))))
 '(cider-test-failure-face ((t (:background "orange red" :foreground "white"))))
 '(fixed-pitch ((t (:inherit default))))
 '(fringe ((t (:inherit default)))))
(put 'upcase-region 'disabled nil)
