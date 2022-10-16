(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (require 'modus-themes)
;; (modus-themes-load-themes)
;; (modus-themes-load-operandi)

;; (load-theme 'leuven t)
;; (load-theme 'doom-ayu-light t)
;; (load-theme 'doom-plain t)
;; (load-theme 'doom-one-light t)
;; (load-theme 'solarized-light t)
(load-theme 'solarized-selenized-white t)
;; (load-theme 'spacemacs-light t)
;; (load-theme 'sanityinc-tomorrow-day t)

(windmove-default-keybindings)
(global-set-key [M-down] 'scroll-up-line)
(global-set-key [M-up] 'scroll-down-line)

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

(savehist-mode)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides nil
      completion-category-defaults nil)

(require 'vertico)
(vertico-mode)
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(require 'project)

(defun git-ignores (dir)
  (let* ((root-command (format "git -C %s rev-parse --show-toplevel" dir))
         (root-path (replace-regexp-in-string
                     "\n\\'" "" (shell-command-to-string root-command)))
         (relative-dir (file-relative-name dir root-path))
         (command (format "git -C %s status --ignored --porcelain %s" root-path dir))
         (output (shell-command-to-string command))
         (lines (split-string output "\n"))
         (ignored (seq-filter (lambda (line)
                                (string-prefix-p "!!" line))
                              lines))
         (result (mapcar (lambda (s)
                           (file-relative-name (substring s 3) relative-dir))
                         ignored)))
    result))

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
         (result (mapcar (lambda (s) (concat root-path s))
                         lines)))
    result))

(cl-defmethod project-ignores (project dir)
  (git-ignores dir))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "project.clj")))
              (if p (cons 'clojure-project-clj (expand-file-name p)) nil))))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "deps.edn")))
              (if p (cons 'clojure-deps-edn (expand-file-name p)) nil))))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "build.sbt")))
              (if p (cons 'scala-build-sbt (expand-file-name p)) nil))))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "pyproject.toml")))
              (if p (cons 'python-pyproject-toml (expand-file-name p)) nil))))

(add-hook 'project-find-functions
          (lambda (dir)
            (let ((p (locate-dominating-file dir "package.json")))
              (if p (cons 'npm-package-json (expand-file-name p)) nil))))

(require 'flymake)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)

(require 'clojure-mode)
(require 'flymake-kondor)
(add-hook 'clojure-mode-hook (lambda ()
			       (flymake-kondor-setup)
			       (flymake-mode)))

(require 'cider-mode)
(define-key cider-mode-map (kbd "C-c f") 'cider-format-buffer)

(require 'js)
(define-key js-mode-map (kbd "M-.") nil)

;; (require 'rjsx-mode)
;; (define-key rjsx-mode-map (kbd "M-.") nil)
;; (define-key rjsx-mode-map "<" nil)
;; (define-key rjsx-mode-map (kbd "C-d") nil)
;; (define-key rjsx-mode-map ">" nil)

(require 'eglot)
(add-hook 'js-mode-hook (lambda ()
                          (when (project-current) (eglot-ensure))))
(define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(cider-connection-message-fn nil)
 '(cider-repl-display-help-banner nil)
 '(cider-save-file-on-load t)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(dired-listing-switches "-alB")
 '(eldoc-echo-area-use-multiline-p nil)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-switch-indent-offset 4)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(make-backup-files nil)
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow solarized-theme spacemacs-theme doom-themes flymake-kondor leuven-theme consult cider clojure-mode eglot magit modus-themes orderless vertico))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
