;; (require 'modus-themes)
;; (modus-themes-load-themes)
;; (modus-themes-load-operandi)

(load-theme 'solarized-light t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setenv "PATH" (concat "c:/Program Files/Git/usr/bin;" (getenv "PATH")))
(setq exec-path (cons "C:/Program Files/Git/usr/bin" exec-path))

(windmove-default-keybindings)
(global-set-key [M-down] (quote scroll-up-line))
(global-set-key [M-up] (quote scroll-down-line))

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

(defun format-buffer ()
  "Indent the entire buffer and deletes all trailing whitespace."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil))
  (delete-trailing-whitespace))

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

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

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

(cl-defmethod project-ignores (project dir)
  (git-ignores dir))

(defun clojure-project (dir)
  (let ((p (locate-dominating-file dir "project.clj")))
    (if p (cons 'clojure-project-clj (expand-file-name p)) nil)))
(add-hook 'project-find-functions #'clojure-project)

(require 'clojure-mode)
(define-key clojure-mode-map (kbd "C-c f") 'cider-format-buffer)
;; (add-hook 'clojure-mode-hook
;;           (lambda ()
;;             (when (project-current)
;;               (eglot-ensure))))

(defun scala-project (dir)
  (let ((p (locate-dominating-file dir "build.sbt")))
    (if p (cons 'scala-build-sbt (expand-file-name p)) nil)))
(add-hook 'project-find-functions #'scala-project)

(defun python-project (dir)
  (let ((p (locate-dominating-file dir "pyproject.toml")))
    (if p (cons 'python-pyproject-toml (expand-file-name p)) nil)))
(add-hook 'project-find-functions #'python-project)

(add-hook 'python-mode-hook
          (lambda ()
            (when (project-current)
              (let ((venvpath (concat (cdr (project-current)) ".venv")))
                (message "Activating pyvenv %s" venvpath)
                (pyvenv-mode)
                (pyvenv-activate venvpath)
                (eglot-ensure)))))

(defun npm-project (dir)
  (let ((p (locate-dominating-file dir "package.json")))
    (if p (cons 'npm-package-json (expand-file-name p)) nil)))
(add-hook 'project-find-functions #'npm-project)

(require 'rjsx-mode)
(add-hook 'rjsx-mode-hook
          (lambda ()
            (when (project-current)
              (eglot-ensure))))
(define-key rjsx-mode-map (kbd "M-.") nil)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)

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
 '(display-time-mode t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-selected-packages
   '(solarized-theme yaml-mode helm rjsx-mode pyvenv sbt-mode scala-mode cider eglot leuven-theme magit modus-themes))
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((eglot-ignored-server-capabilities :completionProvider)
     (eglot-ignored-server-capabilities :hoverProvider)
     (eglot-ignored-server-capabilities :hoverProvider :completionProvider)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight normal :height 90 :width normal)))))
