(require 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-operandi)

(windmove-default-keybindings)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; START Set up project.el

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

(defun go-project (dir)
  (let ((p (locate-dominating-file dir "go.mod")))
    (if p (cons 'go-go-mod p) nil)))
(add-hook 'project-find-functions #'go-project)

(defun npm-project (dir)
  (let ((p (locate-dominating-file dir "package.json")))
    (if p (cons 'npm-package-json p) nil)))
(add-hook 'project-find-functions #'npm-project)

(defun clojure-project (dir)
  (let ((p (locate-dominating-file dir "deps.edn")))
    (if p (cons 'clojure-deps-edn p) nil)))
(add-hook 'project-find-functions #'clojure-project)

;;; END Set up project.el

(setf eglot-stay-out-of '(imenu))

(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(inf-clojure clojure-mode cider typescript-mode go-mode eglot magit modus-themes))
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 102 :width normal)))))
