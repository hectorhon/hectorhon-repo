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

(require 'js)
(define-key js-mode-map (kbd "M-.") nil)

(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(require 'eglot)
(add-hook 'js-mode-hook (lambda ()
                          (if (project-current)
                              (eglot-ensure))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-prompt-function 'cider-repl-prompt-lastname)
 '(cider-save-file-on-load t)
 '(column-number-mode t)
 '(completion-styles '(orderless basic))
 '(create-lockfiles nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-selected-packages
   '(consult orderless vertico doom-themes cider clojure-mode eglot magit))
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vertico-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "outline" :slant normal :weight normal :height 90 :width normal)))))
