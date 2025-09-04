;;; -*- lexical-binding: t -*-

(setq exec-path (cons "C:/Program Files/Git/usr/bin" exec-path))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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

(use-package flymake
  :bind
  ("M-p" . flymake-goto-prev-error)
  ("M-n" . flymake-goto-next-error))

(defun flymake-eslint-enable-local (orig-fun &rest args)
  (when (and (buffer-file-name)
             (project-current)
             (file-exists-p (file-name-concat
                             (project-root (project-current))
                             "package.json")))
    (let ((node-modules-bin-path
           (file-name-concat (project-root (project-current))
                             "node_modules/.bin")))
      (unless (member node-modules-bin-path exec-path)
        (setq-local exec-path (cons node-modules-bin-path exec-path)))))
  (if (not (executable-find flymake-eslint-executable-name))
      (message "Can't find eslint on exec-path")
    (apply orig-fun args)))

(use-package flymake-eslint
  :init
  (advice-add 'flymake-eslint-enable :around #'flymake-eslint-enable-local))

(use-package treesit-fold
  :load-path "C:/Users/hectorhon/repo/third/treesit-fold")

(use-package typescript-ts-mode)

(use-package yaml-ts-mode)

(use-package bicep-ts-mode)

(use-package powershell-ts-mode
  :load-path "C:/Users/hectorhon/repo/third/powershell-ts-mode"
  :config
  (with-eval-after-load 'eglot
    (message "Adding powershell lsp server to eglot")
    (add-to-list
     'eglot-server-programs
     `(powershell-ts-mode
       . ("pwsh"
          "-OutputFormat" "Text"
          "-File"
          ,(expand-file-name ".cache/powershell/Start-EditorServices.ps1"
                             user-emacs-directory)
          "-Stdio"
          "-HostVersion" "1.0"
          "-HostName" "Emacs"
          "-HostProfileId" "Emacs.Eglot"
          "-SessionDetailsPath"
          ,(expand-file-name "eglot-powershell" temporary-file-directory)
          "-BundledModulesPath"
          ,(expand-file-name ".cache/powershell"))))))

(use-package angular-ts-mode
  :load-path "c:/Users/hectorhon/.emacs.d/site-lisp")

(defun eslint-enable-for-js-projects ()
  (when (file-exists-p
         (file-name-concat
          (project-root (project-current)) "package.json"))
    (flymake-eslint-enable)))

(defun typescript-organize-imports ()
  (when (and eglot--managed-mode
             (member major-mode '(typescript-ts-mode)))
    (eglot-code-action-organize-imports-ts 1)
    (eglot-code-action-remove-unused-imports-ts 1)))

(use-package eglot
  :bind
  ("C-." . eglot-code-actions)
  :hook
  (eglot-managed-mode . eslint-enable-for-js-projects)
  :config
  (eglot--code-action eglot-code-action-organize-imports-ts
                      "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-remove-unused-imports-ts
                      "source.removeUnusedImports.ts")
  (add-hook 'before-save-hook #'typescript-organize-imports)
  (progn
    (add-to-list
     'eglot-server-programs
     '((angular-ts-mode :language-id "html")
       "ngserver"
       "--tsProbeLocations"
       "c:/Users/hectorhon/AppData/Roaming/npm/node_modules"
       "--ngProbeLocations"
       "c:/Users/hectorhon/AppData/Roaming/npm/node_modules"
       "--logToConsole"
       "--stdio"))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind
  (("M-s r" . consult-ripgrep)
   ("M-s d" . consult-find)
   ("M-s l" . consult-line))
  :init
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package embark
  :bind
  (("C-;" . embark-act)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package apheleia
  :hook
  ((typescript-ts-mode . apheleia-mode)
   (angular-ts-mode . apheleia-mode)
   (js-mode . apheleia-mode)
   (js-json-mode . apheleia-mode))
  :config
  (setf (alist-get 'prettier-html apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=angular"
          (apheleia-formatters-js-indent "--use-tabs"
					 "--tab-width"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("9af2b1c0728d278281d87dc91ead7f5d9f2287b1ed66ec8941e97ab7a6ab73c0"
     "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"
     default))
 '(enable-recursive-minibuffers t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(package-selected-packages
   '(apheleia bicep-ts-mode consult embark embark-consult flymake-eslint
              magit orderless spacemacs-theme vertico))
 '(project-vc-extra-root-markers '("package.json"))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "outline" :slant normal :weight regular :height 113 :width normal)))))
