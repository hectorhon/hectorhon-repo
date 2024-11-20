(setenv "PATH" (concat "c:/Program Files/Git/usr/bin;"
                       (getenv "PATH")))
(setq exec-path (cons "C:/Program Files/Git/usr/bin" exec-path))
(setq exec-path
      (cons "C:/Users/hectorhon/repo/garden/garden-web-app/node_modules/.bin"
            exec-path))
(setq insert-directory-program "c:/Program Files/Git/usr/bin/ls.exe")

(add-to-list 'auto-mode-alist '("\\.js[mx]?\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-ts-mode))

(windmove-default-keybindings)
(global-set-key [M-down] 'scroll-up-line)
(global-set-key [M-up] 'scroll-down-line)
(global-set-key [M-S-down] (lambda () (interactive) (scroll-other-window-down -1)))
(global-set-key [M-S-up] (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

;; (mapconcat 'buffer-name (buffer-list) "\n")

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

(require 'xref)
(defun hectorhon/xref--insert-xrefs (xref-alist)
  (require 'compile) ; For the compilation faces.
  (cl-loop for (group . xrefs) in xref-alist
           for max-line = (cl-loop for xref in xrefs
                                   maximize (xref-location-line
                                             (xref-item-location xref)))
           for line-format = (and max-line
                                  (format
                                   #("%%%dd:" 0 4 (face xref-line-number) 5 6 (face shadow))
                                   (1+ (floor (log max-line 10)))))
           with item-text-props = (list 'mouse-face 'highlight
                                        'keymap xref--button-map
                                        'help-echo
                                        (concat "mouse-2: display in another window, "
                                                "RET or mouse-1: follow reference"))
           with prev-group = nil
           with prev-line = nil
           do
           (xref--insert-propertized '(face xref-file-header xref-group t)
                                     "\n" group "\n")
           (dolist (xref xrefs)
             (pcase-let (((cl-struct xref-item summary location) xref))
               (let* ((line (xref-location-line location))
                      (prefix
                       (cond
                        ((not line) "  ")
                        ((and (equal line prev-line)
                              (equal prev-group group))
                         "")
                        (t (format line-format line)))))
                 ;; Render multiple matches on the same line, together.
                 (when (and (equal prev-group group)
                            (or (null line)
                                (not (equal prev-line line))))
                   (insert "\n"))
                 (xref--insert-propertized (nconc (list 'xref-item xref)
                                                  item-text-props)
                                           prefix summary)
                 (setq prev-line line
                       prev-group group))))
           (insert "\n"))
  (add-to-invisibility-spec '(ellipsis . t))
  (save-excursion
    (goto-char (point-min))
    (while (= 0 (forward-line 1))
      (xref--apply-truncation)))
  (run-hooks 'xref-after-update-hook))
(advice-add 'xref--insert-xrefs :override #'hectorhon/xref--insert-xrefs)

(use-package dired
  :config
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file)
  ;; :init
  ;; (add-hook 'dired-after-readin-hook
  ;;           (lambda ()
  ;;             (let ((inhibit-read-only t))
  ;;               (goto-char (point-min))
  ;;               (let ((current-extension nil))
  ;;                 (while (not (eobp))
  ;;                   (let ((extension-at-line
  ;;                          (let ((str (thing-at-point 'line t)))
  ;;                            (if (string-match "\\(\\.[A-Za-z]+\\)$" str)
  ;;                                (match-string 0 str)
  ;;                              nil))))
  ;;                     (unless (string-equal current-extension extension-at-line)
  ;;                       (goto-char (line-beginning-position))
  ;;                       (insert "\n") ; (or extension-at-line "") "\n")
  ;;                       (setq current-extension extension-at-line))
  ;;                     (forward-line 1)))))))
  )

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :bind (("M-]" . embark-act)))

(use-package consult
  :bind ("C-x r b" . consult-bookmark))

(use-package vertico
  :init
  (vertico-mode)
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply #'consult-completion-in-region args))))

(use-package project
  :config
  (advice-add
   #'project-try-vc
   :around
   (lambda (orig-fun &rest args)
     (let ((res (apply orig-fun args)))
       (when res
         (setf (nth 1 res) 'Git)
         res)))))

(use-package hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind
  ("C-c <right>" . hs-show-block)
  ("C-c <left>" . hs-hide-block)
  ("C-c <" . hs-hide-all)
  ("C-c >" . hs-show-all))

(use-package yasnippet
  :config (yas-reload-all)
  :hook (tsx-ts-mode . yas-minor-mode)
  :hook (typescript-ts-mode . yas-minor-mode)
  :hook (js-ts-mode . yas-minor-mode))

(use-package js
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js-ts-mode-map (kbd "M-.") nil)
  (setq js--declaration-keyword-re "")
  (let ((js-rules (alist-get 'javascript js--treesit-indent-rules)))
    (setf (alist-get '(node-is #1="switch_\\(?:case\\|default\\)")
                     js-rules nil nil 'equal)
          '(parent-bol 2))))

(use-package apheleia
  ;; :init (apheleia-global-mode +1))
  :hook (tsx-ts-mode . apheleia-mode)
  :hook (typescript-ts-mode . apheleia-mode)
  :hook (js-ts-mode . apheleia-mode))

(use-package company
  :hook (prog-mode . company-mode))
  ;; :bind
  ;; ("C-M-i" . company-complete)
  ;; (:map company-active-map ("<tab>" . company-complete-selection)))

(use-package eglot
  :bind
  ("C-." . eglot-code-actions)
  ("C-c C-f" . eglot-format-buffer)
  ("C-c C-r" . eglot-rename)
  :config
  (eglot--code-action eglot-code-action-organize-imports-ts
                      "source.organizeImports.ts")
  (add-hook 'before-save-hook (lambda ()
                                (when (member major-mode '(tsx-ts-mode))
                                  (eglot-code-action-organize-imports-ts 1)))))

(use-package flymake
  :bind
  ("M-p" . flymake-goto-prev-error)
  ("M-n" . flymake-goto-next-error))

(use-package flymake-eslint
  :hook (eglot-managed-mode
         .
         (lambda ()
           (when (derived-mode-p 'js-ts-mode)
             (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)
             (flymake-eslint-enable)))))
;; :hook (js-ts-mode . flymake-eslint-enable))

(defun browse-current-clojure-ns ()
  (interactive)
  (let ((namespace (cider-current-ns)))
    (with-current-buffer
        (cider-popup-buffer cider-browse-ns-buffer 'select nil 'ancillary)
      (cider-browse-ns--list
       (current-buffer)
       namespace
       (cider-browse-ns--combined-vars-with-meta namespace)
       namespace))))

(use-package cider
  :bind
  ("C-h n" . browse-current-clojure-ns)
  :init
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (if (eq major-mode 'cider-mode)
                  (setq completion-at-point-functions
                        '(cider-complete-at-point t))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(cider-connection-message-fn 'cider-random-tip)
 '(cider-repl-display-help-banner nil)
 '(cider-save-file-on-load t)
 '(cider-test-fail-fast nil)
 '(cider-test-show-report-on-success t)
 '(clojure-ts-ensure-grammars nil)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes t)
 '(default-frame-alist '((vertical-scroll-bars) (width . 90)))
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-ignored-server-capabilities '(:inlayHintProvider))
 '(global-auto-revert-mode t)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(ls-lisp-use-insert-directory-program t)
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
 '(make-backup-files nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(embark embark-consult apheleia treemacs imenu-list yasnippet ef-themes leuven-theme company paredit scala-mode yaml-mode consult solarized-theme rust-mode flymake-eslint clojure-mode magit modus-themes orderless cider vertico))
 '(project-vc-extra-root-markers '("project.clj" "package.json" "Cargo.toml" "build.sbt"))
 '(ring-bell-function 'ignore)
 '(rust-indent-offset 2)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(split-width-threshold 150)
 '(tool-bar-mode nil)
 '(treemacs-display-in-side-window nil)
 '(treemacs-filewatch-mode nil)
 '(treemacs-follow-mode nil)
 '(treemacs-fringe-indicator-mode nil)
 '(treemacs-git-mode nil)
 '(treemacs-no-delete-other-windows nil)
 '(treemacs-width-is-initially-locked nil)
 '(whitespace-style '(face lines-tail)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Roboto Mono" :foundry "outline" :slant normal :weight regular :height 102 :width normal))))
 '(cider-error-overlay-face ((t (:extend t :background "orange red" :foreground "white"))))
 '(cider-test-failure-face ((t (:background "orange red" :foreground "white"))))
 '(whitespace-line ((t (:background "old lace" :foreground "#884900")))))
(put 'downcase-region 'disabled nil)
