;;; -*- lexical-binding: t -*-

(add-to-list 'exec-path "C:/Program Files/Git/usr/bin")

(windmove-default-keybindings)

(keymap-global-set "M-<down>" 'scroll-up-line)
(keymap-global-set "M-<up>" 'scroll-down-line)

(defun h/open-line-before ()
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (previous-line)
  (indent-for-tab-command))
(keymap-global-set "M-o" 'h/open-line-before)

(defun h/format-document ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))
    (delete-trailing-whitespace)))
(keymap-global-set "C-c f" 'h/format-document)

(defun h/copy-buffer-file-path ()
  (interactive)
  (let ((path (buffer-file-name)))
    (kill-new path)
    (message path)))
(keymap-global-set "C-c z" 'h/copy-buffer-file-path)

(defun h/gitk ()
  (interactive)
  (start-process "gitk" nil "gitk" "--all"))
(keymap-global-set "C-c !" 'h/gitk)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil))

(use-package consult
  :bind
  ("M-s l" . consult-line)
  ("M-s M-s" . consult-ripgrep)
  ("M-s s" . consult-ripgrep))

(use-package vertico
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  :init
  (vertico-mode))

(use-package embark
  :bind
  (("C-c a" . embark-act)
   ("C-c A" . embark-act-all)
   ("C-c SPC" . embark-select)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package expreg
  :bind
  ("C-\\" . expreg-expand))

(use-package typescript-ts-mode)

(use-package corfu
  :hook
  (typescript-ts-mode . corfu-mode)
  (tsx-ts-mode . corfu-mode))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-p" . flymake-goto-prev-error)
        ("M-n" . flymake-goto-next-error)))

(use-package eglot
  :bind
  ("C-." . eglot-code-actions)
  ("C-c r" . eglot-rename)
  :config
  (eglot--code-action eglot-code-action-add-missing-imports-ts
                      "source.addMissingImports.ts")
  (eglot--code-action eglot-code-action-fix-all-ts
                      "source.fixAll.ts")
  (eglot--code-action eglot-code-action-organize-imports-ts
                      "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-remove-unused-ts
                      "source.removeUnused.ts")
  (eglot--code-action eglot-code-action-remove-unused-imports-ts
                      "source.removeUnusedImports.ts")
  (eglot--code-action eglot-code-action-sort-imports-ts
                      "source.sortImports.ts")
  (setq-default eglot-workspace-configuration
                '(:typescript
                  (:format
                   (:indentSize 2 :semicolons "remove")))))

(defun h/flymake-eslint-enable ()
  (when (member major-mode '(typescript-ts-mode tsx-ts-mode))
    (flymake-eslint-enable)))

(use-package flymake-eslint
  :hook
  (eglot-managed-mode . h/flymake-eslint-enable))

(use-package apheleia
  :hook
  (typescript-ts-mode . apheleia-mode)
  (tsx-ts-mode . apheleia-mode))

(defun h/typescript-ts-mode--indent-rules (language)
  "Rules used for indentation.
Argument LANGUAGE is either `typescript' or `tsx'."
  `((,language
     ((parent-is "program") column-0 0)
     ((node-is "}") standalone-parent 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((and (parent-is "comment") c-ts-common-looking-at-star)
      c-ts-common-comment-start-after-first-star -1)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "ternary_expression") standalone-parent typescript-ts-mode-indent-offset)
     ((parent-is "member_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "named_imports") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "statement_block") standalone-parent typescript-ts-mode-indent-offset)
     ((or (node-is "case")
          (node-is "default"))
      parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "switch_case") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "switch_default") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "type_arguments") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "type_parameters") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is ,(rx (or "variable" "lexical") "_" (or "declaration" "declarator")))
      parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "arguments") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "array") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "formal_parameters") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "template_string") no-indent) ; Don't indent the string contents.
     ((parent-is "template_substitution") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object_pattern") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "object_type") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "class_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "interface_body") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "arrow_function") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol typescript-ts-mode-indent-offset)
     ((parent-is "binary_expression") parent-bol typescript-ts-mode-indent-offset)
     ((match "while" "do_statement") parent-bol 0)
     ((match "else" "if_statement") parent-bol 0)
     ((parent-is ,(rx (or (seq (or "if" "for" "for_in" "while" "do") "_statement")
                          "else_clause")))
      parent-bol typescript-ts-mode-indent-offset)
     ,@(when (eq language 'tsx)
	 (append nil ;; (tsx-ts-mode--indent-compatibility-b893426)
		 `(((node-is "jsx_closing_element") parent 0)
		   ((match "jsx_element" "statement") parent typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_element") parent typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_text") parent-bol typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_opening_element") parent typescript-ts-mode-indent-offset)
		   ((parent-is "jsx_expression") parent-bol typescript-ts-mode-indent-offset)
		   ((match "/" "jsx_self_closing_element") parent 0)
		   ((parent-is "jsx_self_closing_element") parent typescript-ts-mode-indent-offset))))
     ;; FIXME(Theo): This no-node catch-all should be removed.  When is it needed?
     (no-node parent-bol 0))))

(advice-add 'typescript-ts-mode--indent-rules :override #'h/typescript-ts-mode--indent-rules)

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (typescript-ts-mode . lsp)
;;   (tsx-ts-mode . lsp)
;;   :commands lsp
;;   :bind (:map lsp-signature-mode-map
;;               ("M-p" . nil)
;;               ("M-n" . nil))
;;   :config
;;   (lsp-register-custom-settings '(("typescript.format.indentSize" 2)))
;;   (keymap-local-set "C-h ." #'lsp-describe-thing-at-point)
;;   (keymap-local-set "M-n" #'flycheck-next-error)
;;   (keymap-local-set "M-p" #'flycheck-previous-error))

;; (use-package jsonrpc
;;   :config
;;   ;; Improve eglot performance
;;   (fset #'jsonrpc--log-event #'ignore))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(consult-line-start-from-top t)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("7e98dc1aa7f5db0557691da690c38d55e83ddd33c6d268205d66e430d57fb982"
     default))
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(lsp-completion-provider :none)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-modeline-code-action-icons-enable nil)
 '(lsp-modeline-code-actions-enable nil)
 '(lsp-modeline-diagnostics-enable nil)
 '(lsp-modeline-workspace-status-enable nil)
 '(lsp-signature-doc-lines 5)
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
 '(make-backup-files nil)
 '(package-selected-packages
   '(apheleia consult corfu embark embark-consult expreg flymake-eslint
              gnu-elpa-keyring-update magit markdown-mode modus-themes
              orderless vertico wgrep))
 '(project-vc-extra-root-markers '("package.json"))
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaspace Neon Frozen" :foundry "outline" :slant normal :weight regular :height 113 :width normal)))))
