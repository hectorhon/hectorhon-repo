(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(cc-search-directories
   (quote
    ("." "/usr/include" "/usr/local/include/*" "./include")))
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (dichromacy)))
 '(display-time-mode t)
 '(fci-rule-column 81)
 '(global-auto-revert-mode t)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "")
 '(isearch-allow-scroll t)
 '(line-spacing 1)
 '(make-backup-files nil)
 '(scroll-conservatively 2)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(slime-load-failed-fasl (quote never))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(font-lock-comment-face ((t (:foreground "#009e73" :slant normal)))))



;; startup directory
(setq default-directory "/home/hectorhon/quicklisp/local-projects/")


;; set colorcolumn=81
(add-to-list 'load-path "~/.emacs.d/common")
(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)



;; go to scratch buffer
(global-set-key (kbd "<f1>")
                (lambda ()
                  (interactive)
                  (switch-to-buffer "*scratch*")))



;; format code
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)))
(global-set-key (kbd "C-S-F") 'indent-buffer)



;; copy current file name
(defun copy-filename ()
  (interactive)
  (let ((filename (buffer-file-name (window-buffer (minibuffer-selected-window)))))
    (message filename)
    (kill-new filename)))
(global-set-key (kbd "C-c C-z") 'copy-filename)



;; melpa repository
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)



;; ;; set up slime
(setq inferior-lisp-program "/home/hectorhon/sbcl-1.4.14/bin/sbcl")
(setq slime-contribs '(slime-fancy))



;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")
        ("jinja2" . "\\.tmpl\\'")))
