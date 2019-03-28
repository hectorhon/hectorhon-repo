(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (dichromacy)))
 '(default-frame-alist (quote ((width . 80) (height . 36) (menu-bar-lines . 1))))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(global-auto-revert-mode t)
;; '(global-display-line-numbers-mode t) ; causes git annotate to crash
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(scroll-conservatively 101)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Lucida Console" :foundry "outline" :slant normal :weight normal :height 98 :width normal)))))

;; put this here to avoid git annotate crashing
(global-display-line-numbers-mode)



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
