;; Load packages
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'magit)
(straight-use-package 'company)
(straight-use-package 'smartparens)
(straight-use-package 'diminish)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'ivy)
(straight-use-package 'org)
(straight-use-package 'php-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'zenburn-theme)

;; Start packages
(load-theme 'zenburn t)
(require 'uniquify)
(require 'saveplace)
(smartparens-global-mode)
(company-mode)
(exec-path-from-shell-initialize)
(ivy-mode)


;; Diminish as necessary
(diminish 'smartparens-mode)
(diminish 'company-mode)
(diminish 'ivy-mode)

;; configure packages
(setq uniquify-buffer-name-style 'forward)
(setq-default save-place t)
(setq-default indent-tabs-mode nil)

;; variables
(setq select-enable-clipboard t
      select-enable-primary t
      tab-width 2
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; configure UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; custom functions
(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

;; key bindings

(global-set-key (kbd "s-]") (lambda ()
                              (interactive)
                              (shift-right tab-width)))
(global-set-key (kbd "s-[") (lambda ()
                              (interactive)
                              (shift-left tab-width)))

