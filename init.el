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

;; Start packages
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
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; configure UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

