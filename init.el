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

;; Start packages
(smartparens-global-mode)
(company-mode)
(exec-path-from-shell-initialize)
(ivy-mode)
;; Diminish as necessary
(diminish 'smartparens-mode)
(diminish 'company-mode)
