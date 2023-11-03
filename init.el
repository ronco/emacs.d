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
(straight-use-package 'counsel)
(straight-use-package 'move-text)
(straight-use-package 'projectile)

;; Start packages
(load-theme 'zenburn t)
(require 'uniquify)
(require 'saveplace)
(smartparens-global-mode)
(company-mode)
(exec-path-from-shell-initialize)
(ivy-mode)

;; move-text
(move-text-default-bindings)
(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)

;; projectile
(require 'projectile)
(global-unset-key (kbd "s-p"))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)

;; copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (company--active-p)))

(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

(defvar rk/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)

(defun rk/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      ;; (company-yasnippet-or-completion)
      (indent-for-tab-command)))

;; (define-key global-map (kbd "<tab>") #'rk/copilot-tab)

(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)
(global-copilot-mode)

;; magit
(add-hook 'magit-mode-hook
          (lambda () (local-set-key (kbd "<tab>") #'magit-section-toggle)))



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

(setq-default tab-width 2)

;; configure UI
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

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

(global-set-key (kbd "C-x C-r") 'counsel-recentf)

;; file modes
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
