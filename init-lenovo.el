;;; init-lenovo.el --- Emacs 30 on Lenovo G470  -*- lexical-binding: t; -*-

;;; Commentary:
;; Refactor del stack Lenovo para Emacs 30:
;; - robusto en terminal (-nw)
;; - conservador con dependencias externas
;; - compatible con Hyperbole 9.0.1 estable

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Inicio y defaults
;; ---------------------------------------------------------------------------

(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 2 1024 1024)
         gc-cons-percentage 0.1)))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      use-dialog-box nil
      create-lockfiles nil
      make-backup-files t
      auto-save-default nil
      browse-url-browser-function 'browse-url-default-browser)

(delete-selection-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(column-number-mode 1)

;; ---------------------------------------------------------------------------
;; 2. Paquetes y helpers
;; ---------------------------------------------------------------------------

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defun lenovo/executable-p (program)
  "Return non-nil when PROGRAM exists in PATH."
  (and (stringp program) (executable-find program)))

(defun lenovo/message (fmt &rest args)
  "Emit a startup-safe message using FMT and ARGS."
  (apply #'message (concat "[lenovo-init] " fmt) args))

;; ---------------------------------------------------------------------------
;; 3. Integración de sistema
;; ---------------------------------------------------------------------------

(require 'server)

(unless (server-running-p)
  (condition-case err
      (server-start)
    (file-error
     (lenovo/message "No se pudo iniciar el servidor Emacs: %s"
                     (error-message-string err)))))

(if (lenovo/executable-p "nyxt-gtk")
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "nyxt-gtk")
  (lenovo/message "Nyxt-gtk no está disponible; se usa browse-url por defecto"))

;; ---------------------------------------------------------------------------
;; 4. Navegación y completado moderno
;; ---------------------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (orderless basic))))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)))

(use-package embark
  :bind
  (("C-." . embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; ---------------------------------------------------------------------------
;; 5. Hyperbole estable (9.0.1)
;; ---------------------------------------------------------------------------

(use-package hyperbole
  :demand t
  :init
  ;; Hyperbole 9.0.1 usa hbmap:dir-user; HyWiki no está disponible aquí.
  (setq hbmap:dir-user (expand-file-name "~/.hyperb/"))
  (unless (file-directory-p hbmap:dir-user)
    (make-directory hbmap:dir-user t))
  :config
  (setq hsys-org-enable-smart-keys t)
  (hyperbole-mode 1))

;; ---------------------------------------------------------------------------
;; 6. Edición estructural y Lisp
;; ---------------------------------------------------------------------------

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode))

(use-package sly
  :if (lenovo/executable-p "sbcl")
  :custom
  (inferior-lisp-program "sbcl"))

(use-package geiser
  :if (lenovo/executable-p "racket")
  :config
  (setq geiser-active-implementations '(racket)))

(use-package geiser-racket
  :if (lenovo/executable-p "racket")
  :after geiser)

;; ---------------------------------------------------------------------------
;; 7. Proyectos, Git y terminal
;; ---------------------------------------------------------------------------

(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package vterm)

(use-package xclip
  :if (and (lenovo/executable-p "xclip") (getenv "DISPLAY"))
  :init
  (xclip-mode 1))

;; ---------------------------------------------------------------------------
;; 8. Suite documental y científica
;; ---------------------------------------------------------------------------

(use-package pdf-tools
  :if (file-exists-p (expand-file-name "~/.emacs.d/elpa/pdf-tools-1.3.0/server/epdfinfo"))
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :init
  (pdf-loader-install))

(use-package tex
  :ensure auctex
  :config
  (setq-default TeX-PDF-mode t))

(use-package org
  :config
  (require 'org-protocol)
  (setq org-capture-templates
        '(("w" "Web Link" entry (file+headline "~/org/refile.org" "Web Captures")
           "* %:description\n\n  Source: %:link\n  Captured on: %U\n\n  %:initial")))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-latex-pdf-process
        '("latexmk -f -pdf -pdflatex='pdflatex -shell-escape' -interaction=nonstopmode -output-directory=%o %f")))

;; ---------------------------------------------------------------------------
;; 9. Funciones propias
;; ---------------------------------------------------------------------------

(defun my/update-config ()
  "Abrir Magit en ~/.emacs.d para sincronizar la configuración."
  (interactive)
  (let ((emacs-config-dir (expand-file-name "~/.emacs.d/")))
    (save-some-buffers t)
    (magit-status emacs-config-dir)
    (message "Usa Magit para stage (s), commit (cc), y push (Pp).")))

;; ---------------------------------------------------------------------------
;; 10. Estética
;; ---------------------------------------------------------------------------

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-core-style 'overlays)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
