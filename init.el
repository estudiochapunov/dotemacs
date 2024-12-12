;; -*- lexical-binding: t -*-

;;; init.el --- Configuración simple para Windows -*- lexical-binding: t -*-
;;; Commentary:
;; Configuración minimalista con mejor manejo de errores

;;; Code:

;;; Optimizaciones tempranas de GC
(setq gc-cons-threshold (* 64 1000 1000))

;;; Package Management - Versión Robusta
(require 'package)

;; Configuración de repositorios incluyendo MELPA
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("nongnu" . "http://elpa.nongnu.org/nongnu/")
        ("melpa" . "http://melpa.org/packages/")))

;; Configuración de red para mejor compatibilidad
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq network-security-level 'low)

;; Inicialización segura de paquetes
(package-initialize)

;; Refrescar contenidos con manejo de errores
(condition-case err
    (unless package-archive-contents
      (package-refresh-contents))
  (error
   (message "Error refreshing package contents: %s" (error-message-string err))))

;; Instalar use-package si no está presente
(unless (package-installed-p 'use-package)
  (condition-case nil
      (package-install 'use-package)
    (error (message "Failed to install use-package"))))

(require 'use-package)
(setq use-package-always-ensure nil)  ; Desactivamos ensure por defecto

;;; Built-in configuraciones esenciales
(use-package emacs
  :init
  ;; Mejores defaults
  (setq-default
   inhibit-startup-screen t
   initial-scratch-message nil
   ring-bell-function 'ignore
   frame-resize-pixelwise t
   use-dialog-box nil)
  
  ;; Mejor experiencia de edición
  (delete-selection-mode 1)
  (global-auto-revert-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (column-number-mode 1))

;;; Tema built-in
(load-theme 'modus-operandi t)

;;; Transient y Magit
(use-package transient
  :ensure t
  :pin gnu)  ; Forzar uso de la versión GNU

(use-package magit
  :ensure t
  :after transient
  :bind (("C-x g" . magit-status))
  :config
  (when (eq system-type 'windows-nt)
    (when-let ((git-path (executable-find "git")))
      (setq magit-git-executable git-path))))

;;; Provide init
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
