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

;; Función para actualizar la configuración de Emacs
(defun my/update-config ()
  "Update Emacs configuration in Git and push to GitHub.
This function:
1. Shows current Git status
2. Asks for files to add
3. Creates a commit with a message
4. Pushes to GitHub"
  (interactive)
  (let ((default-directory (expand-file-name user-emacs-directory))
        (changes-exist nil))
    
    ;; Verificar si hay cambios
    (when (magit-anything-modified-p)
      (setq changes-exist t)
      
      ;; Mostrar estado actual
      (magit-status-setup-buffer default-directory)
      
      ;; Preguntar si queremos hacer stage de todos los cambios
      (when (y-or-n-p "¿Hacer stage de todos los cambios? ")
        (magit-stage-modified))
      
      ;; Si no elegimos todos, preguntar archivo por archivo
      (unless (magit-staged-files)
        (dolist (file (magit-modified-files))
          (when (y-or-n-p (format "¿Hacer stage de %s? " file))
            (magit-stage-file file))))
      
      ;; Si hay archivos en stage, proceder con el commit
      (when (magit-staged-files)
        (let ((commit-msg (read-string "Mensaje del commit: ")))
          (magit-commit-create (list "-m" commit-msg))
          
          ;; Preguntar si queremos hacer push
          (when (y-or-n-p "¿Hacer push a GitHub? ")
            (magit-push-current-to-pushremote nil)))))
    
    ;; Mensaje si no hay cambios
    (unless changes-exist
      (message "No hay cambios para actualizar"))))

;; Bind the function to a key
(global-set-key (kbd "C-c u") #'my/update-config)

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
