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

(defun my/update-config ()
  "Función interactiva para actualizar la configuración de Emacs.
Realiza los siguientes pasos:
1. Verifica si hay cambios
2. Permite seleccionar archivos a agregar
3. Solicita un mensaje de commit
4. Realiza commit y push"
  (interactive)
  (require 'magit nil t)  ; Cargar Magit de manera segura
  
  ;; Definir el directorio de configuración de Emacs de manera compatible
  (let ((emacs-config-dir 
         (or (and (boundp 'user-emacs-directory) user-emacs-directory)
             (expand-file-name "~/.emacs.d/"))))
    
    ;; Verificar si Magit está disponible
    (if (not (fboundp 'magit-status))
        (error "Magit no está instalado. Por favor, instala Magit primero.")
      
      ;; Cambiar al directorio de configuración de Emacs
      (cd emacs-config-dir)
      
      ;; Verificar si es un repositorio Git
      (if (not (file-exists-p (expand-file-name ".git" emacs-config-dir)))
          (error "El directorio de configuración no es un repositorio Git. Inicializa git primero.")
        
        ;; Mostrar estado de Git
        (magit-status emacs-config-dir)
        
        ;; Preguntar al usuario si quiere continuar
        (when (y-or-n-p "¿Deseas actualizar tu configuración de Emacs? ")
          ;; Solicitar mensaje de commit
          (let ((commit-message 
                 (read-string "Describe los cambios (mensaje de commit): ")))
            
            ;; Agregar todos los archivos modificados
            (when (y-or-n-p "¿Quieres agregar todos los archivos modificados? ")
              (magit-stage-modified t))
            
            ;; Crear commit
            (magit-commit-create 
             (list "-m" commit-message))
            
            ;; Hacer push
            (when (y-or-n-p "¿Deseas hacer push a GitHub? ")
              (magit-push-current-to-pushremote nil))
            
            (message "Configuración actualizada exitosamente.")))))))

;; Atajo de teclado opcional
(global-set-key (kbd "C-c u") 'my/update-config)

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
