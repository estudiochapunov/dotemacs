;; -*- lexical-binding: t -*-

;;; init.el --- Configuración completa para Windows con Magit, Vertico, Corfu y más -*- lexical-binding: t -*-
;;; Commentary:
;; Configuración robusta para Windows con gestión de paquetes mejorada,
;; completado moderno, soporte para Lisp/Scheme/Clojure, Magit (con gestión de commits),
;; herramientas útiles, gestión de backups y guía de recuperación.

;;; Code:

;;; Optimizaciones tempranas de GC
(setq gc-cons-threshold (* 64 1000 1000))

;;; Package Management - Versión Robusta
(require 'package)

;; Configuración de repositorios incluyendo MELPA (HTTPS preferido)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Configuración de red (si tienes problemas de conexión)
(when (and (fboundp 'gnutls-available-p) (gnutls-available-p))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")) ; Desactivar TLS 1.3 si causa problemas
(setq network-security-level 'low)

;; Inicialización y refresco de paquetes con manejo de errores
(package-initialize)

(unless package-archive-contents
  (condition-case err
      (package-refresh-contents)
    (error (message "Error al refrescar los paquetes: %s" (error-message-string err)))))

;; Instalar use-package si no está presente
(unless (package-installed-p 'use-package)
  (condition-case nil
      (package-install 'use-package)
    (error (message "Error al instalar use-package"))))

(require 'use-package)
(setq use-package-always-ensure t) ; Activar para asegurar la instalación de paquetes

;;; Configuración básica de Emacs
(use-package emacs
  :init
  ;; Mejores defaults
  (setq-default
   inhibit-startup-screen t
   initial-scratch-message nil
   ring-bell-function 'ignore
   frame-resize-pixelwise t
   use-dialog-box nil
   sentence-end-double-space nil
   indent-tabs-mode nil
   create-lockfiles nil
   auto-save-default nil
   make-backup-files t ; Activar backups (¡importante!)
   help-window-select t
   enable-recursive-minibuffers t
   history-length 1000
   extended-command-suggest-shorter t)

  ;; Mejor experiencia de edición
  (delete-selection-mode 1)
  (global-auto-revert-mode 1)
  (show-paren-mode 1)
  (electric-pair-mode 1)
  (column-number-mode 1)
  (global-display-line-numbers-mode t) ; Números de línea activados por defecto

  ;; Performance
  (setq auto-mode-case-fold nil)
  (setq read-process-output-max (* 1024 1024))

  :config
    (setq default-buffer-file-coding-system 'utf-8-unix) ; Codificación UTF-8 por defecto
    (setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups/")))) ; Directorio de backups
)

;;; Tema built-in
(load-theme 'modus-operandi t)

;;; Transient y Magit (CON TU CÓDIGO ORIGINAL INTEGRO)
(use-package transient
  :ensure t
  :pin gnu)

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

;;; Stack moderno de completado
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(basic partial-completion orderless))
  (completion-category-overrides '((file (styles . (partial-completion basic)))))
  :init
  (setq completion-category-defaults nil))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

;; Configuración específica para Imenu
(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil)
  (imenu-max-item-length 100)
  (imenu-space-replacement " ")
  :config
  (add-hook 'emacs-lisp-mode-hook #'imenu-add-menubar-index))

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g o" . consult-outline) ; CORREGIDO
   ("M-s f" . consult-find)
   ("M-s r" . consult-ripgrep)
   ("M-g i" . consult-imenu)
   ("C-x r f" . consult-recent-files)
   ("C-x r b" . consult-recent-buffers)
   ("<f1> f" . consult-find)
   ("<f1> r" . consult-ripgrep))
  :custom
  (consult-narrow-key "<")) ; Carácter para estrechar las búsquedas

;;; Configuración para el desarrollo en Lisp
(use-package sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")) ; o tu implementación de Lisp preferida

;;; Configuración para el desarrollo en Scheme
(use-package geiser
  :ensure t
  :config
    (setq geiser-default-implementation 'guile)) ; o tu implementación de Scheme preferida

;;; Editor de código estructurado
(use-package paredit
  :ensure t
  :hook (prog-mode . enable-paredit-mode)) ; Activar paredit en todos los modos de programación

;;; Delimitadores de colores
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)) ; Activar rainbow-delimiters en modos de programación

;;; Configuración adicional (ejemplo: auto-complete-mode, si lo prefieres)
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (ac-config-default)) ; Ejemplo

;;; Guía de recuperación y backup de la configuración de Emacs
;;; (ESTA SECCIÓN ES MUY IMPORTANTE, ADÁPTALA A TU SISTEMA)
;; 1. Ubicación de la configuración: ~/.emacs.d/
;; 2. Crear un repositorio Git: Dentro de ~/.emacs.d/, ejecuta `git init`
;; 3. Agregar los archivos: `git add .`
;; 4. Commit inicial: `git commit -m "Initial Emacs config"`
;; 5. (Opcional) Subir a un repositorio remoto (GitHub, GitLab, etc.):
;;    - Crear un repositorio en tu proveedor Git.
;;    - Agregar el remoto: `git remote add origin <url_del_repositorio>`
;;    - Hacer push: `git push -u origin main` (o `master`)

;;; Para recuperar la configuración en otro equipo:
;; 1. Clonar el repositorio: `git clone <url_del_repositorio> ~/.emacs.d/`

;;; Backup manual (adicional al control de versiones con Git):
;;; Copiar la carpeta ~/.emacs.d/ a una ubicación segura (ej. un disco externo o la nube).

;;; Opcionalmente puedes agregar la siguiente linea para que custom.el guarde las variables elegidas por la interfaz de personalizacion
;;; en tu init.el en vez de crear un custom.el separado (no recomendado usualmente)
;;; (setq custom-file (locate-user-emacs-file "init.el"))

;;; Final del archivo
(provide 'init)

;;; No incluyo custom-set-variables ni custom-set-faces para evitar sobreescribir configuraciones del usuario al copiar y pegar.
;;; El usuario puede usar M-x customize para configurar opciones y se guardarán en su init.el si descomenta la linea de arriba o en un archivo custom.el separado.
