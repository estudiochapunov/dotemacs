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

;; Nyxt queda como destino de `browse-url`; EWW se conserva como alternativa
;; interna, sin depender de la integración Slynk/StumpWM de Gigafactory.
(defun lenovo/browse-url-eww (url &optional new-window)
  "Open URL in EWW, optionally using NEW-WINDOW.
Interactively, use the URL at point or prompt for one."
  (interactive
   (list (or (browse-url-url-at-point)
             (read-string "URL para EWW: "))
         current-prefix-arg))
  (eww-browse-url url new-window))

(with-eval-after-load 'eww
  (setq eww-search-prefix "https://duckduckgo.com/html/?q="))

(global-set-key (kbd "C-c w e") #'lenovo/browse-url-eww)

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

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

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
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-annot-activate-created-annotations t)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))
  (defun lenovo/pdf-view-as-text ()
    "Show the current PDF as read-only text produced by pdftotext."
    (interactive)
    (unless (and buffer-file-name (lenovo/executable-p "pdftotext"))
      (user-error "El PDF no tiene archivo asociado o falta pdftotext"))
    (let ((source buffer-file-name)
          (buffer (get-buffer-create
                   (format "*PDF texto: %s*"
                           (file-name-nondirectory buffer-file-name)))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (zerop (call-process "pdftotext" nil t nil
                                       "-layout" source "-"))
            (error "pdftotext no pudo procesar %s" source))
          (text-mode)
          (visual-line-mode 1)
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (view-mode 1)))
      (pop-to-buffer buffer)))
  :bind
  (:map pdf-view-mode-map
        ("C-c t" . lenovo/pdf-view-as-text)))

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

;; HTML semántico con EWW y preview visual estático con wkhtmltoimage.
(with-eval-after-load 'eww
  (defun lenovo/eww-open-in-nyxt ()
    "Open the current EWW URL in the configured external browser (Nyxt)."
    (interactive)
    (unless (and (boundp 'eww-current-url) eww-current-url)
      (user-error "EWW no tiene una URL activa"))
    (browse-url-generic eww-current-url))
  (define-key eww-mode-map (kbd "C-c w n") #'lenovo/eww-open-in-nyxt))

(defvar-local lenovo/html-preview-source nil)
(defvar-local lenovo/html-preview-output nil)

(defun lenovo/html-preview-refresh ()
  "Regenerate the current HTML preview from its source file."
  (interactive)
  (unless (and lenovo/html-preview-source
               (file-readable-p lenovo/html-preview-source))
    (user-error "Este buffer no tiene una fuente HTML legible"))
  (lenovo/html-preview-image lenovo/html-preview-source))

(defun lenovo/html-preview-image (file)
  "Render HTML FILE with wkhtmltoimage and display it in an Emacs buffer."
  (interactive
   (list (read-file-name "HTML para previsualizar: " nil buffer-file-name t)))
  (unless (lenovo/executable-p "wkhtmltoimage")
    (user-error "wkhtmltoimage no está disponible"))
  (setq file (expand-file-name file))
  (unless (file-readable-p file)
    (user-error "No se puede leer %s" file))
  (let* ((output (make-temp-file "emacs-html-preview-" nil ".png"))
         (buffer (get-buffer-create
                  (format "*HTML preview: %s*" (file-name-nondirectory file))))
         (status (call-process "wkhtmltoimage" nil nil nil
                               "--quiet" "--enable-local-file-access"
                               file output)))
    (unless (zerop status)
      (delete-file output)
      (error "wkhtmltoimage terminó con estado %s" status))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (create-image output 'png nil :max-width (window-pixel-width)))
        (image-mode)
        (when (and lenovo/html-preview-output
                   (file-exists-p lenovo/html-preview-output))
          (delete-file lenovo/html-preview-output))
        (setq-local lenovo/html-preview-source file
                    lenovo/html-preview-output output)
        (local-set-key (kbd "g") #'lenovo/html-preview-refresh)
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (and lenovo/html-preview-output
                               (file-exists-p lenovo/html-preview-output))
                      (delete-file lenovo/html-preview-output)))
                  nil t)))
    (pop-to-buffer buffer)))

(global-set-key (kbd "C-c w p") #'lenovo/html-preview-image)

;; IA nativa en Emacs mediante OpenRouter. La clave se reutiliza desde el
;; almacén privado de OpenCode y nunca se duplica en esta configuración.
(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :init
  (setq gptel-default-mode 'org-mode)
  :config
  (defun lenovo/read-api-key-file (name)
    "Read API key NAME from Lenovo's private Emacs secrets directory."
    (let ((file (expand-file-name
                 (format "~/.config/emacs/secrets/%s.key" name))))
      (if (file-readable-p file)
          (string-trim
           (with-temp-buffer
             (insert-file-contents file)
             (buffer-string)))
        (user-error "No se puede leer %s" file))))
  (defun lenovo/openrouter-api-key ()
    "Return OpenRouter API key from the dedicated private file."
    (lenovo/read-api-key-file "openrouter"))
  (defun lenovo/opencode-go-api-key ()
    "Return OpenCode Go API key from the dedicated private file."
    (lenovo/read-api-key-file "opencode-go"))
  (setq lenovo/gptel-openrouter
        (gptel-make-openai "OpenRouter Lenovo"
          :host "openrouter.ai/api"
          :endpoint "/v1/chat/completions"
          :key #'lenovo/openrouter-api-key
          :stream t
          :models '(openrouter/auto
                    anthropic/claude-sonnet-5
                    x-ai/grok-4.3
                    google/gemini-3.5-flash
                    google/gemini-2.5-flash)))
  (setq lenovo/gptel-opencode-go
        (gptel-make-openai "OpenCode Go"
          :host "opencode.ai/zen/go"
          :endpoint "/v1/chat/completions"
          :key #'lenovo/opencode-go-api-key
          :stream t
          :models '(deepseek-v4-pro
                    deepseek-v4-flash
                    glm-5.2
                    kimi-k2.7-code
                    mimo-v2.5-pro)))
  (setq gptel-backend lenovo/gptel-openrouter
        gptel-model 'openrouter/auto
        gptel-directives
        '((default . "Sos un asistente técnico para la Lenovo Linux Mint de Gabriel. Respondé con claridad, preservá el contenido del usuario y no hagas cambios destructivos sin confirmación.")
          (programmer . "Actuá como ingeniero de software. Producí soluciones verificables y código conciso.")))
  (defun lenovo/gptel-use-openrouter ()
    "Select OpenRouter with Auto Router as the active gptel backend."
    (interactive)
    (setq gptel-backend lenovo/gptel-openrouter
          gptel-model 'openrouter/auto)
    (message "gptel: OpenRouter / Auto Router"))
  (defun lenovo/gptel-use-opencode-go (&optional flash)
    "Select OpenCode Go with DeepSeek V4 Pro, or Flash with prefix FLASH."
    (interactive "P")
    (setq gptel-backend lenovo/gptel-opencode-go
          gptel-model (if flash 'deepseek-v4-flash 'deepseek-v4-pro))
    (message "gptel: OpenCode Go / %s" gptel-model))
  :bind
  (("C-c g g" . gptel)
   ("C-c g s" . gptel-send)
   ("C-c g m" . gptel-menu)
   ("C-c g r" . lenovo/gptel-use-openrouter)
   ("C-c g o" . lenovo/gptel-use-opencode-go)))

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

;; Launcher de flavors locales de Nyxt: lanza el proceso externo, apunta
;; browse-url a ese binario, y conecta Sly al puerto Slynk correspondiente
;; en cuanto el REPL responde. Puertos fijos por flavor (ver
;; Lenovo-sysadmin/documentaciones/BITACORA_CONTEXTO_SISTEMA.md, entrada
;; #239 y scripts/nyxt-ia): estable = 4006, ia = 4007.
(defvar lenovo/nyxt-flavors
  '(("estable" . (:command "nyxt-gtk"
                   :args ("-e" "(nyxt::start-slynk)")
                   :env (("WEBKIT_DISABLE_SECRET_STORAGE" . "1")
                         ("WEBKIT_DISABLE_DMABUF_RENDERER" . "1"))
                   :slynk-port 4006))
    ("ia (laboratorio)" . (:command "nyxt-ia"
                            :args ()
                            :env ()
                            :slynk-port 4007)))
  "Flavors de Nyxt lanzables desde `lenovo/nyxt-launch', con su comando,
argumentos, variables de entorno y puerto Slynk.")

(defun lenovo/nyxt--slynk-port-open-p (port)
  "Return non-nil if something is already listening on 127.0.0.1:PORT."
  (condition-case nil
      (let ((proc (open-network-stream "lenovo-nyxt-probe" nil "127.0.0.1" port)))
        (delete-process proc)
        t)
    (error nil)))

(defun lenovo/nyxt--wait-and-connect (port tries)
  "Poll PORT for Slynk and run `sly-connect' once it answers.
Retries TRIES times, once per second, then gives up with a message."
  (cond
   ((lenovo/nyxt--slynk-port-open-p port)
    (lenovo/message "Slynk arriba en :%d, conectando Sly..." port)
    (sly-connect "localhost" port))
   ((> tries 0)
    (run-with-timer 1 nil #'lenovo/nyxt--wait-and-connect port (1- tries)))
   (t
    (lenovo/message "Slynk no respondió en :%d; conectá a mano con M-x sly-connect" port))))

(defun lenovo/nyxt-launch (flavor)
  "Launch a local Nyxt FLAVOR, point browse-url at it, and connect Sly.
FLAVOR is a key of `lenovo/nyxt-flavors'."
  (interactive
   (list (completing-read "Flavor de Nyxt: " (mapcar #'car lenovo/nyxt-flavors) nil t)))
  (let* ((spec (cdr (assoc flavor lenovo/nyxt-flavors)))
         (command (plist-get spec :command))
         (args (plist-get spec :args))
         (env (plist-get spec :env))
         (port (plist-get spec :slynk-port))
         (process-environment
          (append (mapcar (lambda (kv) (format "%s=%s" (car kv) (cdr kv))) env)
                  process-environment)))
    (unless (executable-find command)
      (user-error "No se encontró %s en PATH" command))
    (apply #'start-process (format "nyxt-%s" flavor) nil command args)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program command)
    (lenovo/message "Lanzando Nyxt (%s) vía %s; browse-url ahora apunta ahí; esperando Slynk en :%d..."
                     flavor command port)
    (lenovo/nyxt--wait-and-connect port 20)))

(global-set-key (kbd "C-c w N") #'lenovo/nyxt-launch)

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
