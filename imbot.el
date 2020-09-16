;;; imbot.el --- Automatic system input method switcher -*- lexical-binding: t; -*-

;; URL: https://github.com/QiangF/imbot
;; Created: July 24th, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; Version: 3.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; imbot is inspired by https://github.com/laishulu/emacs-smart-input-source
;; usage:
;; 1. redefine these functions if you are not using fcitx-remote:
;; imbot--active-p, imbot--activate, imbot--deactivate
;; 2. maybe disable inline edit
;;  (delq 'imbot--english-p imbot--suppression-predicates)
;; 3. add imbot-mode to relevant startup hooks
;;   (add-hook 'evil-mode-hook 'imbot-mode)

;;; Code:

(require 'seq)

(defvar imbot--active-saved nil
  "Buffer local input method state, changes only at manual input method toggling.")

(make-variable-buffer-local 'imbot--active-saved)

(defvar imbot--active-checked nil
  "True input method state checked at pre-command-hook, is t whenever input method is active.")

;; For fcitx, the command is "fcitx-remote"
(defvar imbot-command "/usr/bin/ibus engine"
  "Input method management command.")

;; For fcitx, the tag is "2"
(defvar imbot-english-engine-tag "xkb:us::eng"
  "Tag for the english engine.")

;; For fcitx the switch is "-o"
(defvar imbot-input-activate-switch "rime"
  "Switch for the non english engine.")

;; For fcitx the switch is "-c"
(defvar imbot-input-deactivate-switch "xkb:us::eng"
  "Switch for the english engine.")

(defun imbot--active-p ()
  "Return t when input method is active (in non English state)."
  (not (string-equal (string-trim (shell-command-to-string imbot-command)) imbot-english-engine-tag)))

(defun imbot--activate ()
  "Set input method in non English state."
  (unless imbot--active-checked
    (setq imbot--active-checked t)
    (call-process-shell-command (concat imbot-command " " imbot-input-activate-switch))))

(defun imbot--deactivate ()
  "Set input method in English state."
  (when imbot--active-checked
    (setq imbot--active-checked nil)
    (call-process-shell-command (concat imbot-command " " imbot-input-deactivate-switch))))

(defun imbot--update-cursor ()
  "Set cursor color according to input method state."
  (if imbot--active-checked
      (set-cursor-color "green")
      (set-cursor-color "white")))

;; disable imbot-mode before looking up key definition start with imbot--prefix-override-keys
(defvar imbot--prefix-override-keys
  '("C-c" "C-x" "C-h" "<f1>")
  "Prefix keys not handled by input method, which disable input method temperarily.")

(defvar imbot--prefix-override-map-alist nil
  "An `emulation-mode-map-alists keymap.")

(let ((keymap (make-sparse-keymap)))
  (dolist (prefix
           imbot--prefix-override-keys)
    (define-key keymap (kbd prefix)
      #'imbot--prefix-override-handler))
  (setq imbot--prefix-override-map-alist
        `((imbot--active-checked . ,keymap))))

(defun imbot--prefix-override-add (&optional _args)
  "Setup `emulation-mode-map-alist."
  (add-to-list 'emulation-mode-map-alists 'imbot--prefix-override-map-alist))

(defun imbot--prefix-override-remove (&optional _args)
  "Unset `emulation-mode-map-alist."
  (setq emulation-mode-map-alists
        (delq 'imbot--prefix-override-map-alist emulation-mode-map-alists)))

(defvar imbot--prefix-reinstate-triggers
  '(evil-local-mode yas-minor-mode eaf-mode)
  "Handle modes that mess `emulation-mode-map-alists.")

(defun imbot--prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  (let* ((keys (this-command-keys)))
    (imbot--deactivate)
    ;; Restore the prefix arg
    (setq prefix-arg arg)
    (setq last-command 'imbot--prefix-override-handler)
    (prefix-command-preserve-state)
    ;; Push the key back on the event queue
    (setq unread-command-events
          (append (mapcar (lambda (e) `(t . ,e)) (listify-key-sequence keys))
                  unread-command-events))))

(defvar imbot--overlay nil
  "Inline editing overlay.")

(defface imbot--inline-face '()
  "Face to show inline editing (input method temperarily disabled) is active."
  :group 'imbot)

(set-face-attribute
 'imbot--inline-face nil
 :foreground (face-attribute 'font-lock-constant-face :foreground)
 :inverse-video t)

(defun imbot--english-region-p ()
  "Buffer is in `prog-mode or `conf-mode, and buffer string is not in a string or comment."
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun imbot--english-context-p ()
  "Return t if English should be inputed at cursor point."
  (let ((line-beginning (line-beginning-position))
        (point (point)))
    (or
     ;; 中文后面紧接1个空格切换到英文输入
     ;; \cC represents any character of category “C”, according to “M-x describe-categories”
     (looking-back "\\cc " (max line-beginning (- point 2)))
     (string-match "^\\s-*[0-9]+$" (buffer-substring-no-properties line-beginning point))
     (looking-back "[a-zA-Z\\-\\*]" (max line-beginning (1- point))))))

(defun imbot--english-p ()
  "Check context."
  (when imbot--active-saved
    (let ((english-context (imbot--english-context-p))
          (english-region (imbot--english-region-p)))
      (when (overlayp imbot--overlay)
        (delete-overlay imbot--overlay)
        (setq imbot--overlay nil))
      (when (imbot--english-context-p)
        (unless english-region
          (setq imbot--overlay (make-overlay (line-beginning-position) (line-end-position) nil t t ))
          (overlay-put imbot--overlay 'face 'imbot--inline-face)))
      (or english-context english-region))))

(defvar evil-normal-state-minor-mode)

(defvar evil-visual-state-minor-mode)

(defvar evil-motion-state-minor-mode)

(defvar god-local-mode)

(defvar hydra-curr-map)

(defvar imbot--suppression-watch-list
  '(evil-normal-state-minor-mode
    evil-visual-state-minor-mode
    evil-motion-state-minor-mode
    god-local-mode
    hydra-curr-map)
  "Enable suppression if any variables in this list is t.")

(defun imbot--prefix-override-p ()
  "This-command becomes non nil after prefix sequence completion."
  (or (equal last-command 'imbot--prefix-override-handler)
      (equal real-this-command 'imbot--prefix-override-handler)))

(defvar imbot--suppression-predicates
  (list #'imbot--english-p #'imbot--prefix-override-p)
  "Conditions in which input method should be suppressed, in order of priority.")

(defvar imbot--suppressed nil
  "Buffer local suppression state.")

(make-variable-buffer-local 'imbot--suppressed)

(defun imbot--pre-command-hook ()
  "Update imbot--active-checked in case input state is toggled not via Emacs."
  (setq imbot--active-checked (imbot--active-p))
  (unless imbot--suppressed
    (setq imbot--active-saved imbot--active-checked)))

(defun imbot--post-command-hook ()
  "The main input state processor."
  ;; When an editing command returns to the editor command loop, the buffer is still the original
  ;; buffer, buffer change after Emacs automatically calls set-buffer on the buffer shown in the
  ;; selected window.
  (run-with-timer 0 nil
                  (lambda ()
                    (if (or (eval `(or ,@imbot--suppression-watch-list))
                            (seq-find 'funcall imbot--suppression-predicates nil))
                        (progn (setq imbot--suppressed t)
                               (imbot--deactivate))
                        ;; restore input state
                        (if imbot--active-saved
                            (imbot--activate)
                            (imbot--deactivate))
                        (setq imbot--suppressed nil))
                    (imbot--update-cursor))))

(defun imbot--hook-handler (add-or-remove)
  "Setup hooks, ADD-OR-REMOVE."
  (funcall add-or-remove 'pre-command-hook #'imbot--pre-command-hook)
  (funcall add-or-remove 'focus-out-hook #'imbot--pre-command-hook)
  (funcall add-or-remove 'post-command-hook #'imbot--post-command-hook)
  (funcall add-or-remove 'focus-in-hook #'imbot--post-command-hook))

;;;###autoload
(define-minor-mode imbot-mode
  "Input method managing bot."
  :global t
  :init-value nil
  (if imbot-mode
      (progn
        (imbot--hook-handler 'add-hook)
        (imbot--prefix-override-add)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-add trigger :after #'imbot--prefix-override-add)))
      (imbot--hook-handler 'remove-hook)
      (imbot--prefix-override-remove)
      (dolist (trigger imbot--prefix-reinstate-triggers)
        (advice-remove trigger #'imbot--prefix-override-add))))

(provide 'imbot)
;;; imbot.el ends here
