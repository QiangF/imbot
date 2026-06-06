;;; imbot.el --- Emacs input method  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiang Fang

;; Author: Qiang Fang
;; Keywords: convenience, input method, dbus
;; Homepage: https://github.com/QiangF/imbot
;; Created: July 24th, 2020
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 4.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; imbot provide an emacs input method frontend for fcitx5 via dbus
;; usage:
;; (require 'imbot)
;; (setq default-input-method "imbot")

;;; Code:

(require 'seq)
(require 'dash)
(require 'posframe)
(require 'isearch)

(defgroup imbot nil
  "imbot is a smart input method"
  :group 'imbot)

(defvar imbot-backend 'backend-fcitx-dbus
  "definition for imbot-backend functions")

(require `,imbot-backend)

(defun imbot--predicate-program-mode-p ()
  (when (derived-mode-p 'prog-mode 'conf-mode)
    ;; point in comment or string
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

;; use shape only, eink display theme friendly
(defvar imbot--active-cursor '(hbar . 4))
(defvar imbot--inactive-cursor '(bar . 4))
(defvar imbot--inline-cursor '(hollow . 4)
  "Inline english cursor.")
(setq-default cursor-type imbot--inactive-cursor)
;; (setq-default cursor-in-non-selected-windows 'hollow)

(defvar imbot--commit nil "Holds string committed by D-Bus signal.")
(defvar imbot--tooltip nil "Holds UI payload from Fcitx5 UI updates.")

(defvar imbot--overlay nil
  "Inline english overlay.")

(defun imbot--delete-overlay ()
  (delete-overlay imbot--overlay)
  (setq imbot--overlay nil))

(defface imbot--inline-face '((t (:weight bold :box nil :inverse-video nil)))
  "Face to show inline english (input method temperarily disabled) is active.")

(defun imbot--predicate-english-context-p ()
  "Return t if English should be inputed at cursor point."
  ;; (message "real this command %s" real-this-command)
  (unless (or (eq real-this-command 'imbot--english-inline-deactivate)
              (eq real-this-command 'imbot--english-inline-quit)
              (eq real-this-command 'toggle-input-method)
              (eq major-mode 'mistty-mode))
    (let* ((visual-line-beginning (line-beginning-position))
           (point (point))
           (overlay-active (overlayp imbot--overlay))
           (english-context
            (or
             ;; switch to English when one space follows Chinse character
             ;; \cC represents any character of category “C”, according to “M-x describe-categories”
             (looking-back "\\cc " (max visual-line-beginning (- point 2)))
             (string-match "^\\s-*[0-9]+$" (buffer-substring-no-properties visual-line-beginning point))
             ;; (looking-at-p "^\\*")    ; org heading
             (looking-back "[a-zA-Z\\_-]" (max visual-line-beginning (1- point))))))
      (if overlay-active
          (if english-context
              (progn (move-overlay imbot--overlay visual-line-beginning (line-end-position))
                     (message "Activate input method with [return]. Quit with [C-g]"))
            (imbot--delete-overlay))
        (when english-context
          (setq imbot--overlay (make-overlay visual-line-beginning (line-end-position) nil t t))
          (overlay-put imbot--overlay 'priority 900)
          (overlay-put imbot--overlay
                       'face 'imbot--inline-face)
          (overlay-put imbot--overlay
                       'keymap (let ((keymap (make-sparse-keymap)))
                                 (define-key keymap (kbd "C-\\")
                                             #'imbot--english-inline-quit)
                                 (define-key keymap (kbd "C-g")
                                             #'imbot--english-inline-quit)
                                 (define-key keymap (kbd "RET")
                                             #'imbot--english-inline-deactivate)
                                 (define-key keymap (kbd "<return>")
                                             #'imbot--english-inline-deactivate)
                                 keymap))))
      english-context)))

(defun imbot--english-inline-deactivate ()
  "Deactivate the inline english overlay."
  (interactive)
  (when (overlayp imbot--overlay)
    (imbot--delete-overlay)
    (setq cursor-type imbot--active-cursor))
  (setq imbot--suppressed nil))

(defun imbot--english-inline-quit ()
  "Quit the inline english overlay."
  (interactive)
  (when imbot--overlay
    (imbot--delete-overlay)
    (imbot--deactivate)
    (setq input-method-function nil
          current-input-method nil
          current-input-method-title nil)))

(defvar imbot--disable-predicates
  '(imbot--predicate-english-context-p
    imbot--predicate-program-mode-p))

(defvar imbot--suppressed nil)
(make-variable-buffer-local 'imbot--suppressed)

(defun imbot--suppress-check (&optional force)
  (when (equal input-method-function 'imbot-input-method)
    (let ((suppressed (or (string-match " *temp*" (buffer-name))
                          (seq-find 'funcall imbot--disable-predicates nil))))
      ;; only run on value change
      (when (or (not (equal imbot--suppressed suppressed))
                force)
        (if suppressed
            (setq cursor-type imbot--inline-cursor)
          (if input-method-function
              (setq cursor-type imbot--active-cursor)
            (setq cursor-type imbot--inactive-cursor))
          ;; Another special face is the cursor face.
          ;; On graphical displays, the background color of this face is used to draw the text cursor.
          ;; None of the other attributes of this face have any effect.
          ;; As the foreground color for text under the cursor is taken from the background color of the underlying text.
          ;; On text terminals, the appearance of the text cursor is determined by the terminal, not by the cursor face.
          (custom-set-faces
           '(cursor ((t (:inherit font-lock-keyword-face))))))
        (redisplay t))
      (setq imbot--suppressed suppressed))))

(defun imbot--text-read-only-p ()
  "Return t if the text at point is read-only."
  ;; EWW: when a readonly buffer is readonly，it may still have modifiable text input field
  (and (or buffer-read-only
           ;; (get-pos-property (point) 'read-only)
           (and (get-char-property (point) 'read-only)
                (get-char-property (point) 'front-sticky)))
       (not (or inhibit-read-only
                (get-char-property (point) 'inhibit-read-only)))))

(defface imbot--tooltip-face
  '((((background light)) :background "#bfffff") (t :background "#400000"))
  "Face with a (presumably) dimmed background for popup.")

(defvar imbot--posframe-buffer " *imbot-posframe*"
  "The buffer name for candidate posframe.")

;; https://github.com/tumashu/vertico-posframe
(require 'eieio)
(defun imbot-posframe-refposhandler (&optional frame)
  "The default posframe refposhandler used by vertico-posframe.
Optional argument FRAME ."
  (cond
   ;; EXWM environment
   ((bound-and-true-p exwm--connection)
    (or (ignore-errors
          (let ((info (elt exwm-workspace--workareas
                           exwm-workspace-current-index)))
            (cons (oref info x)
                  (oref info y))))
        ;; Need user install xwininfo.
        (ignore-errors
          (posframe-refposhandler-xwininfo frame))
        ;; Fallback, this value will incorrect sometime, for example: user
        ;; have panel.
        (cons 0 0)))
   (t nil)))

(defun imbot--finish (&optional commit)
  (and imbot--posframe-buffer
       (posframe-hide imbot--posframe-buffer))
  ;; flag for exiting the translation loop
  (setq imbot--commit (or commit ""))
  (setq imbot--tooltip nil))

(defun imbot--activate (&optional _name)
  (unless buffer-read-only
    (setq-local input-method-function 'imbot-input-method)
    (setq-local deactivate-current-input-method-function #'imbot--deactivate)
    (add-hook 'post-command-hook 'imbot--suppress-check)
    (advice-add 'keyboard-quit :after 'imbot--finish)
    (imbot-backend-activate)
    (add-hook 'kill-emacs-hook 'imbot-backend-cleanup)
    (setq cursor-type imbot--active-cursor)
    (imbot--suppress-check t)
    (redisplay t)))

(defun imbot--deactivate ()
  (remove-hook 'post-command-hook 'imbot--suppress-check)
  (advice-remove 'keyboard-quit 'imbot--finish)
  (imbot-backend-send-escape)
  (setq cursor-type imbot--inactive-cursor)
  (setq isearch-input-method-function nil)
  (redisplay t))

(defun imbot--special-p ()
  (if (bound-and-true-p worf-mode)
      (worf--special-p)
    (or (region-active-p)
        (looking-at outline-regexp))))

(defun imbot-set-unread-command-events (key &optional reset)
  "This function is a fork of `quail-add-unread-command-events'."
  (when reset
    (setq unread-command-events nil))
  (setq unread-command-events
        (if (characterp key)
            (cons (cons 'no-record key) unread-command-events)
          (append (cl-mapcan
                   (lambda (e)
                     (list (cons 'no-record e)))
                   (append key nil))
                  unread-command-events))))

(defvar imbot--commit nil)
(defvar imbot--tooltip nil)

(defun imbot-translate (key orig-buffer)
  "流程：
1. 使用函数 `read-key-sequence-vector' 得到 key-sequence
2. 使用函数 `lookup-key' 查询 `imbot--map' 中述 key-sequence 的命令。
3. 如果查询得到的命令是 self-insert-command 时，调用这个函数。
4. 这个函数最终会返回需要插入到 buffer 的字符串。
参考 elisp 手册相关章节:
1. Invoking the Input Method
2. Input Methods
3. Miscellaneous Event Input Features
4. Reading One Event"
  (if (integerp key)
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (inhibit-modification-hooks t)
             (inhibit-quit t)
             ;; Temporarily increase gc-cons-threshold to prevent possible lag
             (gc-cons-threshold (ceiling (* gc-cons-threshold 1.1)))
             (input-method-function nil)
             (input-method-use-echo-area nil))
        ;; preedit sometimes not empty
        (imbot-backend-clear-composition)
        (imbot-set-unread-command-events key)
        (setq imbot--commit nil)
        (while (not imbot--commit)
          ;; Note: read-key-sequence-vector guarantees a vector return type,
          ;; preventing type discrepancies and capturing fresh mid-loop keystrokes.
          ;; t as the fourth argument, return the raw keys even if this sequence isn't bound
          (let* ((seq-direct (read-key-sequence-vector nil nil nil t nil t))
                 ;; Extract the event directly from the fresh vector stream
                 (first (aref seq-direct 0))
                 ;; mouse click is a list (down-mouse-1 (#<window ...> ...))
                 ;; describing the window, coordinates, and timestamp of your click
                 ;; use with `arrayp` or `vectorp`, not `sequencep`
                 ;; vectorp on mouse click event is nil
                 (event (if (vectorp first) (aref first 0) first))
                 commit handled)
            ;; (message "seq-direct %s event %s" seq-direct event)
            (unless
                ;; (mouse-event-p (elt event 0)
                (sequencep event)
              (setq event (fcitx-translate-emacs-key event))
              ;; (notify (format "preedit %s" (nth 0 imbot--tooltip))
              ;;         (format "event base %s car event %s" (event-basic-type event) (car event)))
              ;; (sleep-for 3)
              ;; only handle a fixed number of keys, other keys should run normal command
              (when (car event)
                (setq handled (imbot-backend-process-key (car event) (cdr event))
                      commit imbot--commit))
              ;; preedit not empty
              (if (nth 0 imbot--tooltip)
                  (if handled
                      (progn
                        (posframe-show imbot--posframe-buffer
                                       :refposhandler 'imbot-posframe-refposhandler
                                       :background-color 'unspecified
                                       :foreground-color (face-attribute 'default :foreground)
                                       :border-width 1
                                       :border-color (face-attribute 'default :foreground)
                                       :left-fringe 5
                                       :right-fringe 5
                                       :y-pixel-offset 5
                                       :string (imbot-backend-format-tooltip))
                        ;; Force Emacs to render the child frame immediately before locking on the next read-key-sequence-vector
                        (redisplay))
                    ;; lookup keybinding and call corresponding command, while keep the translating loop
                    (let* ((binding (and (arrayp event)
                                         (key-binding event)))
                           (cmd (or (command-remapping binding) binding)))
                      (when (commandp cmd) (call-interactively cmd))))
                (unwind-protect
                    (if handled
                        ;; commit is still nil whenever composition is active
                        ;; either tooltip empty and commit non-empty, or tooltip non-empty commit nil
                        (imbot--finish (and commit
                                            (string-to-list commit)))
                      ;; Not in a composition: convert our cleanly captured vector
                      ;; to a list so Emacs can execute it normally outside the input method.
                      (imbot--finish (listify-key-sequence seq-direct)))
                  (unless (eq orig-buffer (current-buffer))
                    (imbot--finish)))))))
        imbot--commit)
    (unless (null key)
      (char-to-string key))))

;; ref quail-input-method
(defun imbot-input-method (key)
  "Process character KEY with input method, other keys not handled."
  (if (or imbot--suppressed
          ;; When an overriding keymap is active (e.g., `set-transient-map'
          ;; used by spatial-window, avy, etc.), pass the key through if
          ;; it has a binding there.  This matches quail's behavior per
          ;; Emacs bug#68338.
          (and overriding-terminal-local-map
               (lookup-key overriding-terminal-local-map (vector key)))
          overriding-local-map
          (imbot--special-p)
          (imbot--text-read-only-p))
      (list key)
    (with-silent-modifications
      (unwind-protect
          (let ((result (imbot-translate key (current-buffer))))
            (mapcar #'identity result))
        (imbot--finish)))))

(register-input-method "imbot" "euc-cn" 'imbot--activate "ㄓ" "smart input method")

(advice-add 'toggle-input-method :after 'posframe-hide-all)

(provide 'imbot)

;;; imbot.el ends here
