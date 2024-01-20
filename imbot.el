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
;; emacs uses xim, make sure environment variables are set correctly, in case of ibus, eg:
;; export XIM="ibus"
;; export XIM_PROGRAM="ibus"
;; ibus is slow in restoring application im state, make sure to share ibus state in all apllications 
;; 1. redefine these functions according to your input method manager:
;;    imbot--active-p, imbot--activate, imbot--deactivate
;; 2. disable inline english with:
;;    (delq 'imbot--english-p imbot--suppression-predicates)
;; 3. the key to exit inline english is return
;; 4. add imbot-mode to relevant startup hooks, eg:
;;    (add-hook 'afte-init-hook 'imbot-mode)

;;; Code:

(require 'seq)

(defvar imbot--active-saved nil
  "Buffer local input method state, changes only at manual input method toggling.")

(make-variable-buffer-local 'imbot--active-saved)

(defvar imbot--active-checked nil
  "True input method state checked at pre-command-hook, is t whenever input method is active.")

(defun imbot--track-state (hint)
  (message "buffer: %s saved: %s checked: %s, %s"
           (current-buffer) imbot--active-saved imbot--active-checked hint))

(defvar imbot--im-config (if (and (string= (window-system) "w32")
                                  (fboundp 'w32-get-ime-open-status))
                             'imbot--windows
                           'imbot--fcitx5)
  "User config file with the definition of `imbot--active-p, `imbot--activate, `imbot--deactivate.")

;; check im method status in focus in and focus out hook, if the input method status per application not set
(defvar imbot--active-omit-check t
  "Omit setting imbot--active-checked when t, check on every command when nil.

Checking at every command execution may cause the pre-command-function removed from pre-command-hook
due to possible errors, so setting with emacsclient on im state change via hotkey outside emacs
may be a better solution.")

(require `,imbot--im-config)

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
  (dolist (prefix imbot--prefix-override-keys)
    (define-key keymap (kbd prefix)
                #'imbot--prefix-override-handler))
  ;; better to set imbot--active-checked set for the window manager with an input method toggle shortcut
  (setq imbot--prefix-override-map-alist (if imbot--active-omit-check
                                             `((imbot--active-checked . ,keymap))
                                           `((t . ,keymap)))))

(defun imbot--prefix-override-add (&optional _args)
  "Setup `emulation-mode-map-alist."
  (add-to-list 'emulation-mode-map-alists 'imbot--prefix-override-map-alist))

(defun imbot--prefix-override-remove (&optional _args)
  "Unset `emulation-mode-map-alist."
  (setq emulation-mode-map-alists
        (delq 'imbot--prefix-override-map-alist emulation-mode-map-alists)))

(defvar imbot--prefix-reinstate-triggers
  '(yas-minor-mode eaf-mode)
  "Handle modes that mess `emulation-mode-map-alists, add evil-local-mode if you use evil.")

(defun imbot--prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  ;; pre-command-hook is run before this function
  (let* ((keys (this-command-keys)))
    (unless imbot--active-omit-check
      ;; remove prefix override to avoid recusion, will be added in post-command-hook
      (imbot--prefix-override-remove))
    ;; (imbot--track-state "in prefix override!")
    ;; post-command-hook not run after the first key event?
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
  "Inline english overlay.")

(defvar imbot--inline-cursor '(hbar . 4)
  "Inline english cursor.")

(defface imbot--inline-face '()
  "Face to show inline english (input method temperarily disabled) is active."
  :group 'imbot)

(set-face-attribute
 'imbot--inline-face nil
 :weight 'bold
 ;; :foreground (face-attribute 'font-lock-constant-face :foreground)
 :inverse-video nil)

(defun imbot--english-region-p ()
  "Buffer is in `prog-mode or `conf-mode, and buffer string is not in a string or comment."
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun imbot--delete-overlay ()
  (delete-overlay imbot--overlay)
  (setq cursor-type my-default-cursor-type)
  (setq imbot--overlay nil))

(defun imbot--english-context-p ()
  "Return t if English should be inputed at cursor point."
  (unless (eq last-command 'imbot--inline-english-deactivate)
    (let* ((line-beginning (line-beginning-position))
           (point (point))
           (overlay-active (overlayp imbot--overlay))
           (english-context
            (or
             ;; 中文后面紧接1个空格切换到英文输入
             ;; \cC represents any character of category “C”, according to “M-x describe-categories”
             (looking-back "\\cC " (max line-beginning (- point 2)))
             (string-match "^\\s-*[0-9]+$" (buffer-substring-no-properties line-beginning point))
             (looking-at-p "^\\*")    ; org heading
             (looking-back "[a-zA-Z\\-\\*]" (max line-beginning (1- point))))))
      ;; (message "english context: %s, imbot--active-saved: %s" english-context imbot--active-saved)
      (if overlay-active
          (if (and english-context imbot--active-saved)
              (progn (move-overlay imbot--overlay line-beginning (line-end-position))
                     (message "Activate input method with [return]. Quit with [C-g]"))
            (imbot--delete-overlay))
        (when english-context
          (setq imbot--overlay (make-overlay line-beginning (line-end-position) nil t t))
          (setq cursor-type imbot--inline-cursor)
          (overlay-put imbot--overlay 'face 'imbot--inline-face)
          (overlay-put imbot--overlay 'keymap
                       (let ((keymap (make-sparse-keymap)))
                         (define-key keymap (kbd "C-g")
                                     #'imbot--inline-english-quit)
                         (define-key keymap (kbd "RET")
                                     #'imbot--inline-english-deactivate)
                         (define-key keymap (kbd "<return>")
                                     #'imbot--inline-english-deactivate)
                         keymap))))
      english-context)))

(defun imbot--inline-english-deactivate ()
  "Deactivate the inline english overlay."
  (interactive)
  (when (overlayp imbot--overlay)
    (imbot--delete-overlay))
  (imbot--activate))

(defun imbot--inline-english-quit ()
  "Quit the inline english overlay."
  (interactive)
  (when imbot--overlay
    (imbot--delete-overlay)
    (setq imbot--active-saved nil)
    (setq this-command 'imbot--inline-english-deactivate)))

(defun imbot--english-p ()
  "Check context."
  ;; (imbot--track-state "in english context checking!")
  (or (imbot--english-region-p)
      (imbot--english-context-p)))

(defvar god-local-mode nil)

(defvar hydra-curr-map nil)

(defvar imbot--suppression-watch-list
  '(god-local-mode
    hydra-curr-map)
  "Enable suppression if any variables in this list is t, add evil-normal-state-minor-mode
evil-visual-state-minor-mode evil-motion-state-minor-mode if evil is used")

(defvar imbot--suppression-major-mode
  '(dired-mode debugger-mode)
  "Enable suppression if buffer's major-mode matches any element of this list.")

(defun imbot--prefix-override-p ()
  "This-command becomes non nil after prefix sequence completion."
  (or
   ;; first lookup of imbot--prefix-override-keys?
   ;; second lookup of the pushed back imbot--prefix-override-keys?
   (equal last-command 'imbot--prefix-override-handler)
   (equal real-this-command 'imbot--prefix-override-handler)
   ;; use C-g to disable repeat popup
   (and repeat-mode repeat-in-progress)
   (memq last-command '(er/expand-region er/contract-region))))

(defvar imbot--suppression-predicates
  (list #'imbot--english-p #'imbot--prefix-override-p)
  "Conditions in which input method should be suppressed, in order of priority.")

(defvar imbot--suppressed nil
  "Buffer local suppression state.")

(make-variable-buffer-local 'imbot--suppressed)

(defun imbot--check-only ()
  ;; (message "Check and update checked status only!")
  (setq imbot--active-checked (imbot--active-p)))

(defun imbot--check-on-im-toggle ()
  ;; (message "Check and update saved status!")
  (setq imbot--active-checked (imbot--active-p))
  (setq imbot--active-saved imbot--active-checked)
  (imbot--update-cursor))

(defun imbot--activate ()
  (if imbot--active-omit-check
      (unless imbot--active-checked
        (imbot--activate-force))
    (imbot--activate-force)))

(defun imbot--deactivate ()
  (if imbot--active-omit-check
      (when imbot--active-checked
        (setq imbot--active-checked nil)
        (imbot--deactivate-force))
    (imbot--deactivate-force)))

;; won't work in windows if per app input status is on
;; the toggle with w32-set-ime-open-status will be undone
;; try im-select.exe?
(defun imbot-toggle-im ()
  (interactive)
  (if (imbot--active-p)
      (progn
        (imbot--activate-force)
        (setq imbot--active-saved t))
    (progn
      (imbot--deactivate-force)
      (setq imbot--active-saved nil))))

(defun imbot--pre-command-function ()
  "pre-command-hook function to update imbot--active-saved."
  ;; (add-hook 'post-command-hook #'imbot--post-command-function -100)
  (if buffer-read-only
      (setq imbot--suppressed t
            imbot--active-saved nil)
    (if imbot--active-omit-check
        (unless imbot--suppressed
          (setq imbot--active-saved imbot--active-checked))
      (unless (equal last-command 'imbot--prefix-override-handler)
        (unless (equal real-this-command 'self-insert-command)
          ;; below sexp will be cause the hook function being auto removed if it is slow.
          (if imbot--suppressed
              (imbot--check-only)
            (imbot--check-on-im-toggle)))))))

;; unless (equal real-this-command 'imbot-toggle-im)
(defun imbot--post-command-function ()
  "Restore input state."
  ;; When an editing command returns to the editor command loop, the buffer is still the original buffer,
  ;; buffer change after Emacs automatically calls set-buffer on the buffer shown in the selected window.
  ;; unless (equal 'real-this-command 'self-insert-command)
  (run-with-timer 0 nil
                  ;; in command that changes buffer, like winner-undo, the timer is run with the target window
                  (lambda ()
                    ;; (imbot--track-state "in post-command hook!")
                    (when (or imbot--active-saved
                              imbot--suppressed
                              imbot--active-checked)
                      ;; (imbot--track-state "in post-command timer!")
                      (if (or (eval `(or ,@imbot--suppression-watch-list))
                              (member major-mode imbot--suppression-major-mode)
                              (seq-find 'funcall imbot--suppression-predicates nil))
                          (progn (setq imbot--suppressed t)
                                 ;; (imbot--track-state "in post-command suppress!")
                                 ;; (imbot--track-state (format  "in post-command suppress by %s!"
                                 ;;                       (seq-find 'funcall
                                 ;;                         imbot--suppression-predicates nil)))
                                 (imbot--deactivate))
                        (when imbot--active-saved
                          ;; (imbot--track-state "restore input state!")
                          (imbot--activate))
                        (setq imbot--suppressed nil)))
                    ;; put this on an idle timer?
                    (unless imbot--active-omit-check
                      (unless (equal last-command 'imbot--prefix-override-handler)
                        (imbot--prefix-override-add)))
                    ;; (add-hook 'pre-command-hook #'imbot--pre-command-function -100)
                    (imbot--update-cursor))))

(defvar imbot-post-command-hook-list '(post-command-hook dired-mode-hook)
  "List of hook names to add `imbot--post-command-function into.")

(defun imbot--hook-handler (add-or-remove)
  "Setup hooks, ADD-OR-REMOVE."
  (funcall add-or-remove 'minibuffer-setup-hook 'imbot--deactivate)
  (funcall add-or-remove 'minibuffer-exit-hook 'imbot--post-command-function)
  ;; add to "global" pre-command-hook
  (funcall add-or-remove 'pre-command-hook #'imbot--pre-command-function)
  (dolist (hook-name imbot-post-command-hook-list)
    (funcall add-or-remove hook-name #'imbot--post-command-function)))

(defun imbot--non-interactive (orig-func &rest args)
  (let ((pre-command-hook nil)
        (post-command-hook nil))
    (apply orig-func args)))

;;;###autoload
(define-minor-mode imbot-mode
  "Input method managing bot."
  :global t
  :init-value nil
  (if imbot-mode
      (progn
        (advice-add #'execute-kbd-macro :around #'imbot--non-interactive)
        (imbot--hook-handler 'add-hook)
        (imbot--prefix-override-add)
        ;; (debug-watch 'imbot-mode)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-add trigger :after #'imbot--prefix-override-add)))
    (advice-remove #'execute-kbd-macro #'imbot--non-interactive)
    (imbot--hook-handler 'remove-hook)
    (imbot--prefix-override-remove)
    (dolist (trigger imbot--prefix-reinstate-triggers)
      (advice-remove trigger #'imbot--prefix-override-add))))

(provide 'imbot)
;;; imbot.el ends here
