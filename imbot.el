;;; imbot.el --- Automatic system input method switcher -*- lexical-binding: t; -*-

;; URL: https://github.com/QiangF/imbot
;; Created: July 24th, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; Version: 2.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; imbot is inspired by https://github.com/laishulu/emacs-smart-input-source
;; use it like this:
;; (use-package imbot
;;  :hook (evil-mode . imbot-mode)
;;  :config
;;    ;; 1. redefine these functions if you are not using fcitx-remote
;;    ;; imbot--active-p 
;;    ;; imbot--activate
;;    ;; imbot--deactivate 
;;    ;; imbot--save 
;;    ;; 2. set the following to t if the input state is not saved
;;    ;; during focusing and unfocusing emacs window using the mouse
;;    (setq imbot--hook-into-focus-change nil
;;          imbot--inline-edit-enable t))

;;; Code:

(require 'subr-x)

(defvar imbot--active nil
  "Buffer local input method state")

(make-variable-buffer-local 'imbot--active)

(defvar imbot-command "fcitx-remote"
  "Input method management commangd")

(defun imbot--active-p ()
  "Return t when input method in non English state"
  (let ((output
          (let (deactivate-mark)
            (with-temp-buffer
              (call-process imbot-command nil t)
              (buffer-string)))))
    (char-equal
     (aref output 0) ?2)))

(defun imbot--activate ()
  "Set input method in non English state"
  (call-process imbot-command nil nil nil "-o"))

(defun imbot--deactivate ()
  "Set input method in English state"
  (call-process imbot-command nil nil nil "-c"))

(defun imbot--save ()
  "Set buffer local input method state"
  (unless (minibufferp)
    (setq imbot--active (imbot--active-p))))

(defvar imbot--update-cursor-timer nil
  "Timer for updating cursor color")

(defun imbot--update-cursor (&optional no-query)
  "Set cursor color according to input method state"
  (let ((im-active (if no-query imbot--active (imbot--active-p))))
    (if im-active
        (set-cursor-color "green")
        (set-cursor-color "white"))))

(defun imbot--restore ()
  "Restore buffer local input method state"
  (unless (minibufferp)
    (if imbot--active
        (imbot--activate)
        (imbot--deactivate)))
  (imbot--update-cursor t))

;; CAUTION: disable imbot-mode before looking up key definition start with imbot--prefix-override-keys
(defvar imbot--prefix-override-keys
  '("C-c" "C-x" "C-h" "<f1>")
  "Prefix keys that disable input method temperarily")

(defvar imbot--prefix-override-map-alist nil
  "An emulation-mode-map-alists keymap")

(let ((keymap (make-sparse-keymap)))
  (dolist (prefix
           imbot--prefix-override-keys)
    (define-key keymap (kbd prefix)
      #'imbot--prefix-override-handler))
  (setq imbot--prefix-override-map-alist
        `((imbot--prefix-override . ,keymap))))

(defun imbot--prefix-override-add ()
  "Setup emulation-mode-map-alist"
  (add-to-list 'emulation-mode-map-alists 'imbot--prefix-override-map-alist))

(defun imbot--prefix-override-remove ()
  "Unset emulation-mode-map-alist"
  (setq emulation-mode-map-alists
        (delq 'imbot--prefix-override-map-alist emulation-mode-map-alists)))

(defvar imbot--prefix-reinstate-triggers
  '(evil-local-mode yas-minor-mode eaf-mode)
  "handle modes that mess with `emulation-mode-map-alists")

(defvar imbot--prefix-override nil
  "Imbot prefix override state")

(defun imbot--prefix-override-handler (arg)
  "Prefix key handler with ARG"
  (interactive "P")
  (let* ((keys (this-command-keys)))
    ;; temporarily disable prefix override
    (setq imbot--prefix-override nil)
    (imbot--save)
    (imbot--deactivate)
    ;; Restore the prefix arg
    (setq prefix-arg arg)
    (prefix-command-preserve-state)
    ;; reset this-command so it can be used to indicate the end of a command sequence
    (setq this-command nil)
    ;; Push the key back on the event queue
    (setq unread-command-events
          (append (mapcar (lambda (e) `(t . ,e)) (listify-key-sequence keys))
                  unread-command-events))))

(defvar imbot--disable-restore nil
  "Disable restore for current command")

(defun imbot--find-file-hook ()
  "Disable restore in find-file-hook"
  (setq imbot--disable-restore t)
  (imbot--deactivate))

(defvar imbot--last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks")

(make-variable-buffer-local 'imbot--last-post-command-position)

(defvar imbot--last-buffer nil
  "Buffer at the start of an interactive command")

(defvar imbot--check-for-new-buffer nil
  "Check if new buffer created due to an interactive command")

(defvar imbot--inline-edit-enable t
  "Enable inline auto switch to English input state")

(defvar imbot--overlay nil
  "Inline editing overlay")

(defun imbot--pre-command-hook ()
  "For command that changes buffer, save the last buffer before change happens"
  (setq imbot--last-buffer (current-buffer)))

;; hydra-curr-map is a variable in hydra, it is non-nil when hydra is active
(defun imbot--post-command-hook ()
  "The main input state processor"
  (let* ((prefix-override-command-finished-p (and (not imbot--prefix-override)
                                                  (not (and (boundp hydra-curr-map)
                                                            hydra-curr-map))
                                                  this-command))
         (current-buffer (current-buffer))
         (same-buffer (eq imbot--last-buffer current-buffer)))
    ;; 1. maybe save im state
    ;; no need to save im state if buffer does not change
    (unless same-buffer
      ;; right after a override prefix sequence the im state is already saved
      (when (and imbot--prefix-override
                 (buffer-live-p imbot--last-buffer))
        ;; save im state for previous buffer
        (with-current-buffer imbot--last-buffer
          (imbot--save))))
    ;; 2. restore im state in current buffer after a override prefix sequence or buffer change
    (when (or prefix-override-command-finished-p
              (not same-buffer))
      (if imbot--disable-restore
          (setq imbot--disable-restore nil)
          (imbot--restore)))
    ;; 3. init minibuffer im state
    (when (and (minibufferp)
               (not same-buffer))
      (imbot--deactivate))
    ;; 4. reset prefix override after prefix sequence completed
    (when prefix-override-command-finished-p
      (setq imbot--prefix-override t))
    ;; 5. check context for inline editing condition
    (when imbot--inline-edit-enable
      (unless (equal (point) imbot--last-post-command-position)
        (imbot--check-context)
        (setq imbot--last-post-command-position (point))))
    ;; some functions like find-file changes buffer after post-command-hook
    ;; so save it here for handling of new buffer creation using buffer-list-update-hook
    (setq imbot--last-buffer current-buffer
          imbot--check-for-new-buffer t)))

(defface imbot--inline-face '()
  "Face to show inline editing (input method temperarily disabled) is active"
  :group 'imbot)

(set-face-attribute
 'imbot--inline-face nil
 :foreground (face-attribute 'font-lock-constant-face :foreground)
 :underline t
 :inverse-video t)

(defun imbot--english-only-p ()
  "Buffer is in prog-mode or conf-mode, and buffer string is not in a string or comment"
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun imbot--english-context-p ()
  "Return t if English should be inputed at cursor point"
  (when (imbot--active-p)
    (or
     ;; 中文后面紧接1个空格切换到英文输入
     ;; \cC represents any character of category “C”, according to “M-x describe-categories”
     (looking-back "\\cc " (max (line-beginning-position) (- (point) 2)))
     ;; 英文,数字后保持英文输入
     (looking-back "[a-zA-Z0-9\\-]" (max (line-beginning-position) (1- (point)))))))

(defun imbot--check-context()
  "Check context"
  (let ((english-context (and (not (overlayp imbot--overlay))
                              (imbot--english-context-p)))
        (region-english-only (imbot--english-only-p)))
    (when (or english-context region-english-only)
      (imbot--deactivate))
    (when (and english-context (not region-english-only))
      (setq imbot--overlay (make-overlay (line-beginning-position) (line-end-position) nil t t ))
      (overlay-put imbot--overlay 'face 'imbot--inline-face)
      (overlay-put imbot--overlay 'keymap
                   (let ((keymap (make-sparse-keymap)))
                     (define-key keymap (kbd "RET")
                       #'imbot--inline-edit-deactivate)
                     (define-key keymap (kbd "<return>")
                       #'imbot--inline-edit-deactivate)
                     keymap)))))

(declare-function company--active-p "ext:company.el")
(declare-function company-complete-selection "ext:company.el")

(defun imbot--inline-edit-deactivate ()
  "Deactivate the inline region overlay"
  (interactive)
  (if (and (featurep 'company)
           (company--active-p))
      (company-complete-selection)
      (when (overlayp imbot--overlay)
        (delete-overlay imbot--overlay)
        (setq imbot--overlay nil)
        (imbot--activate))))

;; some os input method remembers the input state per application, such as fcitx.
;; in such case there is no need to hook into focus change. actuall it will cause unpredictable
;; race conditions, as fcitx changes per application input method state before focus hooks
(defvar imbot--hook-into-focus-change nil
  "Set this to t if the input method manager does not remember per app input state")

;; commands such as counsel-find-file opens the minibuffer, which disables the input method,
;; however the buffer is switched back to the buffer before find file intermitantly, if
;; restore im state in the post-command-hook while the current buffer is the old buffer,
;; new file buffer will inherite the input state, which is not predictable, there are 3 options:
;; 1. disable restore in the post-command-hook (need to keep a list of functions)
;; 2. deactivate im in the new buffer for now (there is a delay)
;; 3. advice generate-new-buffer or get-buffer-create (called too often)
(defun imbot--deactivate-in-new-buffer ()
  "Deactivate"
    (when imbot--check-for-new-buffer
      (unless (eq imbot--last-buffer (window-buffer))
        (imbot--deactivate))
      (setq imbot--check-for-new-buffer nil)))

(defun imbot--new-buffer (orig-func &rest args)
  "Function currently not used, it can be used as advice to generate-new-buffer"
  (let ((new-buffer (apply orig-func args)))
    (imbot--deactivate-in-new-buffer)
    new-buffer))

(defun imbot--hook-handler (add-or-remove)
  "Setup hooks"
  (when (boundp 'evil-mode)
    (funcall add-or-remove 'evil-insert-state-exit-hook #'imbot--deactivate)
    (funcall add-or-remove 'evil-insert-state-exit-hook #'imbot--save)
    (funcall add-or-remove 'evil-emacs-state-exit-hook #'imbot--deactivate)
    (funcall add-or-remove 'evil-emacs-state-exit-hook #'imbot--save)
    (funcall add-or-remove 'evil-insert-state-entry-hook #'imbot--restore)
    (funcall add-or-remove 'evil-emacs-state-entry-hook #'imbot--restore))
  (when (boundp 'god-local-mode)
    (funcall add-or-remove 'god-mode-enabled-hook #'imbot--deactivate)
    (funcall add-or-remove 'god-mode-enabled-hook #'imbot--save)
    (funcall add-or-remove 'god-mode-disabled-hook #'imbot--restore))
  (when imbot--hook-into-focus-change
    (funcall add-or-remove 'focus-out-hook #'imbot--save)
    (funcall add-or-remove 'focus-in-hook #'imbot--restore))
  ;; there is an overlap in functionality between file-file-hook and imbot--deactivate-in-new-buffer
  ;; since there is a delay in calling buffer-list-update-hook
  (funcall add-or-remove 'find-file-hook #'imbot--find-file-hook)
  ;; buffer-list-update-hook is needed for new buffer (eg. *help*) created without using find-file-hook
  (funcall add-or-remove 'buffer-list-update-hook #'imbot--deactivate-in-new-buffer)
  (funcall add-or-remove 'pre-command-hook #'imbot--pre-command-hook)
  (funcall add-or-remove 'post-command-hook #'imbot--post-command-hook))

;;;###autoload
(define-minor-mode imbot-mode
  "Input method manage bot"
  :global t
  :init-value nil
  (if imbot-mode
      (progn
        (imbot--hook-handler 'add-hook)
        (imbot--prefix-override-add)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-add trigger :after #'imbot--prefix-override-add))
        (setq imbot--update-cursor-timer
              (run-with-idle-timer 2 nil 'imbot--update-cursor)))
      (progn
        (imbot--hook-handler 'remove-hook)
        (imbot--prefix-override-remove)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-remove trigger #'imbot--prefix-override-add))
        (cancel-timer imbot--update-cursor-timer))))

(provide 'imbot)
;;; imbot.el ends here
