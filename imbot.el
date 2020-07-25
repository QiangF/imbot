;;; imbot.el --- an input method management bot: automatic system input method switch -*- lexical-binding: t; -*-

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

;;; Code:

(require 'subr-x)

(defvar imbot-command "fcitx-remote")

(defvar imbot--active nil "buffer local input method state")

(make-variable-buffer-local 'imbot--active)

(defun imbot--active-p ()
  (let ((output
          (let (deactivate-mark)
            (with-temp-buffer
              (call-process imbot-command nil t)
              (buffer-string)))))
    (char-equal
     (aref output 0) ?2)))

(defun imbot--activate ()
  (setq current-input-method t)
  (call-process imbot-command nil nil nil "-o"))

(defun imbot--deactivate ()
  (setq current-input-method nil)
  (call-process imbot-command nil nil nil "-c"))

(defun imbot--save ()
  (unless (minibufferp)
    (setq imbot--active (imbot--active-p))))

;; Input source specific cursor color and type, or you can use cursor-chg.el
;; unless current-input-method is toggled together input method change with a hot key,
;; cursor color is not updated untill imbot--restore is called.
(defun imbot--restore ()
  (unless (minibufferp)
    (if imbot--active
        (progn
          (imbot--activate)
          (setq cursor-type 'hollow)
          (set-cursor-color "green"))
        (imbot--deactivate)
        (setq cursor-type 'bar)
        (set-cursor-color "white"))))

;; CAUTION: disable imbot-mode before looking up key definition start with imbot--prefix-override-keys
(defvar imbot--prefix-override-keys
  '("C-c" "C-x" "C-h" "<f1>"))

(defvar imbot--prefix-override-map-alist nil)

(let ((keymap (make-sparse-keymap)))
  (dolist (prefix
           imbot--prefix-override-keys)
    (define-key keymap (kbd prefix)
      #'imbot--prefix-override-handler))
  (setq imbot--prefix-override-map-alist
        `((imbot--prefix-override . ,keymap))))

(defvar imbot--prefix-override nil "imbot prefix override state")

(defun imbot--prefix-override-handler (arg)
  "Prefix key handler with ARG."
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

(defvar imbot--disable-restore nil)

(defun imbot--find-file-hook ()
  (setq imbot--disable-restore t)
  (imbot--deactivate))

(defvar imbot--last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'imbot--last-post-command-position)

(defvar imbot--last-buffer nil)

(defvar imbot--check-for-new-buffer nil)

(defun imbot--pre-command-hook ()
  ;; for command that changes buffer, save the last buffer before change happens
  (setq imbot--last-buffer (current-buffer)))

;; hydra-curr-map is a variable in hydra, it is non-nil when hydra is active
(defvar hydra-curr-map nil)

(defun imbot--post-command-hook ()
  (let* ((prefix-override-command-finished-p (and (not imbot--prefix-override)
                                                  (not hydra-curr-map)
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
    (unless (equal (point) imbot--last-post-command-position)
      (imbot--check-context)
      (setq imbot--last-post-command-position (point)))
    ;; some functions like find-file changes buffer after post-command-hook
    ;; so save it here for handling of new buffer creation using buffer-list-update-hook
    (setq imbot--last-buffer current-buffer
          imbot--check-for-new-buffer t)))

(defun imbot--prefix-override-add (&optional args)
  (add-to-list 'emulation-mode-map-alists 'imbot--prefix-override-map-alist))

(defun imbot--prefix-override-remove (&optional args)
  (setq emulation-mode-map-alists
        (delq 'imbot--prefix-override-map-alist emulation-mode-map-alists)))

(defvar imbot--prefix-reinstate-triggers
  '(evil-local-mode yas-minor-mode eaf-mode)
  "handle modes that mess with `emulation-mode-map-alists")

(defface imbot--inline-face '()
  "face to show inline editing (input method temperarily disabled) is active"
  :group 'imbot)

(set-face-attribute
 'imbot--inline-face nil
 :foreground (face-attribute 'font-lock-constant-face :foreground)
 :underline t
 :inverse-video t)

(defun imbot--english-context-p ()
  (when (imbot--active-p)
    (or
     ;; 中文后面紧接1个空格切换到英文输入
     ;; \cC represents any character of category “C”, according to “M-x describe-categories”
     (looking-back "\\cc " (max (line-beginning-position) (- (point) 2)))
     ;; 英文,数字后保持英文输入
     (looking-back "[a-zA-Z0-9\\-]" (max (line-beginning-position) (1- (point)))))))

(defun imbot--check-context()
  (when (imbot--english-context-p)
    (imbot--deactivate)
    (setq imbot--overlay (make-overlay (line-beginning-position) (line-end-position) nil t t ))
    (overlay-put imbot--overlay 'face 'imbot--inline-face)
    (overlay-put imbot--overlay 'keymap
                 (let ((keymap (make-sparse-keymap)))
                   (define-key keymap (kbd "RET")
                     #'imbot--inline-edit-deactivate)
                   (define-key keymap (kbd "<return>")
                     #'imbot--inline-edit-deactivate)
                   keymap))))

(defun imbot--inline-edit-deactivate ()
  "Deactivate the inline region overlay."
  (interactive)
  (if (and (featurep 'company)
           (company--active-p))
      (company-complete-selection)
      (when (overlayp imbot--overlay)
        (delete-overlay imbot--overlay)
        (setq imbot--overlay nil)
        (imbot--activate))))

(defun imbot--toggle-punctuation ())

;; some os input method remembers the input state per application, such as fcitx.
;; in such case there is no need to hook into focus change. actuall it will cause unpredictable
;; race conditions, as fcitx changes per application input method state before focus hooks
(defvar imbot--hook-into-focus-change nil
  "set this to `t if the input method manager does not remember per app input state")

;; commands such as counsel-find-file opens the minibuffer, which disables the input method,
;; however the buffer is switched back to the buffer before find file intermitantly, if
;; restore im state in the post-command-hook while the current buffer is the old buffer,
;; new file buffer will inherite the input state, which is not predictable, there are 3 options:
;; 1. disable restore in the post-command-hook (need to keep a list of functions)
;; 2. deactivate im in the new buffer for now (there is a delay)
;; 3. advice generate-new-buffer or get-buffer-create (called too often)
(defun imbot--deactivate-in-new-buffer ()
    (when imbot--check-for-new-buffer
      (unless (eq imbot--last-buffer (window-buffer))
        (imbot--deactivate))
      (setq imbot--check-for-new-buffer nil)))

(defun imbot--new-buffer (orig-func &rest args)
  (let ((new-buffer (apply orig-func args)))
    (imbot--deactivate-in-new-buffer)
    new-buffer))

(defun imbot--hook-handler (add-or-remove)
  (when (boundp 'evil-mode)
    (funcall add-or-remove 'evil-insert-state-exit-hook #'imbot--deactivate)
    (funcall add-or-remove 'evil-insert-state-exit-hook #'imbot--save)
    (funcall add-or-remove 'evil-emacs-state-exit-hook #'imbot--deactivate)
    (funcall add-or-remove 'evil-emacs-state-exit-hook #'imbot--save)
    (funcall add-or-remove 'evil-insert-state-entry-hook #'imbot--restore)
    (funcall add-or-remove 'evil-emacs-state-entry-hook #'imbot--restore))
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

(define-minor-mode imbot-mode
  "input method manage bot"
  :global t
  :init-value nil
  (if imbot-mode
      (progn
        (imbot--hook-handler 'add-hook)
        (imbot--prefix-override-add)
        ;; (advice-add #'generate-new-buffer :around #'imbot--new-buffer)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-add trigger :after #'imbot--prefix-override-add)))
      (progn
        (imbot--hook-handler 'remove-hook)
        (imbot--prefix-override-remove)
        ;; (advice-remove #'generate-new-buffer #'imbot--new-buffer)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-remove trigger #'imbot--prefix-override-add)))))

(provide 'imbot)
;;; imbot.el ends here
