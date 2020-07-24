;;; imbot.el --- an input method management bot: automatic system input method switch -*- lexical-binding: t; -*-

;; URL: https://github.com/QiangF/imbot
;; Created: July 24th, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0

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

(defvar imbot--im-active nil "buffer local input method state")

(make-variable-buffer-local 'imbot--im-active)

(defun imbot--im-active-p ()
  (let ((output
          (let (deactivate-mark)
            (with-temp-buffer
              (call-process imbot-command nil t)
              (buffer-string)))))
    (char-equal
     (aref output 0) ?2)))

(defun imbot--activate-im ()
  (call-process imbot-command nil nil nil "-o"))

(defun imbot--deactivate-im ()
  (call-process imbot-command nil nil nil "-c"))

(defun imbot--save-im-state ()
  (unless (minibufferp)
    (setq imbot--im-active (imbot--im-active-p))))

;; Input source specific cursor color and type
(defun imbot--restore-im-state ()
  (unless (minibufferp)
    (if imbot--im-active
        (progn
          (imbot--activate-im)
          (setq cursor-type 'hollow)
          (set-cursor-color "green"))
        (imbot--deactivate-im)
        (setq cursor-type 'bar)
        (set-cursor-color "white"))))

;; CAUTION: disable imbot-mode before looking up key definition start with imbot--prefix-override-keys
(defvar imbot--prefix-override-keys
  '("C-c" "C-x" "C-h"))

(defvar imbot--prefix-override-map-alist nil)

(let ((keymap (make-sparse-keymap)))
  (dolist (prefix
           imbot--prefix-override-keys)
    (define-key keymap (kbd prefix)
      #'imbot--prefix-override-handler))
  (setq imbot--prefix-override-map-alist
        `((imbot--prefix-override . ,keymap))))

(defvar imbot--prefix-override nil "imbot prefix override state")

(defvar imbot--last-buffer nil)

(defun imbot--prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  (let* ((keys (this-command-keys)))
    ;; temporarily disable prefix override
    (setq imbot--prefix-override nil)
    (imbot--save-im-state)
    (imbot--deactivate-im)
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
  "find file hook is run before post command hook, which is run in the window
with the old buffer, restore in the post-command-hook has to disabled."
  (setq imbot--disable-restore t)
  (imbot--deactivate-im))

(defun imbot--pre-command-hook ()
  ;; for command that changes buffer, save the last buffer before change happens
  (setq imbot--last-buffer (current-buffer)))

(defun imbot--post-command-hook ()
  (let ((prefix-override-command-finished-p (and (not imbot--prefix-override)
                                                 this-command))
        (same-buffer (eq imbot--last-buffer (current-buffer))))
    ;; 1. maybe save im state
    ;; no need to save im state if buffer does not change
    (unless same-buffer
      ;; right after a override prefix sequence the im state is already saved
      (when (and imbot--prefix-override
                 (buffer-live-p imbot--last-buffer))
        ;; save im state for previous buffer
        (with-current-buffer imbot--last-buffer
          (imbot--save-im-state))))
    ;; 2. restore im state in current buffer after a override prefix sequence or buffer change
    (when (or prefix-override-command-finished-p 
              (not same-buffer))
      (if imbot--disable-restore
          (setq imbot--disable-restore nil)
          (imbot--restore-im-state)))
    ;; 3. init minibuffer im state
    (when (and (minibufferp)
               (not same-buffer))
      (imbot--deactivate-im))
    ;; 4. reset prefix override after prefix sequence completed
    (when prefix-override-command-finished-p 
      (setq imbot--prefix-override t))))

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
  (when (imbot--im-active-p)
    (or
     ;; 中文后面紧接1个空格切换到英文输入
     ;; \cC represents any character of category “C”, according to “M-x describe-categories”
     (looking-back "\\cc " (max (line-beginning-position) (- (point) 2)))
     ;; 英文,数字后保持英文输入
     (looking-back "[a-zA-Z0-9\\-]" (max (line-beginning-position) (1- (point)))))))

(defun imbot--check-context()
  (when (imbot--english-context-p)
    (imbot--deactivate-im)
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
        (imbot--activate-im))))

(defun imbot--toggle-punctuation ())

;; some os input method remembers the input state per application, such as fcitx
;; fcitx change per application input method state before focus-out and focus-in hook
(defvar imbot--override-per-app-switch nil "override os input method manager per application switch")

(defun imbot--hook-handler (add-or-remove)
  (when (boundp 'evil-mode)
    (funcall add-or-remove 'evil-insert-state-exit-hook #'imbot--deactivate-im)
    (funcall add-or-remove 'evil-insert-state-exit-hook #'imbot--save-im-state)
    (funcall add-or-remove 'evil-emacs-state-exit-hook #'imbot--deactivate-im)
    (funcall add-or-remove 'evil-emacs-state-exit-hook #'imbot--save-im-state)
    (funcall add-or-remove 'evil-insert-state-entry-hook #'imbot--restore-im-state)
    (funcall add-or-remove 'evil-emacs-state-entry-hook #'imbot--restore-im-state))
  (when imbot--override-per-app-switch 
    (funcall add-or-remove 'focus-out-hook #'imbot--save-im-state)
    (funcall add-or-remove 'focus-in-hook #'imbot--restore-im-state))
  (funcall add-or-remove 'find-file-hook #'imbot--find-file-hook)
  (funcall add-or-remove 'post-self-insert-hook #'imbot--check-context)
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
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-add trigger :after #'imbot--prefix-override-add)))
      (progn
        (imbot--hook-handler 'remove-hook)
        (imbot--prefix-override-remove)
        (dolist (trigger imbot--prefix-reinstate-triggers)
          (advice-remove trigger #'imbot--prefix-override-add)))))

(provide 'imbot)
;;; imbot.el ends here
