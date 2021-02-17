;;; -*- lexical-binding: t; -*-

;; For fcitx, the command is "fcitx-remote"
(defvar imbot-command "/usr/bin/ibus engine"
  "Input method management command.")

;; For fcitx, the tag is "1"
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
  (with-temp-buffer
    (call-process imbot-command nil t)
    (not (string-equal (string-trim (buffer-string))
                       imbot-english-engine-tag))))

(defun imbot--activate ()
  "Set input method in non English state."
  (unless imbot--active-checked
    (setq imbot--active-checked t)
    (call-process imbot-command nil nil nil imbot-input-activate-switch)))

(defun imbot--deactivate ()
  "Set input method in English state."
  (when imbot--active-checked
    (setq imbot--active-checked nil)
    (call-process imbot-command nil nil nil imbot-input-deactivate-switch)))

(provide 'imbot--ibus)
