;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; this config file redefines:
;; imbot--active-p, imbot--activate, imbot--deactivate

(require 'dbus)

(setq imbot-command (or (locate-file "fcitx5-remote" exec-path)
                        "fcitx-remote")
      imbot-english-engine-tag 1
      imbot-input-activate-switch "-o"
      imbot-input-deactivate-switch "-c")

(defun fcitx5-dbus-call-method (method)
  (dbus-call-method :session
                    "org.fcitx.Fcitx5"
                    "/controller"
                    "org.fcitx.Fcitx.Controller1"
                    method))

(defun imbot--active-p ()
  (not (equal (fcitx5-dbus-call-method "State")
              imbot-english-engine-tag)))

(defun imbot--activate ()
  (unless imbot--active-checked
    (setq imbot--active-checked t)
    (fcitx5-dbus-call-method "Activate")))

(defun imbot--deactivate ()
  (when imbot--active-checked
    (setq imbot--active-checked nil)
    (fcitx5-dbus-call-method "Deactivate")))

(provide 'imbot--fcitx5)
