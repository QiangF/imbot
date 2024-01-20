;;; -*- lexical-binding: t; -*-

;;; Commentary:
;; this config file redefines:
;; imbot--active-p, imbot--activate, imbot--deactivate

(defun imbot--active-p ()
  (w32-get-ime-open-status))

(defun imbot--activate-force ()
  (setq imbot--active-checked t)
  (w32-set-ime-open-status t))

(defun imbot--deactivate-force ()
  (setq imbot--active-checked nil)
  (w32-set-ime-open-status nil))

(setq imbot--active-omit-check nil)

(provide 'imbot--windows)
