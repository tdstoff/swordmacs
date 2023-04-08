;;; swordmacs.el --- The SWORD Project inside Emacs -*- lexical-binding: t; -*-
;;
;; Author: Tyler D. Stoffel <tdstoff7@gmail.com>
;; Maintainer: Tyler D. Stoffel <tdstoff7@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/tdstoff/swordmacs
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Swordmacs is an Emacs interface to Biblical texts provided by the SWORD
;; project using the command line tool `diatheke'.
;;
;;; Usage:
;;
;; First install `diatheke'. On Debian/Ubuntu it's in the `diatheke'
;; package. On MacOS homebrew it's the `sword' package.
;;
;;; Code:
;;
;;;; Requirements
(require 'org)
;;
;;;; Variables
;;
(defgroup swordmacs nil
  "Settings for `swordmacs'."
  :link '(url-link "http://github.com/tdstoff/swordmacs")
  :group 'org)
;;
(defcustom swordmacs-default-module "KJV"
  "Default module (e.g. Bible translation, like \"ESV\") to use."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Module abbreviation (e.g. \"ESV\")")))
;;
;;;; Functions
(defun swordmacs-in-block-p ()
  "Check if the point is inside a bible block."
  (interactive)
  (if (org-in-block-p '("bible")) t nil))
;;
;;;; Commands
;;
(provide 'swordmacs)
;;
;;; swordmacs.el ends here
