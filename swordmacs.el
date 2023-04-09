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
(defconst swordmacs-books
  '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth" "I Samuel" "II Samuel" "I Kings" "II Kings" "I Chronicles" "II Chronicles" "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes" "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel" "Hosea"  "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk" "Zephaniah" "Haggai" "Zechariah" "Malachi"
    "Matthew" "Mark" "Luke" "John" "Acts" "Romans" "I Corinthians" "II Corinthians" "Galatians" "Ephesians" "Philippians" "Colossians" "I Thessalonians" "II Thessalonians" "I Timothy" "II Timothy" "Titus" "Philemon" "Hebrews" "James" "I Peter" "II Peter" "I John" "II John" "III John" "Jude"
    "Revelation of John")
  "List of strings representing books of the Bible.")
;;
;;;; Functions
;;
(defun swordmacs--in-block-p ()
  "Check if the point is inside a bible block."
  (if (org-in-block-p '("bible")) t nil))
;;
(defun swordmacs--diatheke-get-text (module key)
  "Return raw text from diatheke MODULE for KEY.
This simply calls `diatheke -b MODULE -k KEY' and returns the raw output."
  (with-temp-buffer
    (call-process "diatheke" nil '(t nil) nil
                  "-f" "plain"
                  "-b" module "-k" key)
    (buffer-substring (point-min) (save-excursion
                                    (goto-char (point-max))
                                    (forward-line -2)
                                    (end-of-line)
                                    (point)))))
;;
(defun swordmacs--replace-text-in-block (string)
  "Replace the text between `#+begin_bible` and `#+end_bible` with STRING."
  (let* ((beg (progn
                (end-of-line)
                (search-backward "#+begin_bible")
                (forward-line 1)
                (beginning-of-line)
                (point)))
         (end (progn
                (search-forward "#+end_bible")
                (forward-line -1)
                (end-of-line)
                (point))))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert string))))
;;
(defun swordmacs--get-block-key ()
  "Get the key string of a `swordmacs' block."
  (let* ((beg (progn
                (end-of-line)
                (search-backward "#+begin_bible")
                (search-forward " ")
                (point)))
         (end (progn
                (end-of-line)
                (point))))
    (buffer-substring beg end)))
;;
;;;; Commands
;;
(defun swordmacs-refresh-block ()
    "Update the verse contents of a Bible block."
  (interactive)
  (if (swordmacs--in-block-p)
      (let ((key (swordmacs--get-block-key)))
        (swordmacs--replace-text-in-block (swordmacs--diatheke-get-text swordmacs-default-module key)))
    (error "Not inside a bible block")))
;;
;;;; Hooks
;;
(add-hook 'org-ctrl-c-ctrl-c-final-hook
         (lambda () (if (swordmacs--in-block-p) (swordmacs-refresh-block))))
;;
(provide 'swordmacs)
;;
;;; swordmacs.el ends here
