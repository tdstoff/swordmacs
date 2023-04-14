;;; swordmacs.el --- The SWORD Project inside Emacs -*- lexical-binding: t; -*-
;;
;; Author: Tyler D. Stoffel <tdstoff7@gmail.com>
;; Maintainer: Tyler D. Stoffel <tdstoff7@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/tdstoff/swordmacs
;; Package-Requires: ((emacs "28.1"))
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
(require 'transient)
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
(defun swordmacs--overwrite-block-key (key)
  "Overwrite the key string of a `swordmacs' block with KEY."
  (let* ((beg (progn
                (end-of-line)
                (search-backward "#+begin_bible")
                (search-forward " ")
                (point)))
         (end (progn
                (end-of-line)
                (point))))
    (delete-region beg end)
    (insert key)))
;;
(defun swordmacs--is-single-chapter-p (string)
  "Return t if key STRING is a single chapter, nil otherwise."
  (and (= (count ?\: string) 0) (= (count ?\- string) 0)))
;;
(defun swordmacs--is-chapter-range-p (string)
  "Return t if key STRING is a chapter range, nil otherwise."
  (and (= (count ?\: string) 0) (= (count ?\- string) 1)))
;;
(defun swordmacs--is-single-verse-p (string)
  "Return t if key STRING is a single verse, nil otherwise."
  (and (= (count ?\: string) 1) (= (count ?\- string) 0)))
;;
(defun swordmacs--is-verse-range-p (string)
  "Return t if key STRING is a verse range, nil otherwise."
  (and (= (count ?\: string) 1) (= (count ?\- string) 1)))
;;
(defun swordmacs--is-chapter-verse-range-p (string)
  "Return t if key STRING is a chapter and verse range, nil otherwise."
  (and (= (count ?\: string) 2) (= (count ?\- string) 1)))
;;
(defun swordmacs--key-type (string)
  "Return object `type' of key STRING.

Examples:

\"John 3\"        -> 'chapter
\"John 3-4\"      -> 'chapter-range
\"John 3:16\"     -> 'verse
\"John 3:16-17\"  -> 'verse-range
\"John 3:36-4:1\" -> 'chapter-verse-range"
  (if (swordmacs--is-single-chapter-p string) 'chapter
    (if (swordmacs--is-chapter-range-p string) 'chapter-range
      (if (swordmacs--is-single-verse-p string) 'verse
        (if (swordmacs--is-verse-range-p string) 'verse-range
          (if (swordmacs--is-chapter-verse-range-p string) 'chapter-verse-range
            (error "Verse key not recognized")))))))
;;
(defun swordmacs--parse-key (key)
  "Parse a verse key string KEY and return a key list.
Type, chapter name, chapter number(s), and verse number(s).

Examples:

\"John 3\"        -> (:type 'chapter             :book \"John\" :chapter-start 3)
\"John 3-4\"      -> (:type 'chapter-range       :book \"John\" :chapter-start 3 :chapter-end 4)
\"John 3:16\"     -> (:type 'verse               :book \"John\" :chapter-start 3 :verse-start 16)
\"John 3:16-17\"  -> (:type 'verse-range         :book \"John\" :chapter-start 3 :verse-start 16 :verse-end 17)
\"John 3:36-4:1\" -> (:type 'chapter-verse-range :book \"John\" :chapter-start 3 :verse-start 36 :chapter-end 4 :verse-end 1)"
  (let* ((components (split-string key "[ :\-]"))
         (type (swordmacs--key-type key))
         (book (car components))
         (chapter-start (string-to-number (cadr components)))
         (chapter-end (if (eq type 'chapter-range) (string-to-number (caddr components)) (if (eq type 'chapter-verse-range) (string-to-number (cadddr components)) 0)))
         (verse-start (if (or (eq type 'verse) (eq type 'verse-range) (eq type 'chapter-verse-range)) (string-to-number (caddr components)) 0))
         (verse-end (if (eq type 'verse-range) (string-to-number (car (cdddr components))) (if (eq type 'chapter-verse-range) (string-to-number (car (cddddr components))) 0))))
    (list :type type :book book :chapter-start chapter-start :chapter-end chapter-end :verse-start verse-start :verse-end verse-end)))
;;
(defun swordmacs--reconstruct-key (key-list)
  "Reconstruct a verse key string from a parsed key list KEY-LIST."
  (let ((type (plist-get key-list :type))
        (book (plist-get key-list :book))
        (chapter-start (plist-get key-list :chapter-start))
        (chapter-end (plist-get key-list :chapter-end))
        (verse-start (plist-get key-list :verse-start))
        (verse-end (plist-get key-list :verse-end)))
    (cond
     ((eq type 'chapter)
      (format "%s %d" book chapter-start))
     ((eq type 'chapter-range)
      (format "%s %d-%d" book chapter-start chapter-end))
     ((eq type 'verse)
      (format "%s %d:%d" book chapter-start verse-start))
     ((eq type 'verse-range)
      (format "%s %d:%d-%d" book chapter-start verse-start verse-end))
     ((eq type 'chapter-verse-range)
      (format "%s %d:%d-%d:%d" book chapter-start verse-start chapter-end verse-end))
     (t ""))))
;;
(defun swordmacs--change-key (key ref-type amount)
  "Change KEY's component of REF-TYPE by AMOUNT."
  (let* ((parsed-key (swordmacs--parse-key key))
         (type (plist-get parsed-key :type))
         (book (plist-get parsed-key :book))
         (chapter-start (plist-get parsed-key :chapter-start))
         (chapter-end (plist-get parsed-key :chapter-end))
         (verse-start (plist-get parsed-key :verse-start))
         (verse-end (plist-get parsed-key :verse-end)))
    (setq chapter-start (if (eq ref-type 'chapter-start) (+ chapter-start amount) chapter-start))
    (setq chapter-end (if (eq ref-type 'chapter-end) (+ chapter-end amount) chapter-end))
    (setq verse-start (if (eq ref-type 'verse-start) (+ verse-start amount) verse-start))
    (setq verse-end (if (eq ref-type 'verse-end) (+ verse-end amount) verse-end))
    (swordmacs--reconstruct-key (list :type type :book book :chapter-start chapter-start :chapter-end chapter-end :verse-start verse-start :verse-end verse-end))))
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
(defun swordmacs-append (&optional count)
  "Add COUNT number of verses to the end of the Bible block.
Default is one verse."
  (interactive "p")
  (if (swordmacs--in-block-p)
      (let* ((key (swordmacs--get-block-key))
             (count (or count 1))
             (appended-key (swordmacs--change-key key 'verse-end count)))
        (swordmacs--overwrite-block-key appended-key)
        (swordmacs-refresh-block))
    (error "Not inside a bible block")))
;;
(defun swordmacs-unappend (&optional count)
  "Delete COUNT number of verses from the end of the Bible block.
Default is one verse."
  (interactive "p")
  (let* ((count (or count 1))
         (neg-count (- count)))
    (swordmacs-append neg-count)))
;;
(defun swordmacs-prepend (&optional count)
  "Add COUNT number of verses to the beginning of the Bible block.
Default is one verse."
  (interactive "p")
  (if (swordmacs--in-block-p)
      (let* ((key (swordmacs--get-block-key))
             (count (or count 1))
             (neg-count (- count))
             (appended-key (swordmacs--change-key key 'verse-start neg-count)))
        (swordmacs--overwrite-block-key appended-key)
        (swordmacs-refresh-block))
    (error "Not inside a bible block")))
;;
(defun swordmacs-unprepend (&optional count)
  "Delete COUNT number of verses to the beginning of the Bible block.
Default is one verse."
  (interactive "p")
  (let* ((count (or count 1))
         (neg-count (- count)))
    (swordmacs-prepend neg-count)))
;;
(defun swordmacs-forward (&optional count)
  "Move COUNT number of verses forward.
Default is one verse."
  (interactive "p")
  (let* ((count (or count 1)))
    (swordmacs-append count)
    (swordmacs-unprepend count)))
;;
(defun swordmacs-back (&optional count)
  "Move COUNT number of verses backward.
Default is one verse."
  (interactive "p")
  (let* ((count (or count 1)))
    (swordmacs-prepend count)
    (swordmacs-unappend count)))
;;
(defun swordmacs-dispatch ()
  "Perform `swordmacs' actions again and again."
  (interactive)
  (if (swordmacs--in-block-p)
      (swordmacs-transient)
    (error "Not inside a bible block")))
(transient-define-prefix swordmacs-transient ()
  "Swordmacs Popup"
  :transient-suffix 'transient--do-stay
  [["Navigation"
    ("n" "Append verse(s)"    (lambda (arg) (interactive "p") (swordmacs-append arg)))
    ("N" "Unappend verse(s)"  (lambda (arg) (interactive "p") (swordmacs-unappend arg)))
    ("p" "Prepend verse(s)"   (lambda (arg) (interactive "p") (swordmacs-prepend arg)))
    ("P" "Unprepend verse(s)" (lambda (arg) (interactive "p") (swordmacs-unprepend arg)))
    ("f" "Forward"            (lambda (arg) (interactive "p") (swordmacs-continue arg)))
    ("b" "Back"               (lambda (arg) (interactive "p") (swordmacs-continue arg)))]
   ["Other"
    ("q" "quit" (lambda () (interactive) (transient-quit-all)))]])
;;
;;;; Hooks
;;
(add-hook 'org-ctrl-c-ctrl-c-final-hook
         (lambda () (if (swordmacs--in-block-p) (swordmacs-refresh-block))))
;;
(provide 'swordmacs)
;;
;;; swordmacs.el ends here
