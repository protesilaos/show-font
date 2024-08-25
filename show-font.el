;;; show-font.el --- Show font features in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/show-font
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, writing, font

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WORK-IN-PROGRESS.  Show font features in a buffer.  This depends on
;; the font being available on the system.  Otherwise, the preview is
;; inaccurate.

;;; Code:

;; TODO 2024-08-24: How to find out if a font is available on the system?
;; TODO 2024-08-24: Produce informative message if font cannot be displayed.
;; TODO 2024-08-24: Offer option to install missing font.

(eval-when-compile (require 'cl-lib))

(defgroup show-font nil
  "Show font features in a buffer WORK-IN-PROGRESS."
  :group 'font)

(defcustom show-font-pangram 'prot
  "Pangram to display previewed font in."
  :package-version '(show-font . "0.1.0")
  :type '(choice
          (const :tag "The quick brown fox jumps over the lazy dog" fox)
          (const :tag "Grumpy wizards make toxic brew for the evil queen and jack" wizards)
          (const :tag "A quick movement of the enemy will jeopardize six gunboats" gunboats)
          (const :tag "Prot may find zesty owls join quiet vixens as the night beckons" prot)
          string)
  :group 'show-font)

(defcustom show-font-character-sample
  "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
"
  "Character sample to showcase font qualities."
  :package-version '(show-font . "0.1.0")
  :type 'string
  :group 'show-font)

(defgroup show-font-faces nil
  "Show font features in a buffer WORK-IN-PROGRESS."
  :group 'show-font)

(defface show-font-small '((t :height 0.8))
  "Face for font preview at small height."
  :group 'show-font-faces)

(defface show-font-regular '((t :height 1.0))
  "Face for font preview at regular height."
  :group 'show-font-faces)

(defface show-font-medium '((t :height 1.3))
  "Face for font preview at medium height."
  :group 'show-font-faces)

(defface show-font-large '((t :height 1.8))
  "Face for font preview at large height."
  :group 'show-font-faces)

(defface show-font-title '((t :inherit error :height 2.2))
  "Face for font preview title."
  :group 'show-font-faces)

(defface show-font-subtitle '((t :inherit bold :height 1.6))
  "Face for font preview title."
  :group 'show-font-faces)

;;;###autoload
(defconst show-font-extensions-regexp "\\.\\(ttf\\|otf\\)\\'"
  "Regular expression to match font file extensions.")

(defconst show-font-latin-alphabet
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
  "The latin alphabet as a list of strings.")

(defun show-font-pangram-p (string &optional characters)
  "Return non-nil if STRING is a pangram.
With optional CHARACTERS as a list of single character strings, test
that all of them occur at least once in STRING."
  (let ((missing-characters nil))
    (dolist (character (or characters show-font-latin-alphabet))
      (unless (string-match-p character string)
        (push character missing-characters)))
    (if (not missing-characters)
        t
      (message "Still missing: %s" (mapconcat #'identity missing-characters ", "))
      nil)))

;; NOTE 2024-08-24: Copied from `ready-player.el' by Alvaro Ramirez to
;; quickly put this package together.  I do not quite understand the
;; mechanics of file handling yet.  The idea of what I want is to get
;; an empty buffer.  Then I add contents there without making it
;; appear modified.
(defun show-font-handler (operation &rest args)
  "Handle the given I/O `file-name-handler-alist' OPERATION with ARGS.
Determine how to render the font file contents in a buffer."
  (pcase operation
    ('insert-file-contents
     (cl-destructuring-bind (filename visit _beg _end _replace) args
       (when visit
         (setq buffer-file-name filename))
       (list buffer-file-name (point-max))))
    ('file-attributes
     (let* ((file-name-handler-alist nil)
	        (attributes (apply #'file-name-non-special
                               (append (list operation) args))))
       ;; 7 is file size location
       ;; as per `file-attributes'.
       (setf (nth 7 attributes) 0)
       attributes))
    (_ (let ((inhibit-file-name-handlers
              (cons #'show-font-handler
                    (and (eq inhibit-file-name-operation operation)
                         inhibit-file-name-handlers)))
             (inhibit-file-name-operation operation))
         (apply operation args)))))

(defun show-font--get-attribute (attribute &optional file)
  "Get font family ATTRIBUTE from the current file or given FILE.
ATTRIBUTE is a string, such as \"family\" or \"fullname\", which is
matched against the output of the `fc-scan' executable."
  (unless (executable-find "fc-scan")
    (error "Cannot find `fc-scan' executable; will not render font"))
  (let ((f (or file buffer-file-name)))
    (when-let ((_ f)
               (_ (string-match-p show-font-extensions-regexp f))
               (output (shell-command-to-string (format "fc-scan %s" f)))
               (match (string-match (format "%s: \"\\(.*\\)\"" attribute) output))
               (found (match-string 1 output)))
      (if (string-match-p "\"(s)" found)
          (car (split-string found "\"(s)" :omit-nulls))
        found))))

(defun show-font--get-pangram ()
  "Return `show-font-pangram' or fallback string."
  (cond
   ((stringp show-font-pangram)
    show-font-pangram)
   ((eq show-font-pangram 'fox)
    "The quick brown fox jumps over the lazy dog")
   ((eq show-font-pangram 'wizards)
    "Grumpy wizards make toxic brew for the evil queen and jack")
   ((eq show-font-pangram 'gunboats)
    "A quick movement of the enemy will jeopardize six gunboats")
   ((eq show-font-pangram 'prot)
    "Prot may find zesty owls join quiet vixens as the night beckons")
   (t
    "No string or acceptable symbol value for `show-font-pangram', but this will do...")))

(defun show-font--prepare-text ()
  "Prepare pangram text at varying font heights."
  (let ((pangram (show-font--get-pangram))
        (faces '(show-font-small show-font-regular show-font-medium show-font-large))
        (list-of-lines nil)
        (list-of-blocks nil)
        (name (show-font--get-attribute "fullname"))
        (family (show-font--get-attribute "family")))
    (dolist (face faces)
      (push (propertize pangram 'face (list face :family family)) list-of-lines)
      (push (propertize show-font-character-sample 'face (list face :family family)) list-of-blocks))
    (concat
     (propertize name 'face (list 'show-font-title :family family)) "\n"
     (propertize (make-string (length name) ?-) 'face (list 'show-font-title :family family)) "\n"
     (propertize "Rendered with parent family:" 'face (list 'show-font-regular :family family)) "\n"
     (propertize family 'face (list 'show-font-subtitle :family family)) "\n"
     (propertize (make-string (length family) ?=) 'face (list 'show-font-subtitle :family family)) "\n\n"
     (mapconcat #'identity (nreverse list-of-lines) "\n") "\n"
     (mapconcat #'identity (nreverse list-of-blocks) "\n") "\n")))

(defmacro show-font-with-unmodified-buffer (&rest body)
  "Run BODY while not making the buffer appear modified."
  `(progn
     ,@body
     (set-buffer-modified-p nil)))

(defun show-font--add-text (&optional buffer)
  "Add the `show-font-pangram' as an overlay at `point-min'.
With optional BUFFER, operate therein.  Otherwise, do it in the current
buffer."
  (show-font-with-unmodified-buffer
   (with-current-buffer (or buffer (current-buffer))
     (let ((inhibit-read-only t))
       (save-excursion
         (insert (show-font--prepare-text)))))))

;;;###autoload
(define-derived-mode show-font-mode special-mode "Show Font"
  "Major mode to preview a font file's character set."
  (set-buffer-multibyte t)
  (setq buffer-read-only t)
  (visual-line-mode -1)
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (setq-local auto-save-default nil)
  (show-font--add-text))

;;;###autoload
(add-to-list 'file-name-handler-alist (cons show-font-extensions-regexp #'show-font-handler))

;;;###autoload
(add-to-list 'auto-mode-alist (cons show-font-extensions-regexp 'show-font-mode))

(provide 'show-font)
;;; show-font.el ends here
