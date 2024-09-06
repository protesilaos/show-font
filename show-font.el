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

;; TODO 2024-08-24: Offer option to install missing font.
;; TODO 2024-08-27: Make this package work with other operating systems.

(eval-when-compile (require 'cl-lib))

(defgroup show-font nil
  "Show font features in a buffer WORK-IN-PROGRESS."
  :group 'font)

(defconst show-font-pangrams
  '((fox . "The quick brown fox jumps over the lazy dog")
    (wizards . "Grumpy wizards make toxic brew for the evil queen and jack")
    (gunboats . "A quick movement of the enemy will jeopardize six gunboats")
    (prot . "Prot may find zesty owls join quiet vixens as the night beckons"))
  "Default list of pangrams.")

(defcustom show-font-pangram 'prot
  "Pangram to display previewed font in.
This can be a symbol among the `car' of each element in
`show-font-pangrams' or it can be a string.  The string does not
actually need to be a pangram, though users can still have fun
experimenting with `show-font-pangram-p'."
  :package-version '(show-font . "0.1.0")
  :type `(choice
          ,@(mapcar
             (lambda (element)
               (list 'const :tag (cdr element) (car element)))
               show-font-pangrams)
          (string :tag "A custom pangram"))
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

(defconst show-font-title-common
  '((((class color) (min-colors 88) (background dark))
     :foreground "#fff29a")
    (((class color) (min-colors 88) (background light))
     :foreground "#873300")
    (t
     :inherit warning))
  "Common face attributes for titles.")

(defface show-font-title
  `((default :height 2.6)
    ,@show-font-title-common)
  "Face for font preview title."
  :group 'show-font-faces)

(defface show-font-title-small
  `((default :height 2.0)
    ,@show-font-title-common)
  "Face for smaller font preview title."
  :group 'show-font-faces)

;;;###autoload
(defconst show-font-extensions-regexp "\\.\\(ttf\\|otf\\)\\'"
  "Regular expression to match font file extensions.")

(defconst show-font-latin-alphabet
  (eval-when-compile (mapcar #'string (number-sequence ?a ?z)))
  "The latin alphabet as a list of strings.")

(defun show-font-pangram-p (string &optional characters)
  "Return non-nil if STRING is a pangram.
With optional CHARACTERS as a list of single character strings, test
that all of them occur at least once in STRING.

If there are characters missing from STRING, print them in a message and
return nil."
  (let ((missing-characters nil))
    (dolist (character (or characters show-font-latin-alphabet))
      (unless (string-match-p character string)
        (push character missing-characters)))
    (if (not missing-characters)
        t
      (message "Still missing: %s" (mapconcat #'identity missing-characters ", "))
      nil)))

(defun show-font-handler (operation &rest args)
  "Handle the given I/O `file-name-handler-alist' OPERATION with ARGS.
Determine how to render the font file contents in a buffer."
  (cond
   ((eq operation 'insert-file-contents)
    (when-let ((filename (car args))
               (visit (cadr args)))
         (setq buffer-file-name filename)
         (list buffer-file-name (point-max)))
     (show-font--add-text))
    ;; Handle any operation we do not know about.  This is copied from
    ;; the example show in (info "(elisp) Magic File Names").
    (t (let ((inhibit-file-name-handlers
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
  (when-let ((f (or file buffer-file-name))
             (_ (string-match-p show-font-extensions-regexp f))
             (output (shell-command-to-string (format "fc-scan -f \"%%{%s}\" %s"
                                                      (shell-quote-argument attribute)
                                                      (shell-quote-argument f)))))
    (if (string-match-p "," output)
        (car (split-string output ","))
      output)))

(defun show-font--get-installed-fonts (&optional attribute)
  "Get list of font families available on the system.
With optional ATTRIBUTE use it instead of \"family\"."
  (unless (executable-find "fc-list")
    (error "Cannot find `fc-list' executable; will not find installed fonts"))
  (process-lines
   "fc-list"
   "-f"
   (format "%%{%s}\n" (or attribute "file"))))

(defun show-font--installed-p (file)
  "Return non-nil if font FILE is installed on the system."
  (member file (show-font--get-installed-fonts)))

;; TODO 2024-09-06: Maybe we can rewrite `show-font--get-pangram' in some smart way to do this:
;;
;; `(cond
;;   ((stringp show-font-pangram)
;;    show-font-pangram)
;;   ,@(mapcar
;;      (lambda (element)
;;        (list `(eq show-font-pangram ',(car element)) (cdr element)))
;;      show-font-pangrams)
;;   (t
;;    "No string or acceptable symbol value for `show-font-pangram', but this will do...")))
;;
;; Can it be done without all the magic of `pcase' and friends?
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
  (let* ((pangram (show-font--get-pangram))
         (appeasement-message (concat "But here is a pangram to make you happy..." "\n\n" pangram)))
    (cond
     ((not (display-graphic-p))
      (concat (propertize "Fonts cannot be displayed in a terminal or TTY." 'face 'show-font-title)
              "\n\n" appeasement-message))
     ((not (show-font--installed-p buffer-file-name))
      (concat (propertize "Cannot preview this font" 'face 'show-font-title)
              "\n\n"
              (propertize buffer-file-name 'face 'bold)
              " is not installed"
              "\n\n" appeasement-message))
     (t
      (let ((faces '(show-font-small show-font-regular show-font-medium show-font-large))
            (list-of-lines nil)
            (list-of-blocks nil)
            (name (show-font--get-attribute "fullname"))
            (family (show-font--get-attribute "family")))
        (dolist (face faces)
          (push (propertize pangram 'face (list face :family family)) list-of-lines)
          (push (propertize show-font-character-sample 'face (list face :family family)) list-of-blocks))
        (concat
         (propertize name 'face (list 'show-font-title :family family))
         "\n"
         (make-separator-line)
         "\n"
         (propertize "Rendered with parent family:" 'face (list 'show-font-regular :family family))
         "\n"
         (propertize family 'face (list 'show-font-subtitle :family family))
         "\n"
         (make-separator-line)
         "\n"
         ;; Why not use `make-separator-line' here?
         (mapconcat #'identity (nreverse list-of-lines) "\n") "\n"
         (mapconcat #'identity (nreverse list-of-blocks) "\n") "\n"))))))

(defun show-font--add-text (&optional buffer)
  "Add the `show-font-pangram' as an overlay at `point-min'.
With optional BUFFER, operate therein.  Otherwise, do it in the current
buffer."
  (with-silent-modifications
   (with-current-buffer (or buffer (current-buffer))
     (let ((inhibit-read-only t))
       (save-excursion
         (insert (show-font--prepare-text)))))))

;;;###autoload
(define-derived-mode show-font-mode special-mode "Show Font"
  "Major mode to preview a font file's character set."
  (set-buffer-multibyte t)
  (setq-local truncate-lines t
              buffer-undo-list t
              auto-save-default nil
              buffer-read-only t)
  (visual-line-mode -1))

;; FIXME 2024-08-25: Do we want to autoload this or does it belong
;; somewhere else?  It seems wrong like this.

;;;###autoload
(add-to-list 'file-name-handler-alist (cons show-font-extensions-regexp #'show-font-handler))

;;;###autoload
(add-to-list 'auto-mode-alist (cons show-font-extensions-regexp 'show-font-mode))

(provide 'show-font)
;;; show-font.el ends here
