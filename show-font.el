;;; show-font.el --- Show font features in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/show-font
;; Version: 0.1.1
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
;; Show font features in a buffer.  This depends on the font being
;; available on the system.
;;
;; Consult the manual for further information.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup show-font nil
  "Show font features in a buffer."
  :group 'font
  :link '(info-link "(show-font) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/show-font"))

;; TODO 2024-09-06: How best to handle multiple languages?  Say there
;; is a font that only works with Greek characters.  We need to know
;; what characters the font supports.  Then we return the relevant
;; pangram and sample text.
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

;; See TODO above about multiple languages.
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

;;;; Faces

(defgroup show-font-faces nil
  "Show font features in a buffer."
  :group 'show-font
  :link '(info-link "(show-font) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/show-font"))

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

(defface show-font-misc '((t :inherit shadow))
  "Face for other, less important, elements in a preview.")

(defface show-font-button '((t :inherit button))
  "Face for buttons, like to install a missing font.")

;;;; Helper functions

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

;;;###autoload
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
    ;; the example shown in (info "(elisp) Magic File Names").
    (t (let ((inhibit-file-name-handlers
              (cons #'show-font-handler
                    (and (eq inhibit-file-name-operation operation)
                         inhibit-file-name-handlers)))
             (inhibit-file-name-operation operation))
         (apply operation args)))))

(defun show-font--get-attribute-from-file (attribute &optional file)
  "Get font family ATTRIBUTE from the current file or given FILE.
ATTRIBUTE is a string, such as \"family\" or \"fullname\", which is
matched against the output of the `fc-scan' executable."
  ;; TODO 2024-09-06: Make this work with other font backends.
  (unless (executable-find "fc-scan")
    (error "Cannot find `fc-scan' executable; will not render font"))
  (when-let ((f (or file buffer-file-name))
             (_ (string-match-p "\\.\\(ttf\\|otf\\)\\'" f))
             (output (shell-command-to-string (format "fc-scan -f \"%%{%s}\" %s"
                                                      (shell-quote-argument attribute)
                                                      (shell-quote-argument f)))))
    (if (string-match-p "," output)
        (car (split-string output ","))
      output)))

(defun show-font--get-installed-font-families (&optional full)
  "Return list of installed font families names.
With optional FULL, return the full XLFD representation instead."
  (sort
   (delete-dups
    (mapcar
     (lambda (font)
       (if full
           (aref font 6)
         (format "%s" (aref font 0))))
     (x-family-fonts)))
   #'string-lessp))

(defun show-font-installed-p (family)
  "Return non-nil if font family FAMILY is installed on the system.
FAMILY is a string like those of `show-font--get-installed-font-families'."
  (member family (show-font--get-installed-font-families)))

(defun show-font--get-installed-font-files ()
  "Get list of font files available on the system."
  (unless (executable-find "fc-list")
    (error "Cannot find `fc-list' executable; will not find installed fonts"))
  ;; TODO 2024-09-06: Make this work with other font backends.
  (process-lines "fc-list" "-f" (format "%%{%s}\n" "file")))

(defun show-font-installed-file-p (file)
  "Return non-nil if FILE is among `show-font--get-installed-font-files'."
  (member file (show-font--get-installed-font-files)))

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

(defun show-font--install-get-destination ()
  "Return directory where fonts can be copied locally."
  (cond
   ((member system-type '(gnu gnu/linux))
    (expand-file-name "~/.local/share/fonts/"))
   ((eq system-type 'darwin)
    (expand-file-name "~/Library/Fonts/"))
   (t
    (error "Unknown destination for Operating System of type `%s'" system-type))))

(defun show-font--install-confirmation (destination)
  "Prompt whether to copy the font to DESTINATION."
  (y-or-n-p (format "Install font by copying it to `%s'?" destination)))

(defun show-font--install (file)
  "Install the font FILE."
  (when-let ((destination (show-font--install-get-destination))
             (_ (show-font--install-confirmation destination)))
    (copy-file file destination 1) ; ask for confirmation to overwrite
    (message "Copied `%s' to `%s'; now updating the font cache" file destination)
    ;; TODO 2024-09-06: How to do the same on all operating systems?
    (shell-command-to-string (format "fc-cache -f -v"))
    (message "Font installed; restart Emacs to notice the effect")))

(defun show-font-install (&optional file)
  "Install font FILE locally.
FILE must be of type TTF or OTF and must not already be installed (per
`show-font-installed-file-p')."
  (let ((f (or file buffer-file-name)))
    (if (string-match-p "\\.\\(ttf\\|otf\\)\\'" f)
        (cond
         ((show-font-installed-file-p f)
          (user-error "`%s' is already installed; aborting" f))
         (t
          (show-font--install f)))
      (user-error "`%s' is not a known font file (TTF or OTF); aborting" f))))

(defun show-font--prepare-text (&optional family)
  "Prepare pangram text at varying font heights for the current font file.
With optional FAMILY, prepare a preview for the given font family
instead of that of the file."
  (cond
   ((not (display-graphic-p))
    (propertize "Fonts cannot be displayed in a terminal or TTY." 'face 'show-font-title))
   ((and (not family)
         (not (show-font-installed-file-p buffer-file-name)))
    nil)
   (t
    (let ((faces '(show-font-small show-font-regular show-font-medium show-font-large))
          (list-of-lines nil)
          (list-of-blocks nil)
          (pangram (show-font--get-pangram))
          (name (or family (show-font--get-attribute-from-file "fullname")))
          (family (or family (show-font--get-attribute-from-file "family"))))
      (dolist (face faces)
        (push (propertize pangram 'face (list face :family family)) list-of-lines)
        (push (propertize show-font-character-sample 'face (list face :family family)) list-of-blocks))
      (concat
       (propertize name 'face (list 'show-font-title :family family))
       "\n"
       (make-separator-line)
       (if (not (equal name family))
           (concat
            "\n"
            (propertize "Rendered with parent family:" 'face (list 'show-font-regular :family family))
            "\n"
            (propertize family 'face (list 'show-font-title-small :family family))
            "\n"
            (make-separator-line))
         "")
       "\n"
       (mapconcat #'identity (nreverse list-of-lines) "\n") "\n"
       (mapconcat #'identity (nreverse list-of-blocks) "\n") "\n")))))

(defun show-font--install-file-button (_button)
  "Wrapper for `show-font-install' to work as a button."
  (show-font-install))

(define-button-type 'show-font-installed-file-button
  'follow-link nil
  'action #'show-font--install-file-button
  'face 'show-font-button)

(defun show-font--insert-button ()
  "Insert `show-font-installed-file-button' at point."
  (insert
   (concat (propertize "Cannot preview this font" 'face 'show-font-title)
           "\n\n"
           (propertize buffer-file-name 'face 'bold)
           " is not installed"
           "\n\n"
           "Install this font file?"
           "\n"))
  (goto-char (point-max))
  (make-text-button (line-beginning-position 0) (line-end-position 0) :type 'show-font-installed-file-button))

(defun show-font--add-text (&optional buffer)
  "Add the `show-font-pangram' as an overlay at `point-min'.
With optional BUFFER, operate therein.  Otherwise, do it in the current
buffer."
  (with-silent-modifications
   (with-current-buffer (or buffer (current-buffer))
     (let ((inhibit-read-only t))
       (save-excursion
         (if-let ((text (show-font--prepare-text)))
             (insert text)
           (show-font--insert-button)))))))

(defmacro show-font-with-preview-buffer (name &rest body)
  "Evaluate BODY inside NAME buffer."
  (declare (indent 1))
  `(let ((buffer (get-buffer-create ,name)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         ,@body)
       (show-font-mode))
     (display-buffer buffer)))

;;;; Preview an installed font

(defvar show-font-select-preview-history nil
  "Minibuffer history for `show-font-select-preview'.")

(defun show-font--select-preview-prompt ()
  "Prompt for a font among `show-font--get-installed-font-families'."
  (let ((def (car show-font-select-preview-history)))
    (completing-read
     (format-prompt "Select font to preview" def)
     (show-font--get-installed-font-families))))

;;;###autoload
(defun show-font-select-preview (family)
  "Prepare a preview for font FAMILY.
When called interactively, prompt for FAMILY.  When called from Lisp,
FAMILY is a string that satisfies `show-font-installed-p'."
  (interactive
   (list
    (show-font--select-preview-prompt)))
  (if (show-font-installed-p family)
      (show-font-with-preview-buffer (format "*show-font preview of `%s'*" family)
        (save-excursion
          (insert (show-font--prepare-text family)))
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                      (show-font-select-preview family))))
    (error "The font family `%s' does not seem to be installed" family)))

;;;; Preview fonts in a list

;;;###autoload
(defun show-font-list ()
  "Produce a list of installed fonts with their preview.
The preview text is that of `show-font-pangram'."
  (declare (interactive-only t))
  (interactive)
  ;; FIXME 2024-09-06: Here we should only list fonts that can display
  ;; the pangram OR, better, we should have something appropriate to
  ;; show for them (e.g. emoji for the Emoji font).
  (show-font-with-preview-buffer "*show-font preview of all installed fonts*"
    (save-excursion
      (let* ((counter 0)
             (counter-string (lambda () (concat (number-to-string counter)  ". "))))
        (dolist (family (show-font--get-installed-font-families))
           (insert (concat
                    (propertize (funcall counter-string) 'face 'show-font-misc)
                    (propertize family 'face (list 'show-font-title-small :family family))
                    "\n"
                    (make-string (length (funcall counter-string)) ?\s)
                    (propertize (show-font--get-pangram) 'face (list 'show-font-regular :family family))))
           (insert "\n\n")
           (setq counter (+ counter 1)))))
    (setq-local revert-buffer-function
                (lambda (_ignore-auto _noconfirm)
                  (show-font-list)))))

;;;; Major mode to preview the font of the current TTF or OTF file

;;;###autoload
(define-derived-mode show-font-mode special-mode "Show Font"
  "Major mode to preview a font file's character set."
  (set-buffer-multibyte t)
  (setq-local truncate-lines t
              buffer-undo-list t
              auto-save-default nil
              buffer-read-only t)
  (display-line-numbers-mode -1)
  (visual-line-mode -1))

;;;###autoload
(add-to-list 'file-name-handler-alist (cons "\\.\\(ttf\\|otf\\)\\'" #'show-font-handler))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.\\(ttf\\|otf\\)\\'" 'show-font-mode))

(provide 'show-font)
;;; show-font.el ends here
