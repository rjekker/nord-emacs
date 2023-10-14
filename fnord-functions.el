;;; fnord-functions.el --- Utility functions for the fnord theme

;; Copyright for portions of project fnord-theme are held by: Sven Greb
;; <development@svengreb.de> (https://www.svengreb.de) 2016-2023, as part
;; of project nordtheme/emacs (https://github.com/nordtheme/emacs). All
;; other copyright for project fnord-theme are held by Reindert-Jan Ekker
;; <info@rjekker.nl>, 2023-present.

;; Title: Fnord Theme
;; Project: fnord-theme
;; URL: https://github.com/rjekker/fnord-theme
;; Author: Reindert-Jan Ekker
;; Package-Requires: ((emacs "24"))
;; License: MIT

;;; Commentary:

;;; Code:

(require 'thingatpt)
(require 'fnord-colours)

(defconst fnord--class '((class color) (min-colors 257)))

(defun fnord--get-colour (n)
  "Retrieve nord colour N. Colour 16 is the special comment colour."
  (cl-assert (and (>= n 0) (< n 17)))
  (when n
    (nth n
         (list fnord-0 fnord-01 fnord-02 fnord-03 fnord-04 fnord-05 fnord-06 fnord-07 fnord-08 fnord-09 fnord-10 fnord-11 fnord-12 fnord-13 fnord-14 fnord-15 fnord-comment-colour))))


(defun fnord--subst-colours (face-spec)
  "If foreground or background in FACE-SPEC are ints, replace with fnord colour.
This now also supports the :color property of :box"
  (dolist (prop '(:foreground :background :distant-foreground :distant-background))
    (when-let ((val (plist-get face-spec prop)))
      (when (integerp val)
        (plist-put face-spec prop (fnord--get-colour val)))))
  (when-let ((underline_val (plist-get face-spec :underline)))
    (pcase underline_val
      ((pred integerp)
       ;; :underline given as integer color value
       (plist-put face-spec :underline (fnord--get-colour underline_val)))
      ((pred listp)
       ;; underline has list value that might include a color
       (when-let ((val (plist-get underline_val :color)))
         (when (integerp val)
           (plist-put underline_val :color (fnord--get-colour val)))))))
  
  (when-let ((box (plist-get face-spec :box)))
    (when-let ((val (plist-get box :color)))
      (when (integerp val)
        (plist-put box :color (fnord--get-colour val)))))
  face-spec)


(defun fnord--face (face-spec)
  "Create FACE-SPEC where foreground and background can be numbers.
Those will be substituted using `fnord--get-colour'."
  (let ((face (car face-spec))
        (spec (cdr face-spec)))
    `(,face ((,fnord--class . ,(fnord--subst-colours spec))))))


(defun fnord--set-face (face spec)
  "Set FACE to SPEC.
SPEC can be a fnord spec, with integers for colours."
  (apply #'face-spec-set (fnord--face (cons face spec))))

  
(defun fnord--test-face-spec-at-point ()
  "Utility for testing fnord face specs.
Run this with point before a face spec like \"(default :foreground 5)\",
to test it out."
  (interactive)
  (let ((sexp (car (read-from-string (substring-no-properties (thing-at-point 'sexp))))))
    (apply #'face-spec-set (fnord--face sexp))))


(defun fnord--change-face-attr (face attr value)
  "Set a single attribute ATTR of FACE to VALUE.
If VALUE is nil, it will be set to `unspecified'."
  (set-face-attribute face nil
                      attr
                      (pcase value
                        ((pred null) 'unspecified)
                        (_ value))))


(defun fnord--change-face-colour (face attr value)
  "Set a single colour attribute ATTR of FACE to VALUE.
Can pass ints for nord colours.
If VALUE is nil, it will be set to `unspecified'."
  (fnord--change-face-attr face attr
                           (pcase value
                             ((pred integerp) (fnord--get-colour value))
                             ((pred stringp)
                              (unless (string-prefix-p "#" value)
                                (error (format "%s is not a valid color code (should start with #)" value)))))))

(provide 'fnord-functions)

;;; fnord-functions.el ends here
