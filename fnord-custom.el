;;; fnord-custom.el --- Customization options for the fnord theme

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

(require 'fnord-functions)

(defface fnord--loaded-dummy
  '((t (:weight normal)))
  "A dummy face we can use to check whether fnord theme was loaded.")


(defun fnord--theme-loaded-p ()
  "Return t when fnord theme has been loaded.
If you loaded another theme after fnord, this will still return t."
  (eq 'bold
      (face-attribute 'fnord--loaded-dummy :weight )))


(defun fnord--update-custom-face-attr (symbol value face attr)
  "Update ATTR for FACE after its customization has changed."
  (when (fnord--theme-loaded-p)
    (fnord--change-face-attr face attr value))
  (set-default-toplevel-value symbol value))


(defun fnord--update-custom-face-colour (symbol value face attr)
  "Update colour ATTR for FACE after its customization has changed."
  (when (fnord--theme-loaded-p)
    (fnord--change-face-colour face attr value))
  (set-default-toplevel-value symbol value))


(defun fnord--update-custom-face (symbol value face)
  "Set FACE to spec VALUE after customization of SYMBOL."
  (when (fnord--theme-loaded-p)
    (fnord--set-face face value))
  (set-default-toplevel-value symbol value))


(defun fnord--defface-spec (face-spec)
  "Generate a complete face spec from FACE-SPEC for use with `defface'.
Substitute integers for fnord theme colours, add class and correct structure."
  (let ((spec (fnord--subst-colours face-spec)))
    `((,fnord--class . ,spec))))


(defcustom fnord-comment-colour "#7b88a1"
  "Foreground colour for comment face.
The default setting for this colour is not in the original Nord palette.
You can specify an int, which will be taken to be one of the fnord theme
 colours, or a string with a custom colour code."
  :type '(choice (string :tag "fnord default" :value  "#7b88a1")
                 (integer :tag "nord default" :value 3)
                 (integer :tag "choose a theme colour")
                 (string :tag "custom colour string"))
  :group 'fnord-theme
  :set (lambda (symbol value)
         (fnord--update-custom-face-colour symbol value 'font-lock-comment-face :foreground)))

(defcustom fnord-region-face 'fnord-region-default
  "Fnord face spec for highlighting the region.

You can choose from predefined options, or specify a custom option.
If you use a sexp, you can use integers 1-15 for colour values and
they will be converted to fnord theme colours."
  :type '(choice (face :tag "fnord default" :value fnord-region-default)
                 (face :tag "fnord default secondary selection" :value fnord-secondary-selection-default)
                 (face :tag "frost" :value fnord-region-frost)
                 (face :tag "snowstorm" :value fnord-region-snowstorm)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(url-link "https://www.nordtheme.com/docs/ports/emacs/configuration")
  :set (lambda (symbol value)
         (fnord--update-custom-face-attr symbol value 'region :inherit)))



(defface fnord-region-default (fnord--defface-spec '(:foreground unspecified :background 2 :distant-foreground 4))
  "Default fnord face for region highlight.")
(defface fnord-secondary-selection-default (fnord--defface-spec '(:foreground unspecified :background 3 :distant-foreground 4))
  "Default fnord face for secondary selection highlight.")
(defface fnord-region-frost (fnord--defface-spec '(:background 8 :foreground 0 :distant-foreground 0))
  "Fnord face for frost region highlighting.")
(defface fnord-region-snowstorm (fnord--defface-spec '(:background 4 :foreground 0 :distant-foreground 0))
  "Fnord face for snowstorm region highlighting.")


(defcustom fnord-region-face 'fnord-region-default
  "Fnord face spec for highlighting the region.

You can choose from predefined options, or specify a custom option.
If you use a sexp, you can use integers 1-15 for colour values and
they will be converted to fnord theme colours."
  :type `(choice (face :tag "fnord default" :value fnord-region-default)
                 (face :tag "fnord default secondary selection" :value fnord-secondary-selection-default)
                 (face :tag "frost" :value fnord-region-frost)
                 (face :tag "snowstorm" :value fnord-region-snowstorm)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(url-link "https://www.nordtheme.com/docs/ports/emacs/configuration")
  :set (lambda (symbol value)
         (fnord--update-custom-face-attr symbol value 'region :inherit)))


(defcustom fnord-secondary-selection-face 'fnord-secondary-selection-default
  "Fnord face spec for highlighting the secondary selection."
  :type '(choice (face :tag "fnord default secondary selection" :value fnord-secondary-selection-default)
                 (face :tag "fnord default region" :value fnord-region-default)
                 (face :tag "frost" :value fnord-region-frost)
                 (face :tag "snowstorm" :value fnord-region-snowstorm)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(info-link "(emacs)Secondary Selection")
  :set (lambda (symbol value)
         (fnord--update-custom-face-attr symbol value 'secondary-selection :inherit)))


(defface fnord-paren-match-nord (fnord--defface-spec '(:background 3 :foreground 8))
  "Paren match face from the original Nord theme.")
(defface fnord-paren-match-brighter (fnord--defface-spec '(:background 10 :foreground 4))
  "Brighter default for paren match face.")


(defcustom fnord-paren-match-face 'fnord-paren-match-brighter
  "Fnord face spec for highlighting matching parens.

By default, fnord-theme uses a brighter face here to make it easier to
find a matching paren across a large piece of code."
  :type '(choice (face :tag "fnord default" :value fnord-paren-match-brighter)
                 (face :tag "nord" :value fnord-paren-match-nord)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(info-link "(emacs)Matching Parentheses")
  :set (lambda (symbol value)
         (fnord--update-custom-face-attr symbol value 'show-paren-match :inherit)))

(provide 'fnord-custom)

;;; fnord-custom.el ends here
