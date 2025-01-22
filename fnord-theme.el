;;; fnord-theme.el --- Nord theme, fixed -*- lexical-binding:t -*-

;; Copyright for portions of project fnord-theme are held by: Sven Greb
;; <development@svengreb.de> (https://www.svengreb.de) 2016-2023, as part
;; of project nordtheme/emacs (https://github.com/nordtheme/emacs). All
;; other copyright for project fnord-theme are held by Reindert-Jan Ekker
;; <info@rjekker.nl>, 2023-present.

;; Author: Reindert-Jan Ekker <info@rjekker.nl>
;; Maintainer: Reindert-Jan Ekker <info@rjekker.nl>
;; URL: https://github.com/rjekker/fnord-theme
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))
;; License: MIT
;; Keywords: faces

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This project implements the Fnord theme for the Emacs editor;
;; it is a fork of a similar project called nordtheme/emacs.

;; For more information see https://github.com/rjekker/fnord-theme

;;; Code:

(deftheme fnord "Nord theme, fixed.." :background-mode 'dark)

(unless (>= emacs-major-version 24)
  (error "Fnord-theme requires Emacs 24 or later"))

(require 'hl-line)

(defgroup fnord-theme nil
  "Fnord theme customizations.
Most settings in this group are applied immediately - no need to reload the theme."
  :group 'faces)

;;;; Defining the Fnord Theme Colours

(defgroup fnord-theme-colours nil
  "Colour values for the fnord theme.
These settings are NOT applied immediately - you will need to reload the theme."
  :group 'fnord-theme
  :link '(url-link "https://www.nordtheme.com/docs/colors-and-palettes"))

(defcustom fnord-theme-0 "#2E3440"
  "Fnord theme colour 0 - Polar Night (dark()). For background and area coloring."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-01 "#3B4252"
  "Fnord theme colour 1 - Polar night 1 (dark).
A brighter shade based on `fnord-theme-0'.
For elevated, more prominent or focused UI elements like

- status bars and text editor gutters
- panels, modals and floating popups like notifications or auto completion
- user interaction/form components like buttons, text/select fields or
  checkboxes

It also works fine for more inconspicuous and passive elements like borders
 or as dropshadow between different components."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-02 "#434C5E"
  "Fnord theme colour 2 - Polar night 2 (dark).
Used to colorize the currently active text editor line as well as selection-
and text highlighting color. It can also be used as an brighter variant for
the same target elements like `fnord-theme-01'."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-03 "#4C566A"
  "Fnord theme colour 3 - Polar night 3 (dark).
For UI elements like indent- and wrap guide marker. In the context of code
syntax highlighting it is used for comments and invisible/non-printable
 characters."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-04 "#D8DEE9"
  "Fnord theme colour 4 - Snow Storm 1 (light).
For UI elements like the text editor caret.
In the context of syntax highlighting it is used as text color for variables,
constants, attributes and fields."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-05 "#E5E9F0"
  "Fnord theme colour 5 - Snow Storm 2 (light).
Used for more subtle/inconspicuous UI text elements that do not need so much
visual attention. Other use cases are also state animations like a more brighter
text color when a button is hovered, active or focused."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-06 "#ECEFF4"
  "Fnord theme colour 6 - Snow Storm 3 (light).
For elevated UI text elements that require more visual attention.
In the context of syntax highlighting it is used as text color for plain text as
well as reserved and structuring syntax characters like curly- and square
brackets."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-07 "#8FBCBB"
  "Fnord theme colour 7 - Frost 1 (green).
A calm and highly contrasted color.
Used for UI elements that should, next to the primary accent color nord8, stand
out and get more visual attention.
In the context of syntax highlighting it is used for classes, types and
primitives."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-08 "#88C0D0"
  "Fnord theme colour 8 - Frost 2 (light blue)
Bright and shiny primary accent color.
Used for primary UI elements with main usage purposes that require the most
visual attention. In the context of syntax highlighting it is used for
declarations, calls and execution statements of functions, methods and routines."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-09 "#81A1C1"
  "Fnord theme colour 9 - Frost 3 (blue).
A more darkened and less saturated color.
Used for secondary UI elements that also require more visual attention than
other elements.In the context of syntax highlighting it is used for language
specific, syntactic and reserved keywords as well as

- support characters
- operators
- tags
- units
- punctuations like (semi)colons, points and commas."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-10 "#5E81AC"
  "Fnord theme colour 10 - Frost 4 (blue).
A dark and intensive color.
Used for tertiary UI elements that require more visual attention than default
elements. In the context of syntax highlighting it is used for pragmas, comment
keywords and pre-processor statements."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-11 "#BF616A"
  "Fnord theme colour 11 - Aurora 1 (red).
Used for UI elements that are rendering error states like linter markers and the
highlighting of Git diff deletions. In the context of syntax highlighting it is
used to override the highlighting of syntax elements that are detected as
errors."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-12 "#D08770"
  "Fnord theme colour 12 - Aurora 2 (orange).
Rarely used for UI elements, but it may indicate a more advanced or dangerous
functionality. In the context of syntax highlighting it is used for special
syntax elements like annotations and decorators."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-13 "#EBCB8B"
  "Fnord theme colour 13 - Aurora 3 (yellow).
Used for UI elements that are rendering warning states like linter markers and
the highlighting of Git diff modifications. In the context of syntax
highlighting it is used to override the highlighting of syntax elements that are
detected as warnings as well as escape characters and within regular
expressions."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-14 "#A3BE8C"
  "Fnord theme colour 14 - Aurora 4 (light green).
Used for UI elements that are rendering success states and visualizations and
the highlighting of Git diff additions. In the context of syntax highlighting
it is used as main color for strings of any type like double/single quoted or
interpolated."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-theme-15 "#B48EAD"
  "Fnord theme colour 15 - Aurora 5 (purple).
Rarely used for UI elements, but it may indicate a more uncommon functionality.
In the context of syntax highlighting it is used as main color for numbers of
any type like integers and floating point numbers."
  :type 'string
  :group 'fnord-theme-colours)

;;;; Function definitions

(defconst fnord-theme--class '((class color) (min-colors 257)))

(defun fnord-theme--get-colour (n)
  "Retrieve nord colour N. Colour 16 is the special comment colour."
  (unless (and (>= n 0) (< n 16))
    (error "Fnord theme colours must be in the range 0..15"))
  (when n
    (nth n
         (list fnord-theme-0 fnord-theme-01 fnord-theme-02 fnord-theme-03 fnord-theme-04 fnord-theme-05 fnord-theme-06 fnord-theme-07 fnord-theme-08 fnord-theme-09 fnord-theme-10 fnord-theme-11 fnord-theme-12 fnord-theme-13 fnord-theme-14 fnord-theme-15))))


(defun fnord-theme--subst-colours (face-spec)
  "If foreground or background in FACE-SPEC are ints, replace with fnord colour.
This now also supports the :color property of :box"
  (dolist (prop '(:foreground :background :distant-foreground :distant-background))
    (when-let ((val (plist-get face-spec prop)))
      (when (integerp val)
        (plist-put face-spec prop (fnord-theme--get-colour val)))))
  (when-let ((underline_val (plist-get face-spec :underline)))
    (pcase underline_val
      ((pred integerp)
       ;; :underline given as integer color value
       (plist-put face-spec :underline (fnord-theme--get-colour underline_val)))
      ((pred listp)
       ;; underline has list value that might include a color
       (when-let ((val (plist-get underline_val :color)))
         (when (integerp val)
           (plist-put underline_val :color (fnord-theme--get-colour val)))))))
  
  (when-let ((box (plist-get face-spec :box)))
    (when-let ((val (plist-get box :color)))
      (when (integerp val)
        (plist-put box :color (fnord-theme--get-colour val)))))
  face-spec)


(defun fnord-theme--face (face-spec)
  "Create FACE-SPEC where foreground and background can be numbers.
Those will be substituted using `fnord-theme--get-colour'."
  (let ((face (car face-spec))
        (spec (cdr face-spec)))
    `(,face ((,fnord-theme--class . ,(fnord-theme--subst-colours spec))))))


(defun fnord-theme--set-face (face spec)
  "Set FACE to SPEC.
SPEC can be a fnord spec, with integers for colours."
  (apply #'face-spec-set (fnord-theme--face (cons face spec))))

  
(defun fnord-theme--test-face-spec-at-point ()
  "Utility for testing fnord face specs.
Run this with point before a face spec like \"(default :foreground 5)\",
to test it out."
  (interactive)
  (require 'thingatpt)
  (let ((sexp (car (read-from-string (substring-no-properties (thing-at-point 'sexp))))))
    (apply #'face-spec-set (fnord-theme--face sexp))))


(defun fnord-theme--change-face-attr (face attr value)
  "Set a single attribute ATTR of FACE to VALUE.
If VALUE is nil, it will be set to `unspecified'."
  (set-face-attribute face nil
                      attr
                      (pcase value
                        ((pred null) 'unspecified)
                        (_ value))))


(defun fnord-theme--change-face-colour (face attr value)
  "Set a single colour attribute ATTR of FACE to VALUE.
Can pass ints for nord colours.
If VALUE is nil, it will be set to `unspecified'."
  (fnord-theme--change-face-attr face attr
                           (pcase value
                             ((pred integerp) (fnord-theme--get-colour value))
                             ((pred stringp)
                              (if (not (string-prefix-p "#" value))
                                  (error (format "%s is not a valid color code (should start with #)" value))
                                value)))))

(defface fnord-theme--loaded-dummy
  '((t (:weight normal)))
  "A dummy face we can use to check whether fnord theme was loaded.")


(defun fnord-theme--loaded-p ()
  "Return t when fnord theme has been loaded.
If you loaded another theme after fnord, this will still return t."
  (eq 'bold
      (face-attribute 'fnord-theme--loaded-dummy :weight )))


(defun fnord-theme--update-custom-face-attr (symbol value face attr)
  "Update ATTR for FACE after its customization has changed.

'SYMBOL and VALUE are set by `defcustom'."
  (when (fnord-theme--loaded-p)
    (fnord-theme--change-face-attr face attr value))
  (set-default-toplevel-value symbol value))


(defun fnord-theme--update-custom-face-colour (symbol value face attr)
  "Update colour ATTR for FACE after its customization has changed.

'SYMBOL and VALUE are set by `defcustom'."
  (when (fnord-theme--loaded-p)
    (fnord-theme--change-face-colour face attr value))
  (set-default-toplevel-value symbol value))


(defun fnord-theme--update-custom-face (symbol value face)
  "Set FACE to spec VALUE after customization of SYMBOL."
  (when (fnord-theme--loaded-p)
    (fnord-theme--set-face face value))
  (set-default-toplevel-value symbol value))


(defun fnord-theme--defface-spec (face-spec)
  "Generate a complete face spec from FACE-SPEC for use with `defface'.
Substitute integers for fnord theme colours, add class and correct structure."
  (let ((spec (fnord-theme--subst-colours face-spec)))
    `((,fnord-theme--class . ,spec))))

;;;; Support for hl-line
;;;;; (when the mark gets set we deactivate hl-line)

(defun fnord-theme--activate-hl-line-toggle (symbol value)
  "Activate smart toggle behaviour for hl-line as mark is activated/deactivated.

'SYMBOL and VALUE are set by `defcustom'."
  (if (and value (featurep 'hl-line))
      (progn
        (add-hook 'activate-mark-hook #'fnord-theme--hl-line-off)
        (add-hook 'deactivate-mark-hook #'fnord-theme--hl-line-on))
    (remove-hook 'activate-mark-hook #'fnord-theme--hl-line-off)
    (remove-hook 'deactivate-mark-hook #'fnord-theme--hl-line-on))
  (set-default-toplevel-value symbol value))


(defcustom fnord-theme-no-hl-line-when-region-active nil
  "Temporarily turn `hl-line-mode' off when region is active.

This is only necessary when using `hl-line-mode' and `transient-mark-mode'.
The default region face has low contrast with the face for hl-line.
When this is t, we make the hl-line invisible while a mark is active."
  :type 'boolean
  :group 'fnord-theme-theme
  :set #'fnord-theme--activate-hl-line-toggle)

(defun fnord-theme--hl-line-off ()
  "Turn hl-line mode off."
  (when (require 'hl-line nil 'noerror)
    (overlay-put
     (cond
      ((local-variable-p 'hl-line-overlay) hl-line-overlay)
      ((local-variable-p 'global-hl-line-overlay) global-hl-line-overlay))
     'face nil)))

(defun fnord-theme--hl-line-on ()
  "Turn hl-line mode on."
  (when (require 'hl-line nil 'noerror)
    (overlay-put
     (cond
      ((local-variable-p 'hl-line-overlay) hl-line-overlay)
      ((local-variable-p 'global-hl-line-overlay) global-hl-line-overlay))
     'face hl-line-face)))

;;;; Default/preset faces

(defface fnord-theme-region-default
  (fnord-theme--defface-spec '(:foreground unspecified :background 2 :distant-foreground 4))
  "Default fnord face for region highlight.")

(defface fnord-theme-secondary-selection-default
  (fnord-theme--defface-spec '(:foreground unspecified :background 3 :distant-foreground 4))
  "Default fnord face for secondary selection highlight.")

(defface fnord-theme-region-frost
  (fnord-theme--defface-spec '(:background 8 :foreground 0 :distant-foreground 0))
  "Fnord face for frost region highlighting.")

(defface fnord-theme-region-snowstorm
  (fnord-theme--defface-spec '(:background 4 :foreground 0 :distant-foreground 0))
  "Fnord face for snowstorm region highlighting.")

(defface fnord-theme-paren-match-nord
  (fnord-theme--defface-spec '(:background 3 :foreground 8))
  "Paren match face from the original Nord theme.")

(defface fnord-theme-paren-match-brighter
  (fnord-theme--defface-spec '(:background 10 :foreground 6))
  "Brighter default for paren match face.")

;;;; Other customizable faces and colours

(defcustom fnord-theme-comment-colour "#7b88a1"
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
         (fnord-theme--update-custom-face-colour symbol value 'font-lock-comment-face :foreground)))


(defcustom fnord-theme-dark-green "#6f9252"
  "Dark green - mostly used with diffs.
The default setting for this colour is not in the original Nord palette.
You can specify an int, which will be taken to be one of the fnord theme
 colours, or a string with a custom colour code."
  :type '(choice (string :tag "fnord default" :value  "#6f9252")
                 (integer :tag "choose a theme colour")
                 (string :tag "custom colour string"))
  :group 'fnord-theme)


(defcustom fnord-theme-dark-red "#84363d"
  "Darker red - mostly used with diffs.
The default setting for this colour is not in the original Nord palette.
You can specify an int, which will be taken to be one of the fnord theme
 colours, or a string with a custom colour code."
  :type '(choice (string :tag "fnord default" :value  "#60272c")
                 (integer :tag "choose a theme colour")
                 (string :tag "custom colour string"))
  :group 'fnord-theme)



(defcustom fnord-theme-darker-green "#577140"
  "Darker green - mostly used with diffs.
The default setting for this colour is not in the original Nord palette.
You can specify an int, which will be taken to be one of the fnord theme
 colours, or a string with a custom colour code."
  :type '(choice (string :tag "fnord default" :value  "#3e512e")
                 (integer :tag "choose a theme colour")
                 (string :tag "custom colour string"))
  :group 'fnord-theme)


(defcustom fnord-theme-darker-red "#60272c"
  "Darker red - mostly used with diffs.
The default setting for this colour is not in the original Nord palette.
You can specify an int, which will be taken to be one of the fnord theme
 colours, or a string with a custom colour code."
  :type '(choice (string :tag "fnord default" :value  "#60272c")
                 (integer :tag "choose a theme colour")
                 (string :tag "custom colour string"))
  :group 'fnord-theme)


(defcustom fnord-theme-region-face 'fnord-theme-region-default
  "Fnord face spec for highlighting the region.

You can choose from predefined options, or specify a custom option.
If you use a sexp, you can use integers 1-15 for colour values and
they will be converted to fnord theme colours."
  :type '(choice (face :tag "fnord default" :value fnord-theme-region-default)
                 (face :tag "fnord default secondary selection" :value fnord-theme-secondary-selection-default)
                 (face :tag "frost" :value fnord-theme-region-frost)
                 (face :tag "snowstorm" :value fnord-theme-region-snowstorm)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(url-link "https://www.nordtheme.com/docs/ports/emacs/configuration")
  :set (lambda (symbol value)
         (fnord-theme--update-custom-face-attr symbol value 'region :inherit)))


(defcustom fnord-theme-region-face 'fnord-theme-region-default
  "Fnord face spec for highlighting the region.

You can choose from predefined options, or specify a custom option.
If you use a sexp, you can use integers 1-15 for colour values and
they will be converted to fnord theme colours."
  :type `(choice (face :tag "fnord default" :value fnord-theme-region-default)
                 (face :tag "fnord default secondary selection" :value fnord-theme-secondary-selection-default)
                 (face :tag "frost" :value fnord-theme-region-frost)
                 (face :tag "snowstorm" :value fnord-theme-region-snowstorm)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(url-link "https://www.nordtheme.com/docs/ports/emacs/configuration")
  :set (lambda (symbol value)
         (fnord-theme--update-custom-face-attr symbol value 'region :inherit)))


(defcustom fnord-theme-paren-match-face 'fnord-theme-paren-match-brighter
  "Fnord face spec for highlighting matching parens.

By default, fnord-theme uses a brighter face here to make it easier to
find a matching paren across a large piece of code."
  :type '(choice (face :tag "fnord default" :value fnord-theme-paren-match-brighter)
                 (face :tag "nord" :value fnord-theme-paren-match-nord)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(info-link "(emacs)Matching Parentheses")
  :set (lambda (symbol value)
         (fnord-theme--update-custom-face-attr symbol value 'show-paren-match :inherit)))


(defcustom fnord-theme-secondary-selection-face 'fnord-theme-secondary-selection-default
  "Fnord face spec for highlighting the secondary selection."
  :type '(choice (face :tag "fnord default secondary selection" :value fnord-theme-secondary-selection-default)
                 (face :tag "fnord default region" :value fnord-theme-region-default)
                 (face :tag "frost" :value fnord-theme-region-frost)
                 (face :tag "snowstorm" :value fnord-theme-region-snowstorm)
                 (face :tag "custom face"))
  :group 'fnord-theme
  :link '(info-link "(emacs)Secondary Selection")
  :set (lambda (symbol value)
         (fnord-theme--update-custom-face-attr symbol value 'secondary-selection :inherit)))

;;;; Defining the faces for the Fnord theme

(defvar fnord-theme--faces
  `(;; add a dummy face so we can detect whether theme was loaded
    (fnord-theme--loaded-dummy :weight bold)

    (bold :weight bold)
    (bold-italic :weight bold :slant italic)
    (bookmark-face :foreground 12 :distant-foreground 12)
    (cursor :background 4)
    (default :foreground 6 :background 0)
    (error :foreground 11 :weight bold)
    
    (escape-glyph :foreground 12)
    (homoglyph :foreground 15)
    (nobreak-hyphen :foreground 15)
    (nobreak-space :foreground 3)
    (italic :slant italic)
    (link :underline t :foreground 8)
    (link-visited :underline t :foreground 10)
    (underline :underline t)
    (shadow :foreground 4)
    (warning :foreground 13 :weight bold)
    (success :foreground 14)
    (textsec-suspicious :background 11 :foreground 6)
    (tooltip :background 3 :foreground 4)
    
    ;; edit keyboard macro
    (edmacro-label :foreground 7 :weight normal)
    
    ;; highlighting and selections
    (highlight :foreground 8 :background 3)
    (hl-line :background 1 :distant-foreground 4)
    (hl-todo :foreground 11 :weight bold)
    (next-error :inherit highlight)
    (trailing-whitespace :background 10)

    ;; current match
    (match :foreground 0 :background 8)
    ;; related matches
    (lazy-highlight :foreground 4 :background 10)

    ;; region
    (region :inherit ,fnord-theme-region-face)
    (secondary-selection :inherit ,fnord-theme-secondary-selection-face)
    
    ;; search/replace
    (isearch :inherit match)
    (isearch-group-1 :foreground 0 :background 12)
    (isearch-group-2 :foreground 0 :background 13)
    (isearch-fail :foreground 12)
    (query-replace :inherit match)

    (show-paren-match :inherit ,fnord-theme-paren-match-face)
    (show-paren-mismatch :foreground 6 :background 11)
    
    ;; windows and frames
    (border :foreground 4)
    (fringe :foreground 4 :background 0)
    (header-line :foreground 4 :background 2)
    (minibuffer-prompt :foreground 8 :weight bold)
    (mm-command-output :foreground 8)
    (mode-line :foreground 8 :background 3)
    (mode-line-buffer-id :weight bold)
    (mode-line-highlight :inherit highlight)
    (mode-line-inactive :foreground 4 :background 1)

    (scroll-bar :background 3)
    (tool-bar :foreground 4 :background 3)
    (tooltip :foreground 0 :background 4)
    (window-divider :background 3)
    (window-divider-first-pixel :background 3)
    (window-divider-last-pixel :background 3)

    (completions-annotations :foreground 9)
    (completions-common-part :foreground 8 :weight bold)
    (completions-first-difference :inherit isearch-fail)
    (completions-group-title :foreground 7)
    
    (font-lock-builtin-face :foreground 9)
    (font-lock-comment-face :foreground ,fnord-theme-comment-colour)
    (font-lock-comment-delimiter-face :inherit font-lock-comment-face)
    (font-lock-constant-face :foreground 9)
    (font-lock-doc-face :inherit font-lock-comment-face)
    (font-lock-function-name-face :foreground 8)
    (font-lock-keyword-face :foreground 9)
    (font-lock-negation-char-face :foreground 9)
    (font-lock-number-face :foreground 15)
    (font-lock-preprocessor-face :foreground 10 :weight bold)
    (font-lock-reference-face :foreground 9)
    (font-lock-regexp-grouping-backslash :foreground 13)
    (font-lock-regexp-grouping-construct :foreground 13)
    (font-lock-string-face :foreground 14)
    (font-lock-type-face :foreground 7)
    (font-lock-variable-name-face :foreground 6)
    (font-lock-warning-face :foreground 13)

    (hi-aquamarine :background 7 :distant-foreground 0)
    (hi-blue :background 8 :distant-foreground 0)
    (hi-blue-b :foreground 8 :weight bold)
    (hi-green :background 14 :distant-foreground 0)
    (hi-green-b :foreground 14 :distant-foreground 0)
    (hi-pink :background 11 :foreground 4)
    (hi-red-b :foreground 11 :weight bold)
    (hi-salmon :background 12 :distant-foreground 0)
    (hi-yellow :background 13 :distant-foreground 0)
    
    (tty-menu-disabled-face :foreground 1)
    (tty-menu-enabled-face :background 2 :foreground 4)
    (tty-menu-selected-face :foreground 8 :underline t)

    (calendar-month-header :foreground 7)
    (calendar-weekday-header :foreground 6)
    (calendar-weekend-header :foreground 15)
    (calendar-today :foreground 14 :weight bold)
    (diary :foreground 7)
    (diary-anniversary :foreground 13)
    (holiday :foreground 10 :background 0)
    
    (compilation-mode-line-exit :foreground 14)
    (compilation-mode-line-fail :foreground 11)

    (confusingly-reordered :underline (:style wave :color 11))

    
    (buffer-menu-buffer :foreground 7 :weight bold)
    (ibuffer-locked-buffer :foreground 11)
    (file-name-shadow :inherit shadow)

    (button :background 0 :foreground 8 :box (:line-width 1 :color 9 :style released-button))
    (widget-button-pressed :background 3 :foreground 7 :box (:line-width 1 :color 9 :style pressed-button))
    (widget-documentation :foreground 4)
    (widget-inactive :foreground 3)
    (widget-field :background 2 :foreground 4)
    (widget-single-line-field :inherit widget-field)

    (custom-button :inherit button)
    (custom-button-mouse :inherit widget-button-pressed)
    (custom-button-pressed :inherit widget-button-pressed)
    (custom-button-pressed-unraised  :background 3 :foreground 7 :box (:line-width 1 :color 9 :style flat-button))
    (custom-button-unraised :background 0 :foreground 8 :box (:line-width 1 :color 9 :style flat-button))
    (custom-changed :foreground 13)
    (custom-comment :inherit font-lock-comment-face)
    (custom-comment-tag :foreground 7)
    (custom-documentation :foreground 4)
    (custom-group-tag :foreground 8 :weight bold)
    (custom-group-tag-1 :foreground 8 :weight bold)
    (custom-invalid :foreground 11)
    (custom-link :inherit link)
    (custom-modified :foreground 13)
    (custom-rogue :foreground 12 :background 2)
    (custom-saved :foreground 14)
    (custom-set :foreground 8)
    (custom-state :foreground 14)
    (custom-themed :foreground 8 :background 2)
    (custom-visibility :foreground 9)

    (dired-broken-symlink :foreground 11)
    (dired-ignored :inherit font-lock-comment-face)
    (dired-mark :foreground 7)
    (dired-perm-write :foreground 11)
    (dired-symlink :foreground 14)
    (dired-special :foreground 15)
    (dired-directory :foreground 8)
    
    (help-argument-name :foreground 8)
    (help-for-help-header :foreground 7 :height 1)
    (help-key-binding :inherit button)
    (shortdoc-heading :inherit info-menu-header)
    
    (info-title-4 :foreground 6 :weight bold)
    (info-title-1 :height 1.2 :inherit info-title-2)
    (info-title-2 :height 1.2 :inherit info-title-3)
    (info-title-3 :height 1.2 :inherit info-title-4)
    (info-menu-header :height 1.2 :foreground 6 :weight bold)
    (Info-quoted :foreground 6)
    (info-menu-star :foreground 15)

    ;; linum deprecated in emacs 29
    (linum :foreground 3 :background 0)
    (linum-relative-current-face :foreground 3 :background 0)
    (line-number-current-line :foreground 6)
    
    (icomplete-first-match :foreground 14 :weight bold)
    (icomplete-section :foreground 7 :slant normal)
    
    (elisp-shorthand-font-lock-face :foreground 15)
    
    ;; easypg
    (epa-validity-high :foreground 7 :weight normal)
    (epa-validity-medium :foreground 8 :weight normal :slant normal)
    (epa-validity-low :foreground 13 :weight normal)
    (epa-validity-disabled :foreground 11 :background 2)
    (epa-mark :foreground 13)
    (epa-string :inherit font-lock-string-face)
    (epa-field-body :foreground 8)
    (epa-field-name :foreground 7)
    
    (ido-indicator :foreground 12)
    (ido-subdir :foreground 9)
    (ido-virtual :foreground 15)
    (ido-first-match :foreground 14 :weight bold)
    (ido-only-match :foreground 14 :weight bold)
    
    (org-level-1 :foreground 7 :weight extra-bold)
    (org-level-2 :foreground 8 :weight bold)
    (org-level-3 :foreground 9 :weight semi-bold)
    (org-level-4 :foreground 7 :weight normal)
    (org-level-5 :foreground 8)
    (org-level-6 :foreground 9)
    (org-level-7 :foreground 7)
    (org-level-8 :foreground 8)
    (org-level-8 :foreground 9)
    (org-agenda-structure :foreground 7 :weight bold)
    (org-agenda-date :foreground 8 :underline nil)
    (org-agenda-date-weekend :foreground 9)
    (org-agenda-date-today :foreground 8 :weight bold)
    (org-agenda-done :foreground 14)
    (org-agenda-dimmed-todo-face :background 13)
    (org-block :background 1)
    (org-block-begin-line :foreground 10 :background 1)
    (org-block-end-line :foreground 10 :background 1)
    (org-checkbox :foreground 7)
    (org-checkbox-statistics-done :foreground 14)
    (org-checkbox-statistics-todo :foreground 13)
    (org-code :foreground 7)
    (org-column :background 2)
    (org-column-title :background 2 :weight bold :underline t)
    (org-date :foreground 8)
    (org-document-info :foreground 4)
    (org-document-info-keyword :foreground 10 :weight bold)
    (org-document-title :foreground 7 :weight bold)
    (org-drawer :foreground 10)
    (org-done :foreground 14 :weight bold)
    (org-ellipsis :foreground 3)
    (org-footnote :foreground 8)
    (org-formula :foreground 9)
    (org-headline-done :foreground 10)
    (org-hide :foreground 0 :background 0)
    (org-link :inherit link)
    (org-meta-line :foreground 10)
    (org-priority :foreground 15)
    (org-property-value :foreground 9)
    (org-scheduled :foreground 13)
    (org-scheduled-previously :foreground 13)
    (org-scheduled-today :foreground 12)
    (org-special-keyword :foreground 9)
    (org-table :foreground 4 :background 1)
    (org-tag :foreground 10)
    (org-todo :foreground 13 :weight bold)
    (org-upcoming-deadline :foreground 12)
    (org-warning :foreground 13 :weight bold)
    (org-sexp-date :foreground 7)
    (org-quote :background 1 :slant italic)
    (org-verse :background 1 :slant italic)
    (org-verbatim :foreground 7)

    (outline-1 :foreground 7 :weight bold)
    (outline-2 :foreground 8 :weight bold)
    (outline-3 :foreground 9 :weight bold)
    (outline-4 :foreground 7 :weight bold)
    (outline-5 :foreground 8 :weight bold)
    (outline-6 :foreground 9 :weight bold)
    (outline-7 :foreground 7 :weight bold)
    (outline-8 :foreground 8 :weight bold)
    
    (package-description :foreground 4)
    (package-help-section-name :foreground 8 :weight bold)
    (package-name :foreground 8)
    (package-status-available :foreground 7)
    (package-status-avail-obso :foreground 7 :slant italic)
    (package-status-built-in :foreground 9)
    (package-status-dependency :foreground 8 :slant italic)
    (package-status-disabled :foreground 3)
    (package-status-external :foreground 12 :slant italic)
    (package-status-held :foreground 4 :weight bold)
    (package-status-new :foreground 14)
    (package-status-incompat :foreground 11)
    (package-status-installed :foreground 7 :weight bold)
    (package-status-unsigned :foreground 13)
    
    (term :foreground 4 :background 0)
    (term-color-black :foreground 1 :background 1)
    (term-color-white :foreground 5 :background 5)
    (term-color-cyan :foreground 7 :background 7)
    (term-color-blue :foreground 8 :background 8)
    (term-color-red :foreground 11 :background 11)
    (term-color-yellow :foreground 13 :background 13)
    (term-color-green :foreground 14 :background 14)
    (term-color-magenta :foreground 15 :background 15)
    (ansi-color-black :foreground 1 :background 1)
    (ansi-color-white :foreground 5 :background 5)
    (ansi-color-cyan :foreground 7 :background 7)
    (ansi-color-blue :foreground 8 :background 8)
    (ansi-color-red :foreground 11 :background 11)
    (ansi-color-yellow :foreground 13 :background 13)
    (ansi-color-green :foreground 14 :background 14)
    (ansi-color-magenta :foreground 15 :background 15)
    (ansi-color-bright-black :foreground 1 :background 1)
    (ansi-color-bright-white :foreground 5 :background 5)
    (ansi-color-bright-cyan :foreground 7 :background 7)
    (ansi-color-bright-blue :foreground 8 :background 8)
    (ansi-color-bright-red :foreground 11 :background 11)
    (ansi-color-bright-yellow :foreground 13 :background 13)
    (ansi-color-bright-green :foreground 14 :background 14)
    (ansi-color-bright-magenta :foreground 15 :background 15)
    
    (undo-tree-visualizer-current-face :foreground 8)
    (undo-tree-visualizer-default-face :foreground 4)
    (undo-tree-visualizer-unmodified-face :foreground 4)
    (undo-tree-visualizer-register-face :foreground 9)
    
    (vc-conflict-state :foreground 12)
    (vc-edited-state :foreground 13)
    (vc-locally-added-state :underline 14)
    (vc-locked-state :foreground 10)
    (vc-missing-state :foreground 11)
    
    (vcs-missing-state :foreground 11)
    (vc-needs-update-state :foreground 12)
    (vc-removed-state :foreground 11)
    (vc-state-base :foreground 4)
    (vc-up-to-date-state :foreground 8)
    (vertical-border :foreground 2)
  
    (which-func :foreground 8)

    (whitespace-big-indent :inherit trailing-whitespace)
    (whitespace-empty :inherit trailing-whitespace)
    (whitespace-hspace :inherit trailing-whitespace)
    (whitespace-indentation :inherit trailing-whitespace)
    (whitespace-line :inherit trailing-whitespace)
    (whitespace-newline :inherit trailing-whitespace)
    (whitespace-missing-newline-at-eof :inherit trailing-whitespace)
    (whitespace-space :inherit trailing-whitespace)
    (whitespace-space-after-tab :inherit trailing-whitespace)
    (whitespace-space-before-tab :inherit trailing-whitespace)
    (whitespace-tab :inherit trailing-whitespace)
    (whitespace-trailing :inherit trailing-whitespace)

    (tab-bar :inherit mode-line-inactive)
    (tab-bar-tab :inherit mode-line-highlight)
    (tab-bar-tab-inactive :inherit tab-bar)

    (imenu-list-entry-face-0 :foreground 7)
    (imenu-list-entry-face-1 :foreground 8)
    (imenu-list-entry-face-2 :foreground 9)
    (imenu-list-entry-face-3 :foreground 10)

    ;; orderless
    (orderless-match-face-0 :foreground 14)
    (orderless-match-face-1 :foreground 13)
    (orderless-match-face-2 :foreground 12)
    (orderless-match-face-3 :foreground 11)
    (orderless-match-face-4 :foreground 15)

    ;; anzu
    (anzu-mode-line :foreground 8)
    (anzu-mode-line-no-match :foreground 11)
    (anzu-match-1 :inherit isearch-group-1)
    (anzu-match-2 :inherit isearch-group-2)
    (anzu-match-3 :background 15)

    ;; Flycheck
    (flycheck-error :underline (:style wave :color 11))
    (flycheck-fringe-error :foreground 11 :weight bold)
    (flycheck-fringe-info :foreground 8 :weight bold)
    (flycheck-fringe-warning :foreground 13 :weight bold)
    (flycheck-info :underline (:style wave :color 8))
    (flycheck-warning :underline (:style wave :color 13))

    ;; Rainbow Delimiters
    ;; trying to get some distance between the colours
    (rainbow-delimiters-depth-1-face :foreground 4)
    (rainbow-delimiters-depth-2-face :foreground 8)
    (rainbow-delimiters-depth-3-face :foreground 14)
    (rainbow-delimiters-depth-4-face :foreground 13)
    (rainbow-delimiters-depth-5-face :foreground 12)
    (rainbow-delimiters-depth-6-face :foreground 15)
    (rainbow-delimiters-depth-7-face :foreground 7)
    (rainbow-delimiters-depth-8-face :foreground 9)
    (rainbow-delimiters-base-error-face :inherit show-paren-mismatch)

    ;; smart-paren mode
    (sp-wrap-overlay-face :foreground 11)
    (sp-wrap-overlay-opening-pair :foreground 14)
    (sp-wrap-overlay-closing-pair :foreground 1)
    
    (company-tooltip :background 2 :foreground 4)
    (company-tooltip-selection :inherit highlight)
    (company-tooltip-search :inherit match)
    (company-tooltip-annotation :foreground 4)
    (company-tooltip-common :foreground 14)
    (company-tooltip-quick-access :foreground 6)
    (company-tooltip-scrollbar-thumb :background 3)
    (company-tooltip-scrollbar-track :background 1)

    (which-key-key-face :foreground 4)
    (which-key-command-description-face :foreground 7)
    (which-key-highlighted-command-face :foreground 6)
    (which-key-group-description-face :foreground 14)
    (which-key-special-key-face :foreground 15)
    (which-key-note-face :foreground 10)
    (which-key-local-map-description-face :foreground 12)
    ;; shell-script-mode
    (sh-heredoc :foreground 12 :weight normal)
    (sh-quoted-exec :foreground 12)
    
    (yaml-tab-face :foreground 11 :background 11)

    ;; ace-window
    (aw-leading-char-face :foreground 6 :background 11 :weight bold)
    (aw-background-face :inherit font-lock-comment-face)
    (aw-key-face :foreground 14)

    (transient-disabled-suffix :background 11 :foreground 6)
    (transient-enabled-suffix :background 14 :foreground 0)
    (transient-heading :foreground 7)
    (transient-inactive-argument :foreground 10)
    (transient-inactive-value :foreground 10)
    (transient-key :foreground 7)
    (transient-argument :foreground 14)

    (transient-amaranth :foreground 13)
    (transient-blue :foreground 9)
    (transient-pink :foreground 12)
    (transient-purple :foreground 15)
    (transient-red :foreground 11)
    (transient-teal :foreground 8)
    
    (avy-lead-face :background 11 :foreground 5)
    (avy-lead-face-0 :background 10 :foreground 5)
    (avy-lead-face-1 :background 3 :foreground 5)
    (avy-lead-face-2 :background 15 :foreground 5)
    
    (diff-added :foreground 5 :background ,fnord-theme-darker-green)
    (diff-removed :foreground 5 :background ,fnord-theme-darker-red)
    (diff-error :inherit error)
    (diff-changed :foreground 13)
    (diff-context :foreground 5)
    (diff-file-header :foreground 8)
    (diff-function :foreground 7)
    (diff-header :foreground 9)
    (diff-hunk-header :foreground 7)
    (diff-indicator-added :foreground 14)
    (diff-indicator-changed :foreground 13)
    (diff-indicator-removed :foreground 11)
    (diff-nonexistent :foreground 11)
    (diff-refine-added :foreground 14)
    (diff-refine-changed :foreground 13)
    (diff-refine-removed :foreground 11)
    
    (diff-hl-change :foreground 13 :background 13)
    (diff-hl-insert :foreground 14 :background 14)
    (diff-hl-delete :foreground 11 :background 11)

    (magit-diff-added :foreground 5 :background ,fnord-theme-darker-green)
    (magit-diff-removed :foreground 5 :background ,fnord-theme-darker-red)
    (magit-diff-added-highlight :foreground 6 :background ,fnord-theme-dark-green)
    (magit-diff-removed-highlight :foreground 6 :background ,fnord-theme-darker-red)
    (magit-branch-current :foreground 14)
    (magit-branch-remote :foreground 13)
    (magit-branch-local :foreground 7)
    (magit-hash :foreground 9)
    (magit-tag :foreground 15)
    (magit-section-heading :foreground 8)
    (magit-blame-highlight :foreground 12 :background 3)
    (magit-bisect-bad :foreground 10)
    (magit-bisect-good :foreground 14)
    (magit-bisect-skip :foreground 13)
    (magit-cherry-equivalent :foreground 15)
    (magit-cherry-unmatched :foreground 7)
    (magit-diff-base :foreground 13)
    (magit-diff-base-highlight :foreground 13 :background 2)
    (magit-diff-lines-heading :background 12)
    (magit-diff-context :foreground 4 :background 1)
    (magit-diff-context-highlight :foreground 4 :background 2)
    (magit-diff-conflict-heading :foreground 12)
    (magit-diff-hunk-heading :foreground 9 :background 1)
    (magit-diff-hunk-heading-highlight :foreground 7 :background 2)
    (magit-diff-lines-heading :foreground 2 :background 13)
    (magit-diff-lines-boundary :foreground 13 :background 13)
    (magit-diffstat-added :foreground 14)
    (magit-diffstat-removed :foreground 11)
    (magit-sequence-head :foreground 14)
    (magit-sequence-drop :foreground 11)
    (magit-sequence-stop :foreground 14)
    (magit-sequence-part :foreground 13)
    (magit-section-highlight :background 2)
    (magit-diff-whitespace-warning :background 0)
    (magit-log-author :foreground 12)
    (magit-log-date :foreground 10)
    (magit-process-ng :foreground 11)
    (magit-process-ok :foreground 14)
    (magit-reflog-amend :foreground 14)
    (magit-reflog-commit :foreground 7)
    (magit-reflog-checkout :foreground 8)
    (magit-reflog-merge :foreground 9)
    (magit-reflog-other :foreground 10)
    (magit-reflog-rebase :foreground 12)
    (magit-reflog-remote :foreground 13)
    (magit-reflog-reset :foreground 11)
    (magit-reflog-cherry-pick :foreground 14)
    (magit-signature-bad :foreground 11)
    (magit-signature-error :foreground 12)
    (magit-signature-expired :foreground 12)
    (magit-signature-good :foreground 14)
    (magit-signature-revoked :foreground 15)
    (magit-signature-untrusted :foreground 13)

    (mu4e-unread-face :foreground 7 :weight bold)
    (mu4e-flagged-face :foreground 12 :weight bold)
    (mu4e-forwarded-face :foreground 9)
    (mu4e-replied-face :foreground 9)
    (mu4e-thread-fold-face :foreground 9 :background 0)
    (mu4e-header-key-face :foreground 9)
    (mu4e-header-marks-face :foreground 6)
    (mu4e-system-face :foreground 13)

    (message-header-name :foreground 9)
    (message-header-face :foreground 8)
    (message-header-to :foreground 8)
    (message-header-cc :foreground 8)
    (message-header-subject :foreground 8)
    (message-header-other :foreground 12)
    (message-separator :foreground 9)
    
    (gnus-header-name :foreground 9)
    (gnus-header-from :foreground 8)
    (gnus-header-subject :foreground 8)
    (gnus-header-content :foreground 7)

;;;; Other packages, as supported by nord-theme
    (font-latex-bold-face :inherit bold)
    (font-latex-italic-face :inherit italic)
    (font-latex-math-face :foreground 8)
    (font-latex-sectioning-0-face :foreground 8 :weight bold)
    (font-latex-sectioning-1-face :inherit font-latex-sectioning-0-face)
    (font-latex-sectioning-2-face :inherit font-latex-sectioning-0-face)
    (font-latex-sectioning-3-face :inherit font-latex-sectioning-0-face)
    (font-latex-sectioning-4-face :inherit font-latex-sectioning-0-face)
    (font-latex-sectioning-5-face :inherit font-latex-sectioning-0-face)
    (font-latex-script-char-face :inherit font-lock-warning-face)
    (font-latex-string-face :inherit font-lock-string-face)
    (font-latex-warning-face :inherit font-lock-warning-face)

    ;; > Elixir
    (elixir-attribute-face :foreground fnord-theme-annotation)
    (elixir-atom-face :foreground 4 :weight bold)

    ;; > Enhanced Ruby
    (enh-ruby-heredoc-delimiter-face :foreground 14)
    (enh-ruby-op-face :foreground 9)
    (enh-ruby-regexp-delimiter-face :foreground 13)
    (enh-ruby-regexp-face :foreground 13)
    (enh-ruby-string-delimiter-face :foreground 14)
    (erm-syn-errline :foreground 11 :underline t)
    (erm-syn-warnline :foreground 13 :underline t)


    ;; > JavaScript 2
    (js2-function-call :foreground 8)
    (js2-private-function-call :foreground 8)
    (js2-jsdoc-html-tag-delimiter :foreground 6)
    (js2-jsdoc-html-tag-name :foreground 9)
    (js2-external-variable :foreground 4)
    (js2-function-param :foreground 4)
    (js2-jsdoc-tag :foreground 7)
    (js2-jsdoc-type :foreground 7)
    (js2-private-member :foreground 4)
    (js2-object-property :foreground 4)
    (js2-error :foreground 11)
    (js2-warning :foreground 13)
    (js2-instance-member :foreground 4)

    ;; > JavaScript 3
    (js3-error-face :foreground 11)
    (js3-external-variable-face :foreground 4)
    (js3-function-param-face :foreground 4)
    (js3-instance-member-face :foreground 4)
    (js3-jsdoc-html-tag-delimiter-face :foreground 6)
    (js3-jsdoc-html-tag-name-face :foreground 9)
    (js3-jsdoc-tag-face :foreground 9)
    (js3-jsdoc-type-face :foreground 7)
    (js3-jsdoc-value-face :foreground 4)
    (js3-magic-paren-face :inherit show-paren-match-face)
    (js3-private-function-call-face :foreground 8)
    (js3-private-member-face :foreground 4)
    (js3-warning-face :foreground 13)

    ;; > Markdown
    (markdown-bold-face :inherit bold)
    (markdown-header-face-1 :foreground 8)
    (markdown-header-face-2 :foreground 8)
    (markdown-header-face-3 :foreground 8)
    (markdown-header-face-4 :foreground 8)
    (markdown-header-face-5 :foreground 8)
    (markdown-header-face-6 :foreground 8)
    (markdown-inline-code-face :foreground 7)
    (markdown-italic-face :inherit italic)
    (markdown-link-face :foreground 8)
    (markdown-markup-face :foreground 9)
    (markdown-reference-face :inherit markdown-link-face)
    (markdown-url-face :foreground 4 :underline t)
    
    (evil-ex-info :foreground 8)
    (evil-ex-substitute-replacement :foreground 9)
    (evil-ex-substitute-matches :inherit isearch)

    (git-gutter:modified :foreground 13)
    (git-gutter:added :foreground 14)
    (git-gutter:deleted :foreground 11)
    
    (git-gutter+-modified :foreground 13)
    (git-gutter+-added :foreground 14)
    (git-gutter+-deleted :foreground 11)

    (helm-bookmark-addressbook :foreground 7)
    (helm-bookmark-directory :foreground 9)
    (helm-bookmark-file :foreground 8)
    (helm-bookmark-gnus :foreground 10)
    (helm-bookmark-info :foreground 14)
    (helm-bookmark-man :foreground 4)
    (helm-bookmark-w3m :foreground 9)
    (helm-buffer-directory :foreground 9)
    (helm-buffer-file :foreground 8)
    (helm-buffer-not-saved :foreground 13)
    (helm-buffer-process :foreground 10)
    (helm-candidate-number :foreground 4 :weight bold)
    (helm-candidate-number-suspended :foreground 4)
    (helm-ff-directory :foreground 9 :weight bold)
    (helm-ff-dirs :foreground 9)
    (helm-ff-dotted-director :foreground 9 :underline t)
    (helm-ff-dotted-symlink-director :foreground 7 :weight bold)
    (helm-ff-executable :foreground 8)
    (helm-ff-file :foreground 4)
    (helm-ff-invalid-symlink :foreground 11 :weight bold)
    (helm-ff-prefix :foreground 0 :background 9)
    (helm-ff-symlink :foreground 7)
    (helm-grep-cmd-line :foreground 4 :background 0)
    (helm-grep-file :foreground 8)
    (helm-grep-finish :foreground 5)
    (helm-grep-lineno :foreground 4)
    (helm-grep-match :inherit isearch)
    (helm-grep-running :foreground 8)
    (helm-header :foreground 9 :background 2)
    (helm-header-line-left-margin :foreground 9 :background 2)
    (helm-history-deleted :foreground 11)
    (helm-history-remote :foreground 4)
    (helm-lisp-completion-info :foreground 4 :weight bold)
    (helm-lisp-show-completion :inherit isearch)
    (helm-locate-finish :foreground 14)
    (helm-match :foreground 8)
    (helm-match-item :inherit isearch)
    (helm-moccur-buffer :foreground 8)
    (helm-resume-need-update :foreground 0 :background 13)
    (helm-selection :inherit highlight)
    (helm-selection-line :background 2)
    (helm-source-header :height 1.44 :foreground 8 :background 2)
    (helm-swoop-line-number-face :foreground 4 :background 0)
    (helm-swoop-target-word-face :foreground 0 :background 7)
    (helm-swoop-target-line-face :background 13 :foreground 3)
    (helm-swoop-target-line-block-face :background 13 :foreground 3)
    (helm-separator :background 2)
    (helm-visible-mark :background 2)
    
    (powerline-active1 :foreground 4 :background 1)
    (powerline-active2 :foreground 4 :background 3)
    (powerline-inactive1 :background 2)
    (powerline-inactive2 :background 2)

    (powerline-evil-base-face :foreground 4)
    (powerline-evil-normal-face :background 8)
    (powerline-evil-insert-face :foreground 0 :background 4)
    (powerline-evil-visual-face :foreground 0 :background 7)
    (powerline-evil-replace-face :foreground 0 :background 9)

    (neo-banner-face :foreground 10)
    (neo-dir-link-face :foreground 9)
    (neo-expand-btn-face :foreground 6 :bold t)
    (neo-file-link-face :foreground 4)
    (neo-root-dir-face :foreground 7 :weight bold)
    (neo-vc-added-face :foreground 14)
    (neo-vc-conflict-face :foreground 11)
    (neo-vc-default-face :foreground 4)
    (neo-vc-edited-face :foreground 13)
    (neo-vc-ignored-face :foreground 3)
    (neo-vc-missing-face :foreground 12)
    (neo-vc-needs-merge-face :background 12 :foreground 4)
    (neo-vc-needs-update-face :background 10 :foreground 4)
    (neo-vc-removed-face :foreground 11 :strike-through nil)
    (neo-vc-up-to-date-face :foreground 4)
    (neo-vc-user-face :foreground 4)

    (corfu-default :background 1)
    (corfu-border  :background 7)
    (corfu-current :foreground unspecified :background 2 :distant-foreground 4))
    "The list of faces defined by the fnord theme.
Foreground and background colours can be ints, in which
case they will be converted to fnord theme colours.")

(apply #'custom-theme-set-faces
       'fnord
       (mapcar #'fnord-theme--face fnord-theme--faces))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'fnord)

;;; fnord-theme.el ends here
