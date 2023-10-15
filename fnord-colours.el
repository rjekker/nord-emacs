;;; fnord-colours.el --- Colour definitions for the fnord theme

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

(defgroup fnord-theme nil
  "Fnord theme customizations.
Most settings in this group are applied immediately - no need to reload the theme."
  :group 'faces)

(defgroup fnord-theme-colours nil
  "Colour values for the fnord theme.
These settings are NOT applied immediately - you will need to reload the theme."
  :group 'fnord-theme
  :link '(url-link "https://www.nordtheme.com/docs/colors-and-palettes"))

(defcustom fnord-0 "#2E3440"
  "Fnord theme colour 0 - Polar Night (dark()). For background and area coloring."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-01 "#3B4252"
  "Fnord theme colour 1 - Polar night 1 (dark).
A brighter shade based on `fnord-0'.
For elevated, more prominent or focused UI elements like

- status bars and text editor gutters
- panels, modals and floating popups like notifications or auto completion
- user interaction/form components like buttons, text/select fields or
  checkboxes

It also works fine for more inconspicuous and passive elements like borders
 or as dropshadow between different components."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-02 "#434C5E"
  "Fnord theme colour 2 - Polar night 2 (dark).
Used to colorize the currently active text editor line as well as selection-
and text highlighting color. It can also be used as an brighter variant for
the same target elements like `fnord-01'."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-03 "#4C566A"
  "Fnord theme colour 3 - Polar night 3 (dark).
For UI elements like indent- and wrap guide marker. In the context of code
syntax highlighting it is used for comments and invisible/non-printable
 characters."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-04 "#D8DEE9"
  "Fnord theme colour 4 - Snow Storm 1 (light).
For UI elements like the text editor caret.
In the context of syntax highlighting it is used as text color for variables,
constants, attributes and fields."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-05 "#E5E9F0"
  "Fnord theme colour 5 - Snow Storm 2 (light).
Used for more subtle/inconspicuous UI text elements that do not need so much
visual attention. Other use cases are also state animations like a more brighter
text color when a button is hovered, active or focused."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-06 "#ECEFF4"
  "Fnord theme colour 6 - Snow Storm 3 (light).
For elevated UI text elements that require more visual attention.
In the context of syntax highlighting it is used as text color for plain text as
well as reserved and structuring syntax characters like curly- and square
brackets."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-07 "#8FBCBB"
  "Fnord theme colour 7 - Frost 1 (green).
A calm and highly contrasted color.
Used for UI elements that should, next to the primary accent color nord8, stand
out and get more visual attention.
In the context of syntax highlighting it is used for classes, types and
primitives."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-08 "#88C0D0"
  "Fnord theme colour 8 - Frost 2 (light blue)
Bright and shiny primary accent color.
Used for primary UI elements with main usage purposes that require the most
visual attention. In the context of syntax highlighting it is used for
declarations, calls and execution statements of functions, methods and routines."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-09 "#81A1C1"
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

(defcustom fnord-10 "#5E81AC"
  "Fnord theme colour 10 - Frost 4 (blue).
A dark and intensive color.
Used for tertiary UI elements that require more visual attention than default
elements. In the context of syntax highlighting it is used for pragmas, comment
keywords and pre-processor statements."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-11 "#BF616A"
  "Fnord theme colour 11 - Aurora 1 (red).
Used for UI elements that are rendering error states like linter markers and the
highlighting of Git diff deletions. In the context of syntax highlighting it is
used to override the highlighting of syntax elements that are detected as
errors."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-12 "#D08770"
  "Fnord theme colour 12 - Aurora 2 (orange).
Rarely used for UI elements, but it may indicate a more advanced or dangerous
functionality. In the context of syntax highlighting it is used for special
syntax elements like annotations and decorators."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-13 "#EBCB8B"
  "Fnord theme colour 13 - Aurora 3 (yellow).
Used for UI elements that are rendering warning states like linter markers and
the highlighting of Git diff modifications. In the context of syntax
highlighting it is used to override the highlighting of syntax elements that are
detected as warnings as well as escape characters and within regular
expressions."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-14 "#A3BE8C"
  "Fnord theme colour 14 - Aurora 4 (light green).
Used for UI elements that are rendering success states and visualizations and
the highlighting of Git diff additions. In the context of syntax highlighting
it is used as main color for strings of any type like double/single quoted or
interpolated."
  :type 'string
  :group 'fnord-theme-colours)

(defcustom fnord-15 "#B48EAD"
  "Fnord theme colour 15 - Aurora 5 (purple).
Rarely used for UI elements, but it may indicate a more uncommon functionality.
In the context of syntax highlighting it is used as main color for numbers of
any type like integers and floating point numbers."
  :type 'string
  :group 'fnord-theme-colours)

(require 'cl-lib)

(provide 'fnord-colours)

;;; fnord-colours.el ends here
