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
  "Nord theme customizations.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom fnord-region-highlight nil
  "Allows to set a region highlight style based on the Nord components.
Valid styles are
    - 'snowstorm' - Uses 'nord0' as foreground- and 'nord4' as background color
    - 'frost' - Uses 'nord0' as foreground- and 'nord8' as background color"
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-uniform-mode-lines nil
  "Enables uniform activate- and inactive mode lines using 'nord3' as background."
  :type 'boolean
  :group 'fnord-theme)

(defcustom fnord-comment-colour "#616e88"
  "A special colour for comments that is not in the original Nord palette."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-comment-colour-256 "darkgrey"
  "A special colour for comments that is not in the original Nord palette."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-0 "#2E3440"
  "Nord theme colour 0 - Polar Night. For background and area coloring."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-1 "#3B4252"
  "Nord theme colour 1 - a brighter shade based on `fnord-0'.
For elevated, more prominent or focused UI elements like

- status bars and text editor gutters
- panels, modals and floating popups like notifications or auto completion
- user interaction/form components like buttons, text/select fields or
  checkboxes

It also works fine for more inconspicuous and passive elements like borders
 or as dropshadow between different components."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-2 "#434C5E"
  "Nord theme colour 2 - an even brighter shade of `fnord-0'.
Used to colorize the currently active text editor line as well as selection-
and text highlighting color. It can also be used as an brighter variant for
the same target elements like `fnord-1'."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-3 "#4C566A"
  "Nord theme colour 3 - the brightest shade of `fnord-0'.
For UI elements like indent- and wrap guide marker. In the context of code
syntax highlighting it is used for comments and invisible/non-printable characters."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-4 "#D8DEE9"
  "Nord theme colour 4 - Snow Storm.
For UI elements like the text editor caret.
In the context of syntax highlighting it is used as text color for variables,
constants, attributes and fields."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-5 "#E5E9F0"
  "Nord theme colour 5 - a brighter shade of `fnord-4'.
Used for more subtle/inconspicuous UI text elements that do not need so much
visual attention. Other use cases are also state animations like a more brighter
text color when a button is hovered, active or focused."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-6 "#ECEFF4"
  "Nord theme colour 6 - the brightest shade of `fnord-4'.
For elevated UI text elements that require more visual attention.
In the context of syntax highlighting it is used as text color for plain text as
well as reserved and structuring syntax characters like curly- and square brackets."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-7 "#8FBCBB"
  "Nord theme colour 7 - Frost: A calm and highly contrasted color.
Used for UI elements that should, next to the primary accent color nord8, stand
out and get more visual attention.
In the context of syntax highlighting it is used for classes, types and primitives."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-8 "#88C0D0"
  "Nord theme colour 8 - bright and shiny primary accent color.
Used for primary UI elements with main usage purposes that require the most
visual attention. In the context of syntax highlighting it is used for
declarations, calls and execution statements of functions, methods and routines."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-9 "#81A1C1"
  "Nord theme colour 9 - A more darkened and less saturated color.
Used for secondary UI elements that also require more visual attention than
other elements.In the context of syntax highlighting it is used for language
specific, syntactic and reserved keywords as well as

- support characters
- operators
- tags
- units
- punctuations like (semi)colons, points and commas."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-10 "#5E81AC"
  "Nord theme colour 10 - A dark and intensive color.
Used for tertiary UI elements that require more visual attention than default
elements. In the context of syntax highlighting it is used for pragmas, comment
keywords and pre-processor statements."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-11 "#BF616A"
  "Nord theme colour 11 - Aurora.
Used for UI elements that are rendering error states like linter markers and the
highlighting of Git diff deletions. In the context of syntax highlighting it is
used to override the highlighting of syntax elements that are detected as errors."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-12 "#D08770"
  "Nord theme colour 12.
Rarely used for UI elements, but it may indicate a more advanced or dangerous
functionality. In the context of syntax highlighting it is used for special
syntax elements like annotations and decorators."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-13 "#EBCB8B"
  "Nord theme colour 13.
Used for UI elements that are rendering warning states like linter markers and
the highlighting of Git diff modifications. In the context of syntax
highlighting it is used to override the highlighting of syntax elements that are
detected as warnings as well as escape characters and within regular expressions."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-14 "#A3BE8C"
  "Nord theme colour 14.
Used for UI elements that are rendering success states and visualizations and
the highlighting of Git diff additions. In the context of syntax highlighting
it is used as main color for strings of any type like double/single quoted or
interpolated."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-15 "#B48EAD"
  "Nord theme colour 15.
Rarely used for UI elements, but it may indicate a more uncommon functionality.
In the context of syntax highlighting it is used as main color for numbers of
any type like integers and floating point numbers."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-1-256 "black"
  "Nord theme colour 1 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-2-256 "#434C5E"
  "Nord theme colour 2 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-3-256 "brightblack"
  "Nord theme colour 3 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-4-256 "#D8DEE9"
  "Nord theme colour 4 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-5-256 "white"
  "Nord theme colour 5 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-6-256 "brightwhite"
  "Nord theme colour 6 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-7-256 "cyan"
  "Nord theme colour 7 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-8-256 "brightcyan"
  "Nord theme colour 8 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-9-256 "blue"
  "Nord theme colour 9 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-10-256 "brightblue"
  "Nord theme colour 10 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-11-256 "red"
  "Nord theme colour 11 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-12-256 "brightyellow"
  "Nord theme colour 12 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-13-256 "yellow"
  "Nord theme colour 13 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-14-256 "green"
  "Nord theme colour 14 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(defcustom fnord-15-256 "magenta"
  "Nord theme colour 15 for terminals that do not support 256 colours."
  :type 'string
  :group 'fnord-theme)

(provide 'fnord-colours)

;;; fnord-colours.el ends here
