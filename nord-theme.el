;;; nord-theme.el --- An arctic, north-bluish clean and elegant theme

;; Copyright (C) 2016-present Arctic Ice Studio <development@arcticicestudio.com> (https://www.arcticicestudio.com)
;; Copyright (C) 2016-present Sven Greb <development@svengreb.de> (https://www.svengreb.de)

;; Title: Nord Theme
;; Project: nord-emacs
;; Version: 0.5.0
;; URL: https://github.com/arcticicestudio/nord-emacs
;; Author: Arctic Ice Studio <development@arcticicestudio.com>
;; Package-Requires: ((emacs "24"))
;; License: MIT

;;; Commentary:

;; Nord is a 16 colorspace theme build to run in GUI- and terminal
;; mode with support for many third-party syntax- and UI packages.

;;; References:
;; Awesome Emacs
;;   https://github.com/emacs-tw/awesome-emacs
;; GNU ELPA
;;   https://elpa.gnu.org
;; GNU Emacs
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Face-Customization.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html
;; marmalade repo
;;   https://marmalade-repo.org
;; MELPA
;;   https://melpa.org
;;   https://stable.melpa.org

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Nord theme requires Emacs 24 or later!"))

(deftheme nord "An arctic, north-bluish clean and elegant theme")

(defgroup nord nil
  "Nord theme customizations.
  The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom nord-region-highlight nil
  "Allows to set a region highlight style based on the Nord components.
  Valid styles are
    - 'snowstorm' - Uses 'nord0' as foreground- and 'nord4' as background color
    - 'frost' - Uses 'nord0' as foreground- and 'nord8' as background color"
  :type 'string
  :group 'nord)

(defcustom nord-uniform-mode-lines nil
  "Enables uniform activate- and inactive mode lines using 'nord3' as background."
  :type 'boolean
  :group 'nord)

(defun nord-display-truecolor-or-graphic-p ()
  "Returns whether the display can display nord colors"
  (or (= (display-color-cells) 16777216) (display-graphic-p)))

(defcustom nord-0 "#2E3440"
  "Nord theme colour 0 - Polar Night. For background and area coloring."
  :type 'string
  :group 'nord-theme)

(defcustom nord-1 "#3B4252"
  "Nord theme colour 1 - a brighter shade based on `nord-0'.
For elevated, more prominent or focused UI elements like

- status bars and text editor gutters
- panels, modals and floating popups like notifications or auto completion
- user interaction/form components like buttons, text/select fields or
  checkboxes

It also works fine for more inconspicuous and passive elements like borders
 or as dropshadow between different components."
  :type 'string
  :group 'nord-theme)

(defcustom nord-2 "#434C5E"
  "Nord theme colour 2 - an even brighter shade of `nord-0'.
Used to colorize the currently active text editor line as well as selection-
and text highlighting color. It can also be used as an brighter variant for
the same target elements like `nord-1'."
  :type 'string
  :group 'nord-theme)

(defcustom nord-3 "#4C566A"
  "Nord theme colour 3 - the brightest shade of `nord-0'.
For UI elements like indent- and wrap guide marker. In the context of code
syntax highlighting it is used for comments and invisible/non-printable characters."
  :type 'string
  :group 'nord-theme)

(defcustom nord-4 "#D8DEE9"
  "Nord theme colour 4 - Snow Storm.
For UI elements like the text editor caret.
In the context of syntax highlighting it is used as text color for variables,
constants, attributes and fields."
  :type 'string
  :group 'nord-theme)

(defcustom nord-5 "#E5E9F0"
  "Nord theme colour 5 - a brighter shade of `nord-4'.
Used for more subtle/inconspicuous UI text elements that do not need so much
visual attention. Other use cases are also state animations like a more brighter
text color when a button is hovered, active or focused."
  :type 'string
  :group 'nord-theme)

(defcustom nord-6 "#ECEFF4"
  "Nord theme colour 6 - the brightest shade of `nord-4'.
For elevated UI text elements that require more visual attention.
In the context of syntax highlighting it is used as text color for plain text as
well as reserved and structuring syntax characters like curly- and square brackets."
  :type 'string
  :group 'nord-theme)

(defcustom nord-7 "#8FBCBB"
  "Nord theme colour 7 - Frost: A calm and highly contrasted color.
Used for UI elements that should, next to the primary accent color nord8, stand
out and get more visual attention.
In the context of syntax highlighting it is used for classes, types and primitives."
  :type 'string
  :group 'nord-theme)

(defcustom nord-8 "#88C0D0"
  "Nord theme colour 8 - bright and shiny primary accent color.
Used for primary UI elements with main usage purposes that require the most
visual attention. In the context of syntax highlighting it is used for
declarations, calls and execution statements of functions, methods and routines."
  :type 'string
  :group 'nord-theme)

(defcustom nord-9 "#81A1C1"
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
  :group 'nord-theme)

(defcustom nord-10 "#5E81AC"
  "Nord theme colour 10 - A dark and intensive color.
Used for tertiary UI elements that require more visual attention than default
elements. In the context of syntax highlighting it is used for pragmas, comment
keywords and pre-processor statements."
  :type 'string
  :group 'nord-theme)

(defcustom nord-11 "#BF616A"
  "Nord theme colour 11 - Aurora.
Used for UI elements that are rendering error states like linter markers and the
highlighting of Git diff deletions. In the context of syntax highlighting it is
used to override the highlighting of syntax elements that are detected as errors."
  :type 'string
  :group 'nord-theme)

(defcustom nord-12 "#D08770"
  "Nord theme colour 12.
Rarely used for UI elements, but it may indicate a more advanced or dangerous
functionality. In the context of syntax highlighting it is used for special
syntax elements like annotations and decorators."
  :type 'string
  :group 'nord-theme)

(defcustom nord-13 "#EBCB8B"
  "Nord theme colour 13.
Used for UI elements that are rendering warning states like linter markers and
the highlighting of Git diff modifications. In the context of syntax
highlighting it is used to override the highlighting of syntax elements that are
detected as warnings as well as escape characters and within regular expressions."
  :type 'string
  :group 'nord-theme)

(defcustom nord-14 "#A3BE8C"
  "Nord theme colour 14.
Used for UI elements that are rendering success states and visualizations and
the highlighting of Git diff additions. In the context of syntax highlighting
it is used as main color for strings of any type like double/single quoted or
interpolated."
  :type 'string
  :group 'nord-theme)

(defcustom nord-15 "#B48EAD"
  "Nord theme colour 15.
Rarely used for UI elements, but it may indicate a more uncommon functionality.
In the context of syntax highlighting it is used as main color for numbers of
any type like integers and floating point numbers."
  :type 'string
  :group 'nord-theme)

(defcustom nord-1-256 "black"
  "Nord theme colour 1 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-2-256 "#434C5E"
  "Nord theme colour 2 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-3-256 "brightblack"
  "Nord theme colour 3 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-4-256 "#D8DEE9"
  "Nord theme colour 4 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-5-256 "white"
  "Nord theme colour 5 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-6-256 "brightwhite"
  "Nord theme colour 6 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-7-256 "cyan"
  "Nord theme colour 7 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-8-256 "brightcyan"
  "Nord theme colour 8 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-9-256 "blue"
  "Nord theme colour 9 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-10-256 "brightblue"
  "Nord theme colour 10 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-11-256 "red"
  "Nord theme colour 11 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-12-256 "brightyellow"
  "Nord theme colour 12 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-13-256 "yellow"
  "Nord theme colour 13 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-14-256 "green"
  "Nord theme colour 14 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)

(defcustom nord-15-256 "magenta"
  "Nord theme colour 15 for terminals that do not support 256 colours."
  :type 'string
  :group 'nord-theme)


(defun nord--get-colour (n col-256)
  "Retrieve nord colour N. If COL-256 is t, return the colour for terminals."
  (cl-assert (and (>= n 0) (< n 16)))                  
  (when n
    (nth n (if col-256
                     (list nord-0 nord-1-256 nord-2-256 nord-3-256 nord-4-256 nord-5-256 nord-6-256 nord-7-256 nord-8-256 nord-9-256 nord-10-256 nord-11-256 nord-12-256 nord-13-256 nord-14-256 nord-15-256)
                   (list nord-0 nord-1 nord-2 nord-3 nord-4 nord-5 nord-6 nord-7 nord-8 nord-9 nord-10 nord-11 nord-12 nord-13 nord-14 nord-15)))))


(defconst nord--class '((class color) (min-colors 257)))
(defconst nord--class-256 '((class color) (min-colors 89)))

(defun nord--subst-colours (face-spec col-256)
  (dolist (prop '(:foreground :background))
    (when-let ((val (plist-get face-spec prop)))
      (when (numberp val)
        (plist-put face-spec prop (nord--get-colour val col-256)))))
  face-spec)

(defun nord--face (&rest face-spec)
  "Create FACE-SPEC where foreground and background can be numbers. Those will
be substituted using `nord--get-colour'."
  `((,nord--class . ,(nord--subst-colours face-spec nil))
    (,nord--class-256 . ,(nord--subst-colours face-spec t))))


;;;; Color Constants
(let* ((nord-annotation 12)
       (nord-attribute 7)
       (nord-class 7)
       (nord-comment (if (nord-display-truecolor-or-graphic-p)  "#616e88" "brightblack"))
       (nord-escape 12)
       (nord-method 8)
       (nord-keyword 9)
       (nord-numeric 15)
       (nord-operator 9)
       (nord-preprocessor (if (nord-display-truecolor-or-graphic-p) "#5E81AC" "brightblue"))
       (nord-punctuation (if (nord-display-truecolor-or-graphic-p) "#D8DEE9" "#D8DEE9"))
       (nord-regexp (if (nord-display-truecolor-or-graphic-p) "#EBCB8B" "yellow"))
       (nord-string (if (nord-display-truecolor-or-graphic-p) "#A3BE8C" "green"))
       (nord-tag 9)
       (nord-variable (if (nord-display-truecolor-or-graphic-p) "#D8DEE9" "#D8DEE9"))
       (nord-region-highlight-foreground (if (or
                                              (string= nord-region-highlight "frost")
                                              (string= nord-region-highlight "snowstorm")) "#2E3440" nil))
       (nord-region-highlight-background (if
                                             (string= nord-region-highlight "frost") "#88C0D0"
                                           (if (string= nord-region-highlight "snowstorm") "#D8DEE9" "#434C5E")))
       (nord-uniform-mode-lines-background (if nord-uniform-mode-lines "#4C566A" "#3B4252")))
  
;;;; +------------+
;;;; + Core Faces +
;;;; +------------+
  (custom-theme-set-faces
   'nord
   ;; +--- Base ---+
   `(bold ,(nord--face :weight 'bold))
   `(bold-italic ,(nord--face :weight 'bold :slant 'italic))
   `(default ,(nord--face :foreground 4 :background 0))
   `(error ,(nord--face :foreground 11 :weight 'bold))
   `(escape-glyph ,(nord--face :foreground 12))
   `(font-lock-builtin-face ,(nord--face :foreground 9))
   `(font-lock-comment-face ,(nord--face :foreground nord-comment))
   `(font-lock-comment-delimiter-face ,(nord--face :foreground nord-comment))
   `(font-lock-constant-face ,(nord--face :foreground 9))
   `(font-lock-doc-face ,(nord--face :foreground nord-comment))
   `(font-lock-function-name-face ,(nord--face :foreground 8))
   `(font-lock-keyword-face ,(nord--face :foreground 9))
   `(font-lock-negation-char-face ,(nord--face :foreground 9))
   `(font-lock-preprocessor-face ,(nord--face :foreground 10 :weight 'bold))
   `(font-lock-reference-face ,(nord--face :foreground 9))
   `(font-lock-regexp-grouping-backslash ,(nord--face :foreground 13))
   `(font-lock-regexp-grouping-construct ,(nord--face :foreground 13))
   `(font-lock-string-face ,(nord--face :foreground 14))
   `(font-lock-type-face ,(nord--face :foreground 7))
   `(font-lock-variable-name-face ,(nord--face :foreground 4))
   `(font-lock-warning-face ,(nord--face :foreground 13))
   `(italic ,(nord--face :slant 'italic))
   `(shadow ,(nord--face :foreground 3))
   `(underline ,(nord--face :underline t))
   `(warning ,(nord--face :foreground 13 :weight 'bold))

   ;; +--- Syntax ---+
   ;; > C
   `(c-annotation-face ,(nord--face :foreground nord-annotation))

   ;; > diff
   `(diff-added ,(nord--face :foreground 14))
   `(diff-changed ,(nord--face :foreground 13))
   `(diff-context ,(nord--face :inherit 'default))
   `(diff-file-header ,(nord--face :foreground 8))
   `(diff-function ,(nord--face :foreground 7))
   `(diff-header ,(nord--face :foreground 9 :weight 'bold))
   `(diff-hunk-header ,(nord--face :foreground 9 :background 0))
   `(diff-indicator-added ,(nord--face :foreground 14))
   `(diff-indicator-changed ,(nord--face :foreground 13))
   `(diff-indicator-removed ,(nord--face :foreground 11))
   `(diff-nonexistent ,(nord--face :foreground 11))
   `(diff-refine-added ,(nord--face :foreground 14))
   `(diff-refine-changed ,(nord--face :foreground 13))
   `(diff-refine-removed ,(nord--face :foreground 11))
   `(diff-removed ,(nord--face :foreground 11))

   ;; +--- UI ---+
   `(border ,(nord--face :foreground 4))
   `(buffer-menu-buffer ,(nord--face :foreground 4 :weight 'bold))
   `(button ,(nord--face :background 0 :foreground 8 :box '(:line-width 2 :color 4 :style sunken-button)))
   `(completions-annotations ,(nord--face :foreground 9))
   `(completions-common-part ,(nord--face :foreground 8 :weight 'bold))
   `(completions-first-difference ,(nord--face :foreground 11))
   `(custom-button ,(nord--face :background 0 :foreground 8 :box '(:line-width 2 :color 4 :style sunken-button)))
   `(custom-button-mouse ,(nord--face :background 4 :foreground 0 :box '(:line-width 2 :color 4 :style sunken-button)))
   `(custom-button-pressed ,(nord--face :background 6 :foreground 0 :box '(:line-width 2 :color 4 :style sunken-button)))
   `(custom-button-pressed-unraised ,(nord--face :background 4 :foreground 0 :box '(:line-width 2 :color 4 :style sunken-button)))
   `(custom-button-unraised ,(nord--face :background 0 :foreground 8 :box '(:line-width 2 :color 4 :style sunken-button)))
   `(custom-changed ,(nord--face :foreground 13))
   `(custom-comment ,(nord--face :foreground nord-comment))
   `(custom-comment-tag ,(nord--face :foreground 7))
   `(custom-documentation ,(nord--face :foreground 4))
   `(custom-group-tag ,(nord--face :foreground 8 :weight 'bold))
   `(custom-group-tag-1 ,(nord--face :foreground 8 :weight 'bold))
   `(custom-invalid ,(nord--face :foreground 11))
   `(custom-modified ,(nord--face :foreground 13))
   `(custom-rogue ,(nord--face :foreground 12 :background 2))
   `(custom-saved ,(nord--face :foreground 14))
   `(custom-set ,(nord--face :foreground 8))
   `(custom-state ,(nord--face :foreground 14))
   `(custom-themed ,(nord--face :foreground 8 :background 2))
   `(cursor ,(nord--face :background 4))
   `(fringe ,(nord--face :foreground 4 :background 0))
   `(file-name-shadow ,(nord--face :inherit 'shadow))
   `(header-line ,(nord--face :foreground 4 :background 2))
   `(help-argument-name ,(nord--face :foreground 8))
   `(highlight ,(nord--face :foreground 8 :background 2))
   `(hl-line ,(nord--face :background 1))
   `(info-menu-star ,(nord--face :foreground 9))
   `(isearch ,(nord--face :foreground 0 :background 8))
   `(isearch-fail ,(nord--face :foreground 11))
   `(link ,(nord--face :underline t))
   `(link-visited ,(nord--face :underline t))
   `(linum ,(nord--face :foreground 3 :background 0))
   `(linum-relative-current-face ,(nord--face :foreground 3 :background 0))
   `(match ,(nord--face :inherit 'isearch))
   `(message-cited-text ,(nord--face :foreground 4))
   `(message-header-cc ,(nord--face :foreground 9))
   `(message-header-name ,(nord--face :foreground 7))
   `(message-header-newsgroup ,(nord--face :foreground 14))
   `(message-header-other ,(nord--face :foreground 4))
   `(message-header-subject ,(nord--face :foreground 8))
   `(message-header-to ,(nord--face :foreground 9))
   `(message-header-xheader ,(nord--face :foreground 13))
   `(message-mml ,(nord--face :foreground 10))
   `(message-separator ,(nord--face :inherit 'shadow))
   `(minibuffer-prompt ,(nord--face :foreground 8 :weight 'bold))
   `(mm-command-output ,(nord--face :foreground 8))
   `(mode-line ,(nord--face :foreground 8 :background 3))
   `(mode-line-buffer-id ,(nord--face :weight 'bold))
   `(mode-line-highlight ,(nord--face :inherit 'highlight))
   `(mode-line-inactive ,(nord--face :foreground 4 :background nord-uniform-mode-lines-background))
   `(next-error ,(nord--face :inherit 'error))
   `(nobreak-space ,(nord--face :foreground 3))
   `(outline-1 ,(nord--face :foreground 8 :weight 'bold))
   `(outline-2 ,(nord--face :inherit 'outline-1))
   `(outline-3 ,(nord--face :inherit 'outline-1))
   `(outline-4 ,(nord--face :inherit 'outline-1))
   `(outline-5 ,(nord--face :inherit 'outline-1))
   `(outline-6 ,(nord--face :inherit 'outline-1))
   `(outline-7 ,(nord--face :inherit 'outline-1))
   `(outline-8 ,(nord--face :inherit 'outline-1))
   `(package-description ,(nord--face :foreground 4))
   `(package-help-section-name ,(nord--face :foreground 8 :weight 'bold))
   `(package-name ,(nord--face :foreground 8))
   `(package-status-available ,(nord--face :foreground 7))
   `(package-status-avail-obso ,(nord--face :foreground 7 :slant 'italic))
   `(package-status-built-in ,(nord--face :foreground 9))
   `(package-status-dependency ,(nord--face :foreground 8 :slant 'italic))
   `(package-status-disabled ,(nord--face :foreground 3))
   `(package-status-external ,(nord--face :foreground 12 :slant 'italic))
   `(package-status-held ,(nord--face :foreground 4 :weight 'bold))
   `(package-status-new ,(nord--face :foreground 14))
   `(package-status-incompat ,(nord--face :foreground 11))
   `(package-status-installed ,(nord--face :foreground 7 :weight 'bold))
   `(package-status-unsigned ,(nord--face :underline 13))
   `(query-replace ,(nord--face :foreground 8 :background 2))
   `(region ,(nord--face :foreground nord-region-highlight-foreground :background nord-region-highlight-background))
   `(scroll-bar ,(nord--face :background 3))
   `(secondary-selection ,(nord--face :background 2))

   ;; `show-paren-match-face` and `show-paren-mismatch-face` are deprecated since Emacs version 22.1 and were
   ;; removed in Emacs 25.
   ;; https://github.com/arcticicestudio/nord-emacs/issues/75
   ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c430f7e23fc2c22f251ace4254e37dea1452dfc3
   ;; https://github.com/emacs-mirror/emacs/commit/c430f7e23fc2c22f251ace4254e37dea1452dfc3
   `(show-paren-match-face ,(nord--face :foreground 0 :background 8))
   `(show-paren-mismatch-face ,(nord--face :background 11))

   `(show-paren-match ,(nord--face :foreground 0 :background 8))
   `(show-paren-mismatch ,(nord--face :background 11))
   `(success ,(nord--face :foreground 14))
   `(term ,(nord--face :foreground 4 :background 0))
   `(term-color-black ,(nord--face :foreground 1 :background 1))
   `(term-color-white ,(nord--face :foreground 5 :background 5))
   `(term-color-cyan ,(nord--face :foreground 7 :background 7))
   `(term-color-blue ,(nord--face :foreground 8 :background 8))
   `(term-color-red ,(nord--face :foreground 11 :background 11))
   `(term-color-yellow ,(nord--face :foreground 13 :background 13))
   `(term-color-green ,(nord--face :foreground 14 :background 14))
   `(term-color-magenta ,(nord--face :foreground 15 :background 15))
   `(tool-bar ,(nord--face :foreground 4 :background 3))
   `(tooltip ,(nord--face :foreground 0 :background 4))
   `(trailing-whitespace ,(nord--face :foreground 3))
   `(tty-menu-disabled-face ,(nord--face :foreground 1))
   `(tty-menu-enabled-face ,(nord--face :background 2 :foreground 4))
   `(tty-menu-selected-face ,(nord--face :foreground 8 :underline t))
   `(undo-tree-visualizer-current-face ,(nord--face :foreground 8))
   `(undo-tree-visualizer-default-face ,(nord--face :foreground 4))
   `(undo-tree-visualizer-unmodified-face ,(nord--face :foreground 4))
   `(undo-tree-visualizer-register-face ,(nord--face :foreground 9))
   `(vc-conflict-state ,(nord--face :foreground 12))
   `(vc-edited-state ,(nord--face :foreground 13))
   `(vc-locally-added-state ,(nord--face :underline 14))
   `(vc-locked-state ,(nord--face :foreground 10))
   `(vc-missing-state ,(nord--face :foreground 11))
   `(vc-needs-update-state ,(nord--face :foreground 12))
   `(vc-removed-state ,(nord--face :foreground 11))
   `(vc-state-base ,(nord--face :foreground 4))
   `(vc-up-to-date-state ,(nord--face :foreground 8))
   `(vertical-border ,(nord--face :foreground 2))
   `(which-func ,(nord--face :foreground 8))
   `(whitespace-big-indent ,(nord--face :foreground 3 :background 0))
   `(whitespace-empty ,(nord--face :foreground 3 :background 0))
   `(whitespace-hspace ,(nord--face :foreground 3 :background 0))
   `(whitespace-indentation ,(nord--face :foreground 3 :background 0))
   `(whitespace-line ,(nord--face :background 0))
   `(whitespace-newline ,(nord--face :foreground 3 :background 0))
   `(whitespace-space ,(nord--face :foreground 3 :background 0))
   `(whitespace-space-after-tab ,(nord--face :foreground 3 :background 0))
   `(whitespace-space-before-tab ,(nord--face :foreground 3 :background 0))
   `(whitespace-tab ,(nord--face :foreground 3 :background 0))
   `(whitespace-trailing ,(nord--face :inherit 'trailing-whitespace))
   `(widget-button-pressed ,(nord--face :foreground 9 :background 1))
   `(widget-documentation ,(nord--face :foreground 4))
   `(widget-field ,(nord--face :background 2 :foreground 4))
   `(widget-single-line-field ,(nord--face :background 2 :foreground 4))
   `(window-divider ,(nord--face :background 3))
   `(window-divider-first-pixel ,(nord--face :background 3))
   `(window-divider-last-pixel ,(nord--face :background 3))

    ;;;; +-----------------+
    ;;;; + Package Support +
    ;;;; +-----------------+
   ;; +--- Syntax ---+
   ;; > Auctex
   `(font-latex-bold-face ,(nord--face :inherit 'bold))
   `(font-latex-italic-face ,(nord--face :inherit 'italic))
   `(font-latex-math-face ,(nord--face :foreground 8))
   `(font-latex-sectioning-0-face ,(nord--face :foreground 8 :weight 'bold))
   `(font-latex-sectioning-1-face ,(nord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-2-face ,(nord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-3-face ,(nord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-4-face ,(nord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-5-face ,(nord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-script-char-face ,(nord--face :inherit 'font-lock-warning-face))
   `(font-latex-string-face ,(nord--face :inherit 'font-lock-string-face))
   `(font-latex-warning-face ,(nord--face :inherit 'font-lock-warning-face))

   ;; > Elixir
   `(elixir-attribute-face ,(nord--face :foreground nord-annotation))
   `(elixir-atom-face ,(nord--face :foreground 4 :weight 'bold))

   ;; > Enhanced Ruby
   `(enh-ruby-heredoc-delimiter-face ,(nord--face :foreground 14))
   `(enh-ruby-op-face ,(nord--face :foreground 9))
   `(enh-ruby-regexp-delimiter-face ,(nord--face :foreground 13))
   `(enh-ruby-regexp-face ,(nord--face :foreground 13))
   `(enh-ruby-string-delimiter-face ,(nord--face :foreground 14))
   `(erm-syn-errline ,(nord--face :foreground 11 :underline t))
   `(erm-syn-warnline ,(nord--face :foreground 13 :underline t))

   ;; > Java Development Environment for Emacs
   `(jdee-db-active-breakpoint-face ,(nord--face :background 2 :weight 'bold))
   `(jdee-bug-breakpoint-cursor ,(nord--face :background 2))
   `(jdee-db-requested-breakpoint-face ,(nord--face :foreground 13 :background 2 :weight 'bold))
   `(jdee-db-spec-breakpoint-face ,(nord--face :foreground 14 :background 2 :weight 'bold))
   `(jdee-font-lock-api-face ,(nord--face :foreground 4))
   `(jdee-font-lock-code-face ,(nord--face :slant 'italic))
   `(jdee-font-lock-constant-face ,(nord--face :foreground nord-keyword))
   `(jdee-font-lock-constructor-face ,(nord--face :foreground nord-method))
   `(jdee-font-lock-doc-tag-face ,(nord--face :foreground 7))
   `(jdee-font-lock-link-face ,(nord--face :underline t))
   `(jdee-font-lock-modifier-face ,(nord--face :foreground nord-keyword))
   `(jdee-font-lock-number-face ,(nord--face :foreground nord-numeric))
   `(jdee-font-lock-operator-fac ,(nord--face :foreground nord-operator))
   `(jdee-font-lock-package-face ,(nord--face :foreground nord-class))
   `(jdee-font-lock-pre-face ,(nord--face :foreground nord-comment :slant 'italic))
   `(jdee-font-lock-private-face ,(nord--face :foreground nord-keyword))
   `(jdee-font-lock-public-face ,(nord--face :foreground nord-keyword))
   `(jdee-font-lock-variable-face ,(nord--face :foreground nord-variable))

   ;; > JavaScript 2
   `(js2-function-call ,(nord--face :foreground 8))
   `(js2-private-function-call ,(nord--face :foreground 8))
   `(js2-jsdoc-html-tag-delimiter ,(nord--face :foreground 6))
   `(js2-jsdoc-html-tag-name ,(nord--face :foreground 9))
   `(js2-external-variable ,(nord--face :foreground 4))
   `(js2-function-param ,(nord--face :foreground 4))
   `(js2-jsdoc-value ,(nord--face :foreground nord-comment))
   `(js2-jsdoc-tag ,(nord--face :foreground 7))
   `(js2-jsdoc-type ,(nord--face :foreground 7))
   `(js2-private-member ,(nord--face :foreground 4))
   `(js2-object-property ,(nord--face :foreground 4))
   `(js2-error ,(nord--face :foreground 11))
   `(js2-warning ,(nord--face :foreground 13))
   `(js2-instance-member ,(nord--face :foreground 4))

   ;; > JavaScript 3
   `(js3-error-face ,(nord--face :foreground 11))
   `(js3-external-variable-face ,(nord--face :foreground 4))
   `(js3-function-param-face ,(nord--face :foreground 4))
   `(js3-instance-member-face ,(nord--face :foreground 4))
   `(js3-jsdoc-html-tag-delimiter-face ,(nord--face :foreground 6))
   `(js3-jsdoc-html-tag-name-face ,(nord--face :foreground 9))
   `(js3-jsdoc-tag-face ,(nord--face :foreground 9))
   `(js3-jsdoc-type-face ,(nord--face :foreground 7))
   `(js3-jsdoc-value-face ,(nord--face :foreground 4))
   `(js3-magic-paren-face ,(nord--face :inherit 'show-paren-match-face))
   `(js3-private-function-call-face ,(nord--face :foreground 8))
   `(js3-private-member-face ,(nord--face :foreground 4))
   `(js3-warning-face ,(nord--face :foreground 13))

   ;; > Markdown
   `(markdown-blockquote-face ,(nord--face :foreground nord-comment))
   `(markdown-bold-face ,(nord--face :inherit 'bold))
   `(markdown-header-face-1 ,(nord--face :foreground 8))
   `(markdown-header-face-2 ,(nord--face :foreground 8))
   `(markdown-header-face-3 ,(nord--face :foreground 8))
   `(markdown-header-face-4 ,(nord--face :foreground 8))
   `(markdown-header-face-5 ,(nord--face :foreground 8))
   `(markdown-header-face-6 ,(nord--face :foreground 8))
   `(markdown-inline-code-face ,(nord--face :foreground 7))
   `(markdown-italic-face ,(nord--face :inherit 'italic))
   `(markdown-link-face ,(nord--face :foreground 8))
   `(markdown-markup-face ,(nord--face :foreground 9))
   `(markdown-reference-face ,(nord--face :inherit 'markdown-link-face))
   `(markdown-url-face ,(nord--face :foreground 4 :underline t))

   ;; > Rainbow Delimeters
   `(rainbow-delimiters-depth-1-face ,(nord--face :foreground 7))
   `(rainbow-delimiters-depth-2-face ,(nord--face :foreground 8))
   `(rainbow-delimiters-depth-3-face ,(nord--face :foreground 9))
   `(rainbow-delimiters-depth-4-face ,(nord--face :foreground 10))
   `(rainbow-delimiters-depth-5-face ,(nord--face :foreground 12))
   `(rainbow-delimiters-depth-6-face ,(nord--face :foreground 13))
   `(rainbow-delimiters-depth-7-face ,(nord--face :foreground 14))
   `(rainbow-delimiters-depth-8-face ,(nord--face :foreground 15))
   `(rainbow-delimiters-unmatched-face ,(nord--face :foreground 11))

   ;; > Web Mode
   `(web-mode-attr-tag-custom-face ,(nord--face :foreground nord-attribute))
   `(web-mode-builtin-face ,(nord--face :foreground nord-keyword))
   `(web-mode-comment-face ,(nord--face :foreground nord-comment))
   `(web-mode-comment-keyword-face ,(nord--face :foreground nord-comment))
   `(web-mode-constant-face ,(nord--face :foreground nord-variable))
   `(web-mode-css-at-rule-face ,(nord--face :foreground nord-annotation))
   `(web-mode-css-function-face ,(nord--face :foreground nord-method))
   `(web-mode-css-property-name-face ,(nord--face :foreground nord-keyword))
   `(web-mode-css-pseudo-class-face ,(nord--face :foreground nord-class))
   `(web-mode-css-selector-face ,(nord--face :foreground nord-keyword))
   `(web-mode-css-string-face ,(nord--face :foreground nord-string))
   `(web-mode-doctype-face ,(nord--face :foreground nord-preprocessor))
   `(web-mode-function-call-face ,(nord--face :foreground nord-method))
   `(web-mode-function-name-face ,(nord--face :foreground nord-method))
   `(web-mode-html-attr-name-face ,(nord--face :foreground nord-attribute))
   `(web-mode-html-attr-equal-face ,(nord--face :foreground nord-punctuation))
   `(web-mode-html-attr-value-face ,(nord--face :foreground nord-string))
   `(web-mode-html-entity-face ,(nord--face :foreground nord-keyword))
   `(web-mode-html-tag-bracket-face ,(nord--face :foreground nord-punctuation))
   `(web-mode-html-tag-custom-face ,(nord--face :foreground nord-tag))
   `(web-mode-html-tag-face ,(nord--face :foreground nord-tag))
   `(web-mode-html-tag-namespaced-face ,(nord--face :foreground nord-keyword))
   `(web-mode-json-key-face ,(nord--face :foreground nord-class))
   `(web-mode-json-string-face ,(nord--face :foreground nord-string))
   `(web-mode-keyword-face ,(nord--face :foreground nord-keyword))
   `(web-mode-preprocessor-face ,(nord--face :foreground nord-preprocessor))
   `(web-mode-string-face ,(nord--face :foreground nord-string))
   `(web-mode-symbol-face ,(nord--face :foreground nord-variable))
   `(web-mode-type-face ,(nord--face :foreground nord-class))
   `(web-mode-warning-face ,(nord--face :inherit 'font-lock-warning-face))
   `(web-mode-variable-name-face ,(nord--face :foreground nord-variable))

   ;; +--- UI ---+
   ;; > Anzu
   `(anzu-mode-line ,(nord--face :foreground 8))
   `(anzu-mode-line-no-match ,(nord--face :foreground 11))

   ;; > Avy
   `(avy-lead-face ,(nord--face :background 11 :foreground 5))
   `(avy-lead-face-0 ,(nord--face :background 10 :foreground 5))
   `(avy-lead-face-1 ,(nord--face :background 3 :foreground 5))
   `(avy-lead-face-2 ,(nord--face :background 15 :foreground 5))

   ;; > Company
   `(company-echo-common ,(nord--face :foreground 0 :background 4))
   `(company-preview ,(nord--face :foreground 4 :background 10))
   `(company-preview-common ,(nord--face :foreground 0 :background 8))
   `(company-preview-search ,(nord--face :foreground 0 :background 8))
   `(company-scrollbar-bg ,(nord--face :foreground 1 :background 1))
   `(company-scrollbar-fg ,(nord--face :foreground 2 :background 2))
   `(company-template-field ,(nord--face :foreground 0 :background 7))
   `(company-tooltip ,(nord--face :foreground 4 :background 2))
   `(company-tooltip-annotation ,(nord--face :foreground 12))
   `(company-tooltip-annotation-selection ,(nord--face :foreground 12 :weight 'bold))
   `(company-tooltip-common ,(nord--face :foreground 8))
   `(company-tooltip-common-selection ,(nord--face :foreground 8 :background 3))
   `(company-tooltip-mouse ,(nord--face :inherit 'highlight))
   `(company-tooltip-selection ,(nord--face :background 3 :weight 'bold))

   ;; > diff-hl
   `(diff-hl-change ,(nord--face :background 13))
   `(diff-hl-insert ,(nord--face :background 14))
   `(diff-hl-delete ,(nord--face :background 11))
   
   ;; > Evil
   `(evil-ex-info ,(nord--face :foreground 8))
   `(evil-ex-substitute-replacement ,(nord--face :foreground 9))
   `(evil-ex-substitute-matches ,(nord--face :inherit 'isearch))

   ;; > Flycheck
   `(flycheck-error ,(nord--face :underline '(:style wave :color 11)))
   `(flycheck-fringe-error ,(nord--face :foreground 11 :weight 'bold))
   `(flycheck-fringe-info ,(nord--face :foreground 8 :weight 'bold))
   `(flycheck-fringe-warning ,(nord--face :foreground 13 :weight 'bold))
   `(flycheck-info ,(nord--face :underline '(:style wave :color 8)))
   `(flycheck-warning ,(nord--face :underline '(:style wave :color 13)))

   ;; > Git Gutter
   `(git-gutter:modified ,(nord--face :foreground 13))
   `(git-gutter:added ,(nord--face :foreground 14))
   `(git-gutter:deleted ,(nord--face :foreground 11))

   ;; > Git Gutter Plus
   `(git-gutter+-modified ,(nord--face :foreground 13))
   `(git-gutter+-added ,(nord--face :foreground 14))
   `(git-gutter+-deleted ,(nord--face :foreground 11))

   ;; > Helm
   `(helm-bookmark-addressbook ,(nord--face :foreground 7))
   `(helm-bookmark-directory ,(nord--face :foreground 9))
   `(helm-bookmark-file ,(nord--face :foreground 8))
   `(helm-bookmark-gnus ,(nord--face :foreground 10))
   `(helm-bookmark-info ,(nord--face :foreground 14))
   `(helm-bookmark-man ,(nord--face :foreground 4))
   `(helm-bookmark-w3m ,(nord--face :foreground 9))
   `(helm-buffer-directory ,(nord--face :foreground 9))
   `(helm-buffer-file ,(nord--face :foreground 8))
   `(helm-buffer-not-saved ,(nord--face :foreground 13))
   `(helm-buffer-process ,(nord--face :foreground 10))
   `(helm-candidate-number ,(nord--face :foreground 4 :weight 'bold))
   `(helm-candidate-number-suspended ,(nord--face :foreground 4))
   `(helm-ff-directory ,(nord--face :foreground 9 :weight 'bold))
   `(helm-ff-dirs ,(nord--face :foreground 9))
   `(helm-ff-dotted-director ,(nord--face :foreground 9 :underline t))
   `(helm-ff-dotted-symlink-director ,(nord--face :foreground 7 :weight 'bold))
   `(helm-ff-executable ,(nord--face :foreground 8))
   `(helm-ff-file ,(nord--face :foreground 4))
   `(helm-ff-invalid-symlink ,(nord--face :foreground 11 :weight 'bold))
   `(helm-ff-prefix ,(nord--face :foreground 0 :background 9))
   `(helm-ff-symlink ,(nord--face :foreground 7))
   `(helm-grep-cmd-line ,(nord--face :foreground 4 :background 0))
   `(helm-grep-file ,(nord--face :foreground 8))
   `(helm-grep-finish ,(nord--face :foreground 5))
   `(helm-grep-lineno ,(nord--face :foreground 4))
   `(helm-grep-match ,(nord--face :inherit 'isearch))
   `(helm-grep-running ,(nord--face :foreground 8))
   `(helm-header ,(nord--face :foreground 9 :background 2))
   `(helm-header-line-left-margin ,(nord--face :foreground 9 :background 2))
   `(helm-history-deleted ,(nord--face :foreground 11))
   `(helm-history-remote ,(nord--face :foreground 4))
   `(helm-lisp-completion-info ,(nord--face :foreground 4 :weight 'bold))
   `(helm-lisp-show-completion ,(nord--face :inherit 'isearch))
   `(helm-locate-finish ,(nord--face :foreground 14))
   `(helm-match ,(nord--face :foreground 8))
   `(helm-match-item ,(nord--face :inherit 'isearch))
   `(helm-moccur-buffer ,(nord--face :foreground 8))
   `(helm-resume-need-update ,(nord--face :foreground 0 :background 13))
   `(helm-selection ,(nord--face :inherit 'highlight))
   `(helm-selection-line ,(nord--face :background 2))
   `(helm-source-header ,(nord--face :height 1.44 :foreground 8 :background 2))
   `(helm-swoop-line-number-face ,(nord--face :foreground 4 :background 0))
   `(helm-swoop-target-word-face ,(nord--face :foreground 0 :background 7))
   `(helm-swoop-target-line-face ,(nord--face :background 13 :foreground 3))
   `(helm-swoop-target-line-block-face ,(nord--face :background 13 :foreground 3))
   `(helm-separator ,(nord--face :background 2))
   `(helm-visible-mark ,(nord--face :background 2))

   ;; > Magit
   `(magit-branch ,(nord--face :foreground 7 :weight 'bold))
   `(magit-diff-context-highlight ,(nord--face :background 2))
   `(magit-diff-file-header ,(nord--face :foreground 8 :box '(:color 8)))
   `(magit-diffstat-added ,(nord--face :foreground 14))
   `(magit-diffstat-removed ,(nord--face :foreground 11))
   `(magit-hash ,(nord--face :foreground 8))
   `(magit-hunk-heading ,(nord--face :foreground 9))
   `(magit-hunk-heading-highlight ,(nord--face :foreground 9 :background 2))
   `(magit-item-highlight ,(nord--face :foreground 8 :background 2))
   `(magit-log-author ,(nord--face :foreground 7))
   `(magit-process-ng ,(nord--face :foreground 13 :weight 'bold))
   `(magit-process-ok ,(nord--face :foreground 14 :weight 'bold))
   `(magit-section-heading ,(nord--face :foreground 7 :weight 'bold))
   `(magit-section-highlight ,(nord--face :background 2))

   ;; > MU4E
   `(mu4e-header-marks-face ,(nord--face :foreground 9))
   `(mu4e-title-face ,(nord--face :foreground 8))
   `(mu4e-header-key-face ,(nord--face :foreground 8))
   `(mu4e-highlight-face ,(nord--face :highlight))
   `(mu4e-flagged-face ,(nord--face :foreground 13))
   `(mu4e-unread-face ,(nord--face :foreground 13 :weight 'bold))
   `(mu4e-link-face ,(nord--face :underline t))

   ;; > Powerline
   `(powerline-active1 ,(nord--face :foreground 4 :background 1))
   `(powerline-active2 ,(nord--face :foreground 4 :background 3))
   `(powerline-inactive1 ,(nord--face :background 2))
   `(powerline-inactive2 ,(nord--face :background 2))

   ;; > Powerline Evil
   `(powerline-evil-base-face ,(nord--face :foreground 4))
   `(powerline-evil-normal-face ,(nord--face :background 8))
   `(powerline-evil-insert-face ,(nord--face :foreground 0 :background 4))
   `(powerline-evil-visual-face ,(nord--face :foreground 0 :background 7))
   `(powerline-evil-replace-face ,(nord--face :foreground 0 :background 9))

   ;; > NeoTree
   `(neo-banner-face ,(nord--face :foreground 10))
   `(neo-dir-link-face ,(nord--face :foreground 9))
   `(neo-expand-btn-face ,(nord--face :foreground 6 :bold t))
   `(neo-file-link-face ,(nord--face :foreground 4))
   `(neo-root-dir-face ,(nord--face :foreground 7 :weight 'bold))
   `(neo-vc-added-face ,(nord--face :foreground 14))
   `(neo-vc-conflict-face ,(nord--face :foreground 11))
   `(neo-vc-default-face ,(nord--face :foreground 4))
   `(neo-vc-edited-face ,(nord--face :foreground 13))
   `(neo-vc-ignored-face ,(nord--face :foreground 3))
   `(neo-vc-missing-face ,(nord--face :foreground 12))
   `(neo-vc-needs-merge-face ,(nord--face :background 12 :foreground 4))
   `(neo-vc-needs-update-face ,(nord--face :background 10 :foreground 4))
   `(neo-vc-removed-face ,(nord--face :foreground 11 :strike-through nil))
   `(neo-vc-up-to-date-face ,(nord--face :foreground 4))
   `(neo-vc-user-face ,(nord--face :foreground 4))

   ;; > Cider
   `(cider-result-overlay-face ((t (:background unspecified)))

   ;; > Org
   `(org-level-1 ,(nord--face :foreground 7 :weight extra-bold))
   `(org-level-2 ,(nord--face :foreground 8 :weight 'bold))
   `(org-level-3 ,(nord--face :foreground 9 :weight semi-bold))
   `(org-level-4 ,(nord--face :foreground 10 :weight normal))
   `(org-level-5 ,(nord--face :inherit 'org-level-4))
   `(org-level-6 ,(nord--face :inherit 'org-level-4))
   `(org-level-7 ,(nord--face :inherit 'org-level-4))
   `(org-level-8 ,(nord--face :inherit 'org-level-4))
   `(org-agenda-structure ,(nord--face :foreground 9))
   `(org-agenda-date ,(nord--face :foreground 8 :underline nil))
   `(org-agenda-done ,(nord--face :foreground 14))
   `(org-agenda-dimmed-todo-face ,(nord--face :background 13))
   `(org-block ,(nord--face :foreground 4))
   `(org-block-background ,(nord--face :background 0))
   `(org-block-begin-line ,(nord--face :foreground 7))
   `(org-block-end-line ,(nord--face :foreground 7))
   `(org-checkbox ,(nord--face :foreground 9))
   `(org-checkbox-statistics-done ,(nord--face :foreground 14))
   `(org-checkbox-statistics-todo ,(nord--face :foreground 13))
   `(org-code ,(nord--face :foreground 7))
   `(org-column ,(nord--face :background 2))
   `(org-column-title ,(nord--face :inherit 'org-column :weight 'bold :underline t))
   `(org-date ,(nord--face :foreground 8))
   `(org-document-info ,(nord--face :foreground 4))
   `(org-document-info-keyword ,(nord--face :foreground 3 :weight 'bold))
   `(org-document-title ,(nord--face :foreground 8 :weight 'bold))
   `(org-done ,(nord--face :foreground 14 :weight 'bold))
   `(org-ellipsis ,(nord--face :foreground 3))
   `(org-footnote ,(nord--face :foreground 8))
   `(org-formula ,(nord--face :foreground 9))
   `(org-hide ,(nord--face :foreground 0 :background 0))
   `(org-link ,(nord--face :underline t))
   `(org-scheduled ,(nord--face :foreground 14))
   `(org-scheduled-previously ,(nord--face :foreground 13))
   `(org-scheduled-today ,(nord--face :foreground 8))
   `(org-special-keyword ,(nord--face :foreground 9))
   `(org-table ,(nord--face :foreground 9))
   `(org-todo ,(nord--face :foreground 13 :weight 'bold))
   `(org-upcoming-deadline ,(nord--face :foreground 12))
   `(org-warning ,(nord--face :foreground 13 :weight 'bold))
   `(font-latex-bold-face ,(nord--face :inherit 'bold))
   `(font-latex-italic-face ,(nord--face :slant 'italic))
   `(font-latex-string-face ,(nord--face :foreground 14))
   `(font-latex-match-reference-keywords ,(nord--face :foreground 9))
   `(font-latex-match-variable-keywords ,(nord--face :foreground 4))
   `(ido-only-match ,(nord--face :foreground 8))
   `(org-sexp-date ,(nord--face :foreground 7))
   `(ido-first-match ,(nord--face :foreground 8 :weight 'bold))
   `(ido-subdir ,(nord--face :foreground 9))
   `(org-quote ,(nord--face :inherit 'org-block :slant 'italic))
   `(org-verse ,(nord--face :inherit 'org-block :slant 'italic))
   `(org-agenda-date-weekend ,(nord--face :foreground 9))
   `(org-agenda-date-today ,(nord--face :foreground 8 :weight 'bold))
   `(org-agenda-done ,(nord--face :foreground 14))
   `(org-verbatim ,(nord--face :foreground 7))

   ;; > ivy-mode
   `(ivy-current-match ,(nord--face :inherit 'region))
   `(ivy-minibuffer-match-face-1 ,(nord--face :inherit 'default))
   `(ivy-minibuffer-match-face-2 ,(nord--face :background 7 :foreground 0))
   `(ivy-minibuffer-match-face-3 ,(nord--face :background 8 :foreground 0))
   `(ivy-minibuffer-match-face-4 ,(nord--face :background 9 :foreground 0))
   `(ivy-remote ,(nord--face :foreground 14))
   `(ivy-posframe ,(nord--face :background 1))
   `(ivy-posframe-border ,(nord--face :background 1))
   `(ivy-remote ,(nord--face :foreground 14))

   ;; > perspective
   `(persp-selected-face ,(nord--face :foreground 8 :weight 'bold)))))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nord)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; nord-theme.el ends here
