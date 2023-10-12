;;; fnord-theme.el --- Nord, fixed: An arctic, north-bluish clean and elegant theme

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

;; Nord is a 16 colorspace theme build to run in GUI- and terminal
;; mode with support for many third-party syntax- and UI packages.

;; This project implements the Nord theme for the Emacs editor;
;; it is a fork of a similar project called nordtheme/emacs.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Nord theme requires Emacs 24 or later!"))

(deftheme fnord "Nord, fixed: an arctic, north-bluish clean and elegant theme")

(require 'fnord-colours)

(defun fnord--get-colour (n col-256)
  "Retrieve nord colour N. If COL-256 is t, return the colour for terminals.
Colour 16 is the special comment colour."
  (cl-assert (and (>= n 0) (< n 17)))                  
  (when n
    (nth n (if col-256
               (list fnord-0 fnord-1-256 fnord-2-256 fnord-3-256 fnord-4-256 fnord-5-256 fnord-6-256 fnord-7-256 fnord-8-256 fnord-9-256 fnord-10-256 fnord-11-256 fnord-12-256 fnord-13-256 fnord-14-256 fnord-15-256 fnord-comment-colour-256)
             (list fnord-0 fnord-1 fnord-2 fnord-3 fnord-4 fnord-5 fnord-6 fnord-7 fnord-8 fnord-9 fnord-10 fnord-11 fnord-12 fnord-13 fnord-14 fnord-15 fnord-comment-colour)))))


(defconst fnord--class '((class color) (min-colors 257)))
(defconst fnord--class-256 '((class color) (min-colors 89)))


(defun fnord--subst-colours (face-spec col-256)
  "If foreground or background in FACE-SPEC are given as ints, replace them using `fnord--get-colour'."
  (dolist (prop '(:foreground :background))
    (when-let ((val (plist-get face-spec prop)))
      (when (numberp val)
        (plist-put face-spec prop (fnord--get-colour val col-256)))))
  face-spec)

(defun fnord--face (&rest face-spec)
  "Create FACE-SPEC where foreground and background can be numbers. Those will
be substituted using `fnord--get-colour'."
  `((,fnord--class . ,(fnord--subst-colours face-spec nil))
    (,fnord--class-256 . ,(fnord--subst-colours face-spec t))))


;;;; Color Constants
(let* ((fnord-comment 16)
       (fnord-annotation 12)
       (fnord-attribute 7)
       (fnord-class 7)
       (fnord-escape 12)
       (fnord-method 8)
       (fnord-keyword 9)
       (fnord-numeric 15)
       (fnord-operator 9)
       (fnord-preprocessor 10)
       (fnord-punctuation 4)
       (fnord-regexp 13)
       (fnord-string 14)
       (fnord-tag 9)
       (fnord-variable 4)
       (fnord-region-highlight-foreground (if (or
                                              (string= fnord-region-highlight "frost")
                                              (string= fnord-region-highlight "snowstorm")) "#2E3440" nil))
       (fnord-region-highlight-background (if
                                             (string= fnord-region-highlight "frost") "#88C0D0"
                                           (if (string= fnord-region-highlight "snowstorm") "#D8DEE9" "#434C5E")))
       (fnord-uniform-mode-lines-background (if fnord-uniform-mode-lines "#4C566A" "#3B4252")))
  
;;;; +------------+
;;;; + Core Faces +
;;;; +------------+
  (custom-theme-set-faces
   'fnord
   ;; +--- Base ---+
   `(bold ,(fnord--face :weight 'bold))
   `(bold-italic ,(fnord--face :weight 'bold :slant 'italic))
   `(default ,(fnord--face :foreground 4 :background 0))
   `(error ,(fnord--face :foreground 11 :weight 'bold))
   `(escape-glyph ,(fnord--face :foreground 12))
   `(font-lock-builtin-face ,(fnord--face :foreground 9))
   `(font-lock-comment-face ,(fnord--face :foreground fnord-comment))
   `(font-lock-comment-delimiter-face ,(fnord--face :foreground fnord-comment))
   `(font-lock-constant-face ,(fnord--face :foreground 9))
   `(font-lock-doc-face ,(fnord--face :foreground fnord-comment))
   `(font-lock-function-name-face ,(fnord--face :foreground 8))
   `(font-lock-keyword-face ,(fnord--face :foreground 9))
   `(font-lock-negation-char-face ,(fnord--face :foreground 9))
   `(font-lock-preprocessor-face ,(fnord--face :foreground 10 :weight 'bold))
   `(font-lock-reference-face ,(fnord--face :foreground 9))
   `(font-lock-regexp-grouping-backslash ,(fnord--face :foreground 13))
   `(font-lock-regexp-grouping-construct ,(fnord--face :foreground 13))
   `(font-lock-string-face ,(fnord--face :foreground 14))
   `(font-lock-type-face ,(fnord--face :foreground 7))
   `(font-lock-variable-name-face ,(fnord--face :foreground 4))
   `(font-lock-warning-face ,(fnord--face :foreground 13))
   `(italic ,(fnord--face :slant 'italic))
   `(region ,(fnord--face :foreground (or fnord-region-highlight-foreground 'unspecified) :background (or fnord-region-highlight-background 'unspecified)))   
   `(shadow ,(fnord--face :foreground 3))
   `(underline ,(fnord--face :underline t))   
   `(warning ,(fnord--face :foreground 13 :weight 'bold))

   ;; +--- Syntax ---+
   ;; > C
   `(c-annotation-face ,(fnord--face :foreground fnord-annotation))

   ;; > diff
   `(diff-added ,(fnord--face :foreground 14))
   `(diff-changed ,(fnord--face :foreground 13))
   `(diff-context ,(fnord--face :inherit 'default))
   `(diff-file-header ,(fnord--face :foreground 8))
   `(diff-function ,(fnord--face :foreground 7))
   `(diff-header ,(fnord--face :foreground 9 :weight 'bold))
   `(diff-hunk-header ,(fnord--face :foreground 9 :background 0))
   `(diff-indicator-added ,(fnord--face :foreground 14))
   `(diff-indicator-changed ,(fnord--face :foreground 13))
   `(diff-indicator-removed ,(fnord--face :foreground 11))
   `(diff-nonexistent ,(fnord--face :foreground 11))
   `(diff-refine-added ,(fnord--face :foreground 14))
   `(diff-refine-changed ,(fnord--face :foreground 13))
   `(diff-refine-removed ,(fnord--face :foreground 11))
   `(diff-removed ,(fnord--face :foreground 11))

   ;; +--- UI ---+
   `(border ,(fnord--face :foreground 4))
   `(buffer-menu-buffer ,(fnord--face :foreground 4 :weight 'bold))
   `(button ,(fnord--face :background 0 :foreground 8 :box (list :line-width 1 :color fnord-8 :style 'sunken-button)))
   `(completions-annotations ,(fnord--face :foreground 9))
   `(completions-common-part ,(fnord--face :foreground 8 :weight 'bold))
   `(completions-first-difference ,(fnord--face :foreground 11))
   `(custom-button ,(fnord--face :background 0 :foreground 8 :box (list :line-width 1 :color fnord-8 :style 'sunken-button)))
   `(custom-button-mouse ,(fnord--face :background 4 :foreground 0 :box (list :line-width 1 :color fnord-8 :style 'sunken-button)))
   `(custom-button-pressed ,(fnord--face :background 6 :foreground 0 :box (list :line-width 1 :color fnord-8 :style 'sunken-button)))
   `(custom-button-pressed-unraised ,(fnord--face :background 4 :foreground 0 :box
                                                  (list :line-width 1 :color fnord-8 :style 'sunken-button)))
   `(custom-button-unraised ,(fnord--face :background 0 :foreground 8 :box
                                          (list :line-width 1 :color fnord-8 :style 'sunken-button)))
   `(custom-changed ,(fnord--face :foreground 13))
   `(custom-comment ,(fnord--face :foreground fnord-comment))
   `(custom-comment-tag ,(fnord--face :foreground 7))
   `(custom-documentation ,(fnord--face :foreground 4))
   `(custom-group-tag ,(fnord--face :foreground 8 :weight 'bold))
   `(custom-group-tag-1 ,(fnord--face :foreground 8 :weight 'bold))
   `(custom-invalid ,(fnord--face :foreground 11))
   `(custom-modified ,(fnord--face :foreground 13))
   `(custom-rogue ,(fnord--face :foreground 12 :background 2))
   `(custom-saved ,(fnord--face :foreground 14))
   `(custom-set ,(fnord--face :foreground 8))
   `(custom-state ,(fnord--face :foreground 14))
   `(custom-themed ,(fnord--face :foreground 8 :background 2))
   `(cursor ,(fnord--face :background 4))
   `(fringe ,(fnord--face :foreground 4 :background 0))
   `(file-name-shadow ,(fnord--face :inherit 'shadow))
   `(header-line ,(fnord--face :foreground 4 :background 2))
   `(help-argument-name ,(fnord--face :foreground 8))
   `(highlight ,(fnord--face :foreground 8 :background 2))
   `(hl-line ,(fnord--face :background 1))
   `(info-menu-star ,(fnord--face :foreground 9))
   `(isearch ,(fnord--face :foreground 0 :background 8))
   `(isearch-fail ,(fnord--face :foreground 11))
   `(link ,(fnord--face :underline t))
   `(link-visited ,(fnord--face :underline t))
   `(linum ,(fnord--face :foreground 3 :background 0))
   `(linum-relative-current-face ,(fnord--face :foreground 3 :background 0))
   `(line-number-current-line ,(fnord--face :foreground 6))
   `(match ,(fnord--face :inherit 'isearch))
   `(message-cited-text ,(fnord--face :foreground 4))
   `(message-header-cc ,(fnord--face :foreground 9))
   `(message-header-name ,(fnord--face :foreground 7))
   `(message-header-newsgroup ,(fnord--face :foreground 14))
   `(message-header-other ,(fnord--face :foreground 4))
   `(message-header-subject ,(fnord--face :foreground 8))
   `(message-header-to ,(fnord--face :foreground 9))
   `(message-header-xheader ,(fnord--face :foreground 13))
   `(message-mml ,(fnord--face :foreground 10))
   `(message-separator ,(fnord--face :inherit 'shadow))
   `(minibuffer-prompt ,(fnord--face :foreground 8 :weight 'bold))
   `(mm-command-output ,(fnord--face :foreground 8))
   `(mode-line ,(fnord--face :foreground 8 :background 3))
   `(mode-line-buffer-id ,(fnord--face :weight 'bold))
   `(mode-line-highlight ,(fnord--face :inherit 'highlight))
   `(mode-line-inactive ,(fnord--face :foreground 4 :background fnord-uniform-mode-lines-background))
   `(next-error ,(fnord--face :inherit 'error))
   `(nobreak-space ,(fnord--face :foreground 3))
   `(outline-1 ,(fnord--face :foreground 8 :weight 'bold))
   `(outline-2 ,(fnord--face :inherit 'outline-1))
   `(outline-3 ,(fnord--face :inherit 'outline-1))
   `(outline-4 ,(fnord--face :inherit 'outline-1))
   `(outline-5 ,(fnord--face :inherit 'outline-1))
   `(outline-6 ,(fnord--face :inherit 'outline-1))
   `(outline-7 ,(fnord--face :inherit 'outline-1))
   `(outline-8 ,(fnord--face :inherit 'outline-1))
   `(package-description ,(fnord--face :foreground 4))
   `(package-help-section-name ,(fnord--face :foreground 8 :weight 'bold))
   `(package-name ,(fnord--face :foreground 8))
   `(package-status-available ,(fnord--face :foreground 7))
   `(package-status-avail-obso ,(fnord--face :foreground 7 :slant 'italic))
   `(package-status-built-in ,(fnord--face :foreground 9))
   `(package-status-dependency ,(fnord--face :foreground 8 :slant 'italic))
   `(package-status-disabled ,(fnord--face :foreground 3))
   `(package-status-external ,(fnord--face :foreground 12 :slant 'italic))
   `(package-status-held ,(fnord--face :foreground 4 :weight 'bold))
   `(package-status-new ,(fnord--face :foreground 14))
   `(package-status-incompat ,(fnord--face :foreground 11))
   `(package-status-installed ,(fnord--face :foreground 7 :weight 'bold))
   `(package-status-unsigned ,(fnord--face :underline t))
   `(query-replace ,(fnord--face :foreground 8 :background 2))
   `(scroll-bar ,(fnord--face :background 3))
   `(secondary-selection ,(fnord--face :background 2))

   ;; `show-paren-match-face` and `show-paren-mismatch-face` are deprecated since Emacs version 22.1 and were
   ;; removed in Emacs 25.
   ;; https://github.com/arcticicestudio/fnord-emacs/issues/75
   ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c430f7e23fc2c22f251ace4254e37dea1452dfc3
   ;; https://github.com/emacs-mirror/emacs/commit/c430f7e23fc2c22f251ace4254e37dea1452dfc3
   `(show-paren-match-face ,(fnord--face :foreground 0 :background 8))
   `(show-paren-mismatch-face ,(fnord--face :background 11))

   `(show-paren-match ,(fnord--face :foreground 0 :background 8))
   `(show-paren-mismatch ,(fnord--face :background 11))
   `(success ,(fnord--face :foreground 14))
   `(term ,(fnord--face :foreground 4 :background 0))
   `(term-color-black ,(fnord--face :foreground 1 :background 1))
   `(term-color-white ,(fnord--face :foreground 5 :background 5))
   `(term-color-cyan ,(fnord--face :foreground 7 :background 7))
   `(term-color-blue ,(fnord--face :foreground 8 :background 8))
   `(term-color-red ,(fnord--face :foreground 11 :background 11))
   `(term-color-yellow ,(fnord--face :foreground 13 :background 13))
   `(term-color-green ,(fnord--face :foreground 14 :background 14))
   `(term-color-magenta ,(fnord--face :foreground 15 :background 15))
   `(tool-bar ,(fnord--face :foreground 4 :background 3))
   `(tooltip ,(fnord--face :foreground 0 :background 4))
   `(trailing-whitespace ,(fnord--face :foreground 3))
   `(tty-menu-disabled-face ,(fnord--face :foreground 1))
   `(tty-menu-enabled-face ,(fnord--face :background 2 :foreground 4))
   `(tty-menu-selected-face ,(fnord--face :foreground 8 :underline t))
   `(undo-tree-visualizer-current-face ,(fnord--face :foreground 8))
   `(undo-tree-visualizer-default-face ,(fnord--face :foreground 4))
   `(undo-tree-visualizer-unmodified-face ,(fnord--face :foreground 4))
   `(undo-tree-visualizer-register-face ,(fnord--face :foreground 9))
   `(vc-conflict-state ,(fnord--face :foreground 12))
   `(vc-edited-state ,(fnord--face :foreground 13))
   `(vc-locally-added-state ,(fnord--face :underline t))
   `(vc-locked-state ,(fnord--face :foreground 10))
   `(vc-missing-state ,(fnord--face :foreground 11))
   `(vc-needs-update-state ,(fnord--face :foreground 12))
   `(vc-removed-state ,(fnord--face :foreground 11))
   `(vc-state-base ,(fnord--face :foreground 4))
   `(vc-up-to-date-state ,(fnord--face :foreground 8))
   `(vertical-border ,(fnord--face :foreground 2))
   `(which-func ,(fnord--face :foreground 8))
   `(whitespace-big-indent ,(fnord--face :foreground 3 :background 0))
   `(whitespace-empty ,(fnord--face :foreground 3 :background 0))
   `(whitespace-hspace ,(fnord--face :foreground 3 :background 0))
   `(whitespace-indentation ,(fnord--face :foreground 3 :background 0))
   `(whitespace-line ,(fnord--face :background 0))
   `(whitespace-newline ,(fnord--face :foreground 3 :background 0))
   `(whitespace-space ,(fnord--face :foreground 3 :background 0))
   `(whitespace-space-after-tab ,(fnord--face :foreground 3 :background 0))
   `(whitespace-space-before-tab ,(fnord--face :foreground 3 :background 0))
   `(whitespace-tab ,(fnord--face :foreground 3 :background 0))
   `(whitespace-trailing ,(fnord--face :inherit 'trailing-whitespace :background 15))  
   `(widget-button-pressed ,(fnord--face :foreground 9 :background 1))
   `(widget-documentation ,(fnord--face :foreground 4))
   `(widget-field ,(fnord--face :background 2 :foreground 4))
   `(widget-single-line-field ,(fnord--face :background 2 :foreground 4))
   `(window-divider ,(fnord--face :background 3))
   `(window-divider-first-pixel ,(fnord--face :background 3))
   `(window-divider-last-pixel ,(fnord--face :background 3))

   `(tab-bar ((t (:inherit mode-line-inactive))))
   `(tab-bar-tab ((t (:inherit mode-line-highlight))))
   `(tab-bar-tab-inactive ((t (:inherit tab-bar)))) '(tab-line ((t nil)))

    ;;;; +-----------------+
    ;;;; + Package Support +
    ;;;; +-----------------+
   ;; +--- Syntax ---+
   ;; > Auctex
   `(font-latex-bold-face ,(fnord--face :inherit 'bold))
   `(font-latex-italic-face ,(fnord--face :inherit 'italic))
   `(font-latex-math-face ,(fnord--face :foreground 8))
   `(font-latex-sectioning-0-face ,(fnord--face :foreground 8 :weight 'bold))
   `(font-latex-sectioning-1-face ,(fnord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-2-face ,(fnord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-3-face ,(fnord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-4-face ,(fnord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-sectioning-5-face ,(fnord--face :inherit 'font-latex-sectioning-0-face))
   `(font-latex-script-char-face ,(fnord--face :inherit 'font-lock-warning-face))
   `(font-latex-string-face ,(fnord--face :inherit 'font-lock-string-face))
   `(font-latex-warning-face ,(fnord--face :inherit 'font-lock-warning-face))

   ;; > Elixir
   `(elixir-attribute-face ,(fnord--face :foreground fnord-annotation))
   `(elixir-atom-face ,(fnord--face :foreground 4 :weight 'bold))

   ;; > Enhanced Ruby
   `(enh-ruby-heredoc-delimiter-face ,(fnord--face :foreground 14))
   `(enh-ruby-op-face ,(fnord--face :foreground 9))
   `(enh-ruby-regexp-delimiter-face ,(fnord--face :foreground 13))
   `(enh-ruby-regexp-face ,(fnord--face :foreground 13))
   `(enh-ruby-string-delimiter-face ,(fnord--face :foreground 14))
   `(erm-syn-errline ,(fnord--face :foreground 11 :underline t))
   `(erm-syn-warnline ,(fnord--face :foreground 13 :underline t))

   ;; > Java Development Environment for Emacs
   `(jdee-db-active-breakpoint-face ,(fnord--face :background 2 :weight 'bold))
   `(jdee-bug-breakpoint-cursor ,(fnord--face :background 2))
   `(jdee-db-requested-breakpoint-face ,(fnord--face :foreground 13 :background 2 :weight 'bold))
   `(jdee-db-spec-breakpoint-face ,(fnord--face :foreground 14 :background 2 :weight 'bold))
   `(jdee-font-lock-api-face ,(fnord--face :foreground 4))
   `(jdee-font-lock-code-face ,(fnord--face :slant 'italic))
   `(jdee-font-lock-constant-face ,(fnord--face :foreground fnord-keyword))
   `(jdee-font-lock-constructor-face ,(fnord--face :foreground fnord-method))
   `(jdee-font-lock-doc-tag-face ,(fnord--face :foreground 7))
   `(jdee-font-lock-link-face ,(fnord--face :underline t))
   `(jdee-font-lock-modifier-face ,(fnord--face :foreground fnord-keyword))
   `(jdee-font-lock-number-face ,(fnord--face :foreground fnord-numeric))
   `(jdee-font-lock-operator-fac ,(fnord--face :foreground fnord-operator))
   `(jdee-font-lock-package-face ,(fnord--face :foreground fnord-class))
   `(jdee-font-lock-pre-face ,(fnord--face :foreground fnord-comment :slant 'italic))
   `(jdee-font-lock-private-face ,(fnord--face :foreground fnord-keyword))
   `(jdee-font-lock-public-face ,(fnord--face :foreground fnord-keyword))
   `(jdee-font-lock-variable-face ,(fnord--face :foreground fnord-variable))

   ;; > JavaScript 2
   `(js2-function-call ,(fnord--face :foreground 8))
   `(js2-private-function-call ,(fnord--face :foreground 8))
   `(js2-jsdoc-html-tag-delimiter ,(fnord--face :foreground 6))
   `(js2-jsdoc-html-tag-name ,(fnord--face :foreground 9))
   `(js2-external-variable ,(fnord--face :foreground 4))
   `(js2-function-param ,(fnord--face :foreground 4))
   `(js2-jsdoc-value ,(fnord--face :foreground fnord-comment))
   `(js2-jsdoc-tag ,(fnord--face :foreground 7))
   `(js2-jsdoc-type ,(fnord--face :foreground 7))
   `(js2-private-member ,(fnord--face :foreground 4))
   `(js2-object-property ,(fnord--face :foreground 4))
   `(js2-error ,(fnord--face :foreground 11))
   `(js2-warning ,(fnord--face :foreground 13))
   `(js2-instance-member ,(fnord--face :foreground 4))

   ;; > JavaScript 3
   `(js3-error-face ,(fnord--face :foreground 11))
   `(js3-external-variable-face ,(fnord--face :foreground 4))
   `(js3-function-param-face ,(fnord--face :foreground 4))
   `(js3-instance-member-face ,(fnord--face :foreground 4))
   `(js3-jsdoc-html-tag-delimiter-face ,(fnord--face :foreground 6))
   `(js3-jsdoc-html-tag-name-face ,(fnord--face :foreground 9))
   `(js3-jsdoc-tag-face ,(fnord--face :foreground 9))
   `(js3-jsdoc-type-face ,(fnord--face :foreground 7))
   `(js3-jsdoc-value-face ,(fnord--face :foreground 4))
   `(js3-magic-paren-face ,(fnord--face :inherit 'show-paren-match-face))
   `(js3-private-function-call-face ,(fnord--face :foreground 8))
   `(js3-private-member-face ,(fnord--face :foreground 4))
   `(js3-warning-face ,(fnord--face :foreground 13))

   ;; > Markdown
   `(markdown-blockquote-face ,(fnord--face :foreground fnord-comment))
   `(markdown-bold-face ,(fnord--face :inherit 'bold))
   `(markdown-header-face-1 ,(fnord--face :foreground 8))
   `(markdown-header-face-2 ,(fnord--face :foreground 8))
   `(markdown-header-face-3 ,(fnord--face :foreground 8))
   `(markdown-header-face-4 ,(fnord--face :foreground 8))
   `(markdown-header-face-5 ,(fnord--face :foreground 8))
   `(markdown-header-face-6 ,(fnord--face :foreground 8))
   `(markdown-inline-code-face ,(fnord--face :foreground 7))
   `(markdown-italic-face ,(fnord--face :inherit 'italic))
   `(markdown-link-face ,(fnord--face :foreground 8))
   `(markdown-markup-face ,(fnord--face :foreground 9))
   `(markdown-reference-face ,(fnord--face :inherit 'markdown-link-face))
   `(markdown-url-face ,(fnord--face :foreground 4 :underline t))

   ;; > Rainbow Delimeters
   `(rainbow-delimiters-depth-1-face ,(fnord--face :foreground 7))
   `(rainbow-delimiters-depth-2-face ,(fnord--face :foreground 8))
   `(rainbow-delimiters-depth-3-face ,(fnord--face :foreground 9))
   `(rainbow-delimiters-depth-4-face ,(fnord--face :foreground 10))
   `(rainbow-delimiters-depth-5-face ,(fnord--face :foreground 12))
   `(rainbow-delimiters-depth-6-face ,(fnord--face :foreground 13))
   `(rainbow-delimiters-depth-7-face ,(fnord--face :foreground 14))
   `(rainbow-delimiters-depth-8-face ,(fnord--face :foreground 15))
   `(rainbow-delimiters-unmatched-face ,(fnord--face :foreground 11))

   ;; > Web Mode
   `(web-mode-attr-tag-custom-face ,(fnord--face :foreground fnord-attribute))
   `(web-mode-builtin-face ,(fnord--face :foreground fnord-keyword))
   `(web-mode-comment-face ,(fnord--face :foreground fnord-comment))
   `(web-mode-comment-keyword-face ,(fnord--face :foreground fnord-comment))
   `(web-mode-constant-face ,(fnord--face :foreground fnord-variable))
   `(web-mode-css-at-rule-face ,(fnord--face :foreground fnord-annotation))
   `(web-mode-css-function-face ,(fnord--face :foreground fnord-method))
   `(web-mode-css-property-name-face ,(fnord--face :foreground fnord-keyword))
   `(web-mode-css-pseudo-class-face ,(fnord--face :foreground fnord-class))
   `(web-mode-css-selector-face ,(fnord--face :foreground fnord-keyword))
   `(web-mode-css-string-face ,(fnord--face :foreground fnord-string))
   `(web-mode-doctype-face ,(fnord--face :foreground fnord-preprocessor))
   `(web-mode-function-call-face ,(fnord--face :foreground fnord-method))
   `(web-mode-function-name-face ,(fnord--face :foreground fnord-method))
   `(web-mode-html-attr-name-face ,(fnord--face :foreground fnord-attribute))
   `(web-mode-html-attr-equal-face ,(fnord--face :foreground fnord-punctuation))
   `(web-mode-html-attr-value-face ,(fnord--face :foreground fnord-string))
   `(web-mode-html-entity-face ,(fnord--face :foreground fnord-keyword))
   `(web-mode-html-tag-bracket-face ,(fnord--face :foreground fnord-punctuation))
   `(web-mode-html-tag-custom-face ,(fnord--face :foreground fnord-tag))
   `(web-mode-html-tag-face ,(fnord--face :foreground fnord-tag))
   `(web-mode-html-tag-namespaced-face ,(fnord--face :foreground fnord-keyword))
   `(web-mode-json-key-face ,(fnord--face :foreground fnord-class))
   `(web-mode-json-string-face ,(fnord--face :foreground fnord-string))
   `(web-mode-keyword-face ,(fnord--face :foreground fnord-keyword))
   `(web-mode-preprocessor-face ,(fnord--face :foreground fnord-preprocessor))
   `(web-mode-string-face ,(fnord--face :foreground fnord-string))
   `(web-mode-symbol-face ,(fnord--face :foreground fnord-variable))
   `(web-mode-type-face ,(fnord--face :foreground fnord-class))
   `(web-mode-warning-face ,(fnord--face :inherit 'font-lock-warning-face))
   `(web-mode-variable-name-face ,(fnord--face :foreground fnord-variable))

   ;; +--- UI ---+
   ;; > Anzu
   `(anzu-mode-line ,(fnord--face :foreground 8))
   `(anzu-mode-line-no-match ,(fnord--face :foreground 11))

   ;; > Avy
   `(avy-lead-face ,(fnord--face :background 11 :foreground 5))
   `(avy-lead-face-0 ,(fnord--face :background 10 :foreground 5))
   `(avy-lead-face-1 ,(fnord--face :background 3 :foreground 5))
   `(avy-lead-face-2 ,(fnord--face :background 15 :foreground 5))

   ;; > Company
   `(company-echo-common ,(fnord--face :foreground 0 :background 4))
   `(company-preview ,(fnord--face :foreground 4 :background 10))
   `(company-preview-common ,(fnord--face :foreground 0 :background 8))
   `(company-preview-search ,(fnord--face :foreground 0 :background 8))
   `(company-scrollbar-bg ,(fnord--face :foreground 1 :background 1))
   `(company-scrollbar-fg ,(fnord--face :foreground 2 :background 2))
   `(company-template-field ,(fnord--face :foreground 0 :background 7))
   `(company-tooltip ,(fnord--face :foreground 4 :background 2))
   `(company-tooltip-annotation ,(fnord--face :foreground 12))
   `(company-tooltip-annotation-selection ,(fnord--face :foreground 12 :weight 'bold))
   `(company-tooltip-common ,(fnord--face :foreground 8))
   `(company-tooltip-common-selection ,(fnord--face :foreground 8 :background 3))
   `(company-tooltip-mouse ,(fnord--face :inherit 'highlight))
   `(company-tooltip-selection ,(fnord--face :background 3 :weight 'bold))

   ;; > diff-hl
   `(diff-hl-change ,(fnord--face :background 13))
   `(diff-hl-insert ,(fnord--face :background 14))
   `(diff-hl-delete ,(fnord--face :background 11))
   
   ;; > Evil
   `(evil-ex-info ,(fnord--face :foreground 8))
   `(evil-ex-substitute-replacement ,(fnord--face :foreground 9))
   `(evil-ex-substitute-matches ,(fnord--face :inherit 'isearch))

   ;; > Flycheck
   `(flycheck-error ,(fnord--face :underline (list :style 'wave :color fnord-11)))
   `(flycheck-fringe-error ,(fnord--face :foreground 11 :weight 'bold))
   `(flycheck-fringe-info ,(fnord--face :foreground 8 :weight 'bold))
   `(flycheck-fringe-warning ,(fnord--face :foreground 13 :weight 'bold))
   `(flycheck-info ,(fnord--face :underline (list :style 'wave :color fnord-8)))
   `(flycheck-warning ,(fnord--face :underline (list :style 'wave :color fnord-13)))

   ;; > Git Gutter
   `(git-gutter:modified ,(fnord--face :foreground 13))
   `(git-gutter:added ,(fnord--face :foreground 14))
   `(git-gutter:deleted ,(fnord--face :foreground 11))

   ;; > Git Gutter Plus
   `(git-gutter+-modified ,(fnord--face :foreground 13))
   `(git-gutter+-added ,(fnord--face :foreground 14))
   `(git-gutter+-deleted ,(fnord--face :foreground 11))

   ;; > Helm
   `(helm-bookmark-addressbook ,(fnord--face :foreground 7))
   `(helm-bookmark-directory ,(fnord--face :foreground 9))
   `(helm-bookmark-file ,(fnord--face :foreground 8))
   `(helm-bookmark-gnus ,(fnord--face :foreground 10))
   `(helm-bookmark-info ,(fnord--face :foreground 14))
   `(helm-bookmark-man ,(fnord--face :foreground 4))
   `(helm-bookmark-w3m ,(fnord--face :foreground 9))
   `(helm-buffer-directory ,(fnord--face :foreground 9))
   `(helm-buffer-file ,(fnord--face :foreground 8))
   `(helm-buffer-not-saved ,(fnord--face :foreground 13))
   `(helm-buffer-process ,(fnord--face :foreground 10))
   `(helm-candidate-number ,(fnord--face :foreground 4 :weight 'bold))
   `(helm-candidate-number-suspended ,(fnord--face :foreground 4))
   `(helm-ff-directory ,(fnord--face :foreground 9 :weight 'bold))
   `(helm-ff-dirs ,(fnord--face :foreground 9))
   `(helm-ff-dotted-director ,(fnord--face :foreground 9 :underline t))
   `(helm-ff-dotted-symlink-director ,(fnord--face :foreground 7 :weight 'bold))
   `(helm-ff-executable ,(fnord--face :foreground 8))
   `(helm-ff-file ,(fnord--face :foreground 4))
   `(helm-ff-invalid-symlink ,(fnord--face :foreground 11 :weight 'bold))
   `(helm-ff-prefix ,(fnord--face :foreground 0 :background 9))
   `(helm-ff-symlink ,(fnord--face :foreground 7))
   `(helm-grep-cmd-line ,(fnord--face :foreground 4 :background 0))
   `(helm-grep-file ,(fnord--face :foreground 8))
   `(helm-grep-finish ,(fnord--face :foreground 5))
   `(helm-grep-lineno ,(fnord--face :foreground 4))
   `(helm-grep-match ,(fnord--face :inherit 'isearch))
   `(helm-grep-running ,(fnord--face :foreground 8))
   `(helm-header ,(fnord--face :foreground 9 :background 2))
   `(helm-header-line-left-margin ,(fnord--face :foreground 9 :background 2))
   `(helm-history-deleted ,(fnord--face :foreground 11))
   `(helm-history-remote ,(fnord--face :foreground 4))
   `(helm-lisp-completion-info ,(fnord--face :foreground 4 :weight 'bold))
   `(helm-lisp-show-completion ,(fnord--face :inherit 'isearch))
   `(helm-locate-finish ,(fnord--face :foreground 14))
   `(helm-match ,(fnord--face :foreground 8))
   `(helm-match-item ,(fnord--face :inherit 'isearch))
   `(helm-moccur-buffer ,(fnord--face :foreground 8))
   `(helm-resume-need-update ,(fnord--face :foreground 0 :background 13))
   `(helm-selection ,(fnord--face :inherit 'highlight))
   `(helm-selection-line ,(fnord--face :background 2))
   `(helm-source-header ,(fnord--face :height 1.44 :foreground 8 :background 2))
   `(helm-swoop-line-number-face ,(fnord--face :foreground 4 :background 0))
   `(helm-swoop-target-word-face ,(fnord--face :foreground 0 :background 7))
   `(helm-swoop-target-line-face ,(fnord--face :background 13 :foreground 3))
   `(helm-swoop-target-line-block-face ,(fnord--face :background 13 :foreground 3))
   `(helm-separator ,(fnord--face :background 2))
   `(helm-visible-mark ,(fnord--face :background 2))

   ;; > Magit
   `(magit-branch ,(fnord--face :foreground 7 :weight 'bold))
   `(magit-diff-context-highlight ,(fnord--face :background 2))
   `(magit-diff-file-header ,(fnord--face :foreground 8 :box '(:color 8)))
   `(magit-diffstat-added ,(fnord--face :foreground 14))
   `(magit-diffstat-removed ,(fnord--face :foreground 11))
   `(magit-hash ,(fnord--face :foreground 8))
   `(magit-hunk-heading ,(fnord--face :foreground 9))
   `(magit-hunk-heading-highlight ,(fnord--face :foreground 9 :background 2))
   `(magit-item-highlight ,(fnord--face :foreground 8 :background 2))
   `(magit-log-author ,(fnord--face :foreground 7))
   `(magit-process-ng ,(fnord--face :foreground 13 :weight 'bold))
   `(magit-process-ok ,(fnord--face :foreground 14 :weight 'bold))
   `(magit-section-heading ,(fnord--face :foreground 7 :weight 'bold))
   `(magit-section-highlight ,(fnord--face :background 2))

   ;; gnus
   `(gnus-header-name ,(fnord--face :foreground 9))
   `(gnus-header ,(fnord--face :foreground 8))
   `(gnus-header-content ,(fnord--face :foreground 8))
   `(gnus-header-from ,(fnord--face :foreground 8))
   `(gnus-header-newsgroups ,(fnord--face :foreground 8))
   `(gnus-header-subject ,(fnord--face :foreground 8))
      
   ;; > MU4E
   `(mu4e-header-marks-face ,(fnord--face :foreground 13))
   
   `(mu4e-highlight-face ,(fnord--face :highlight t))
   `(mu4e-flagged-face ,(fnord--face :foreground 12))
   `(mu4e-forwarded-face ,(fnord--face :foreground 7))
   `(mu4e-unread-face ,(fnord--face :foreground 8 :weight 'bold))
   `(mu4e-read-face ,(fnord--face :foreground 9))
   `(mu4e-replied-face ,(fnord--face :foreground 7))
   `(mu4e-link-face ,(fnord--face :underline t))
   `(mu4e-draft-face ,(fnord--face :foreground 15))
   
   ;; > Powerline
   `(powerline-active1 ,(fnord--face :foreground 4 :background 1))
   `(powerline-active2 ,(fnord--face :foreground 4 :background 3))
   `(powerline-inactive1 ,(fnord--face :background 2))
   `(powerline-inactive2 ,(fnord--face :background 2))

   ;; > Powerline Evil
   `(powerline-evil-base-face ,(fnord--face :foreground 4))
   `(powerline-evil-normal-face ,(fnord--face :background 8))
   `(powerline-evil-insert-face ,(fnord--face :foreground 0 :background 4))
   `(powerline-evil-visual-face ,(fnord--face :foreground 0 :background 7))
   `(powerline-evil-replace-face ,(fnord--face :foreground 0 :background 9))

   ;; > NeoTree
   `(neo-banner-face ,(fnord--face :foreground 10))
   `(neo-dir-link-face ,(fnord--face :foreground 9))
   `(neo-expand-btn-face ,(fnord--face :foreground 6 :bold t))
   `(neo-file-link-face ,(fnord--face :foreground 4))
   `(neo-root-dir-face ,(fnord--face :foreground 7 :weight 'bold))
   `(neo-vc-added-face ,(fnord--face :foreground 14))
   `(neo-vc-conflict-face ,(fnord--face :foreground 11))
   `(neo-vc-default-face ,(fnord--face :foreground 4))
   `(neo-vc-edited-face ,(fnord--face :foreground 13))
   `(neo-vc-ignored-face ,(fnord--face :foreground 3))
   `(neo-vc-missing-face ,(fnord--face :foreground 12))
   `(neo-vc-needs-merge-face ,(fnord--face :background 12 :foreground 4))
   `(neo-vc-needs-update-face ,(fnord--face :background 10 :foreground 4))
   `(neo-vc-removed-face ,(fnord--face :foreground 11 :strike-through nil))
   `(neo-vc-up-to-date-face ,(fnord--face :foreground 4))
   `(neo-vc-user-face ,(fnord--face :foreground 4))

   ;; > Cider
   `(cider-result-overlay-face ((t (:background unspecified))))

   ;; > Org
   `(org-level-1 ,(fnord--face :foreground 7 :weight 'extra-bold))
   `(org-level-2 ,(fnord--face :foreground 8 :weight 'bold))
   `(org-level-3 ,(fnord--face :foreground 9 :weight 'semi-bold))
   `(org-level-4 ,(fnord--face :foreground 7 :weight 'normal))
   `(org-level-5 ,(fnord--face :foreground 8))
   `(org-level-6 ,(fnord--face :foreground 9))
   `(org-level-7 ,(fnord--face :foreground 7))
   `(org-level-8 ,(fnord--face :foreground 8))
   `(org-level-8 ,(fnord--face :foreground 9))
   `(org-agenda-structure ,(fnord--face :foreground 9))
   `(org-agenda-date ,(fnord--face :foreground 8 :underline nil))
   `(org-agenda-done ,(fnord--face :foreground 14))
   `(org-agenda-dimmed-todo-face ,(fnord--face :background 13))
   `(org-block ,(fnord--face :foreground 4))
   `(org-block-background ,(fnord--face :background 0))
   `(org-block-begin-line ,(fnord--face :foreground 7))
   `(org-block-end-line ,(fnord--face :foreground 7))
   `(org-checkbox ,(fnord--face :foreground 9))
   `(org-checkbox-statistics-done ,(fnord--face :foreground 14))
   `(org-checkbox-statistics-todo ,(fnord--face :foreground 13))
   `(org-code ,(fnord--face :foreground 7))
   `(org-column ,(fnord--face :background 2))
   `(org-column-title ,(fnord--face :inherit 'org-column :weight 'bold :underline t))
   `(org-date ,(fnord--face :foreground 8))
   `(org-document-info ,(fnord--face :foreground 4))
   `(org-document-info-keyword ,(fnord--face :foreground 3 :weight 'bold))
   `(org-document-title ,(fnord--face :foreground 8 :weight 'bold))
   `(org-done ,(fnord--face :foreground 14 :weight 'bold))
   `(org-ellipsis ,(fnord--face :foreground 3))
   `(org-footnote ,(fnord--face :foreground 8))
   `(org-formula ,(fnord--face :foreground 9))
   `(org-headline-done ,(fnord--face :foreground 10))   
   `(org-hide ,(fnord--face :foreground 0 :background 0))
   `(org-link ,(fnord--face :underline t))
   `(org-scheduled ,(fnord--face :foreground 14))
   `(org-scheduled-previously ,(fnord--face :foreground 13))
   `(org-scheduled-today ,(fnord--face :foreground 8))
   `(org-special-keyword ,(fnord--face :foreground 9))
   `(org-table ,(fnord--face :foreground 9))
   `(org-todo ,(fnord--face :foreground 13 :weight 'bold))
   `(org-upcoming-deadline ,(fnord--face :foreground 12))
   `(org-warning ,(fnord--face :foreground 13 :weight 'bold))
   `(font-latex-bold-face ,(fnord--face :inherit 'bold))
   `(font-latex-italic-face ,(fnord--face :slant 'italic))
   `(font-latex-string-face ,(fnord--face :foreground 14))
   `(font-latex-match-reference-keywords ,(fnord--face :foreground 9))
   `(font-latex-match-variable-keywords ,(fnord--face :foreground 4))
   `(ido-only-match ,(fnord--face :foreground 8))
   `(org-sexp-date ,(fnord--face :foreground 7))
   `(ido-first-match ,(fnord--face :foreground 8 :weight 'bold))
   `(ido-subdir ,(fnord--face :foreground 9))
   `(org-quote ,(fnord--face :inherit 'org-block :slant 'italic))
   `(org-verse ,(fnord--face :inherit 'org-block :slant 'italic))
   `(org-agenda-date-weekend ,(fnord--face :foreground 9))
   `(org-agenda-date-today ,(fnord--face :foreground 8 :weight 'bold))
   `(org-agenda-done ,(fnord--face :foreground 14))
   `(org-verbatim ,(fnord--face :foreground 7))

   ;; > ivy-mode
   `(ivy-current-match ,(fnord--face :inherit 'region))
   `(ivy-minibuffer-match-face-1 ,(fnord--face :inherit 'default))
   `(ivy-minibuffer-match-face-2 ,(fnord--face :background 7 :foreground 0))
   `(ivy-minibuffer-match-face-3 ,(fnord--face :background 8 :foreground 0))
   `(ivy-minibuffer-match-face-4 ,(fnord--face :background 9 :foreground 0))
   `(ivy-remote ,(fnord--face :foreground 14))
   `(ivy-posframe ,(fnord--face :background 1))
   `(ivy-posframe-border ,(fnord--face :background 1))
   `(ivy-remote ,(fnord--face :foreground 14))

   ;; > perspective
   `(persp-selected-face ,(fnord--face :foreground 8 :weight 'bold))))

(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

  (provide-theme 'fnord)

  ;; Local Variables:
  ;; no-byte-compile: t
  ;; indent-tabs-mode: nil
  ;; End:

;;; fnord-theme.el ends here
