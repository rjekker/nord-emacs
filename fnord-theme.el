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
  (error "fnord-theme requires Emacs 24 or later"))

(deftheme fnord "Nord, fixed: an arctic, north-bluish clean and elegant theme.")

(require 'fnord-colours)
(require 'fnord-core)

(defconst fnord--class '((class color) (min-colors 257)))


(defun fnord--subst-colours (face-spec)
  "If foreground or background in FACE-SPEC are ints, replace with fnord colour.
This now also supports the :color property of :box"
  (dolist (prop '(:foreground :background :distant-foreground :distant-background))
    (when-let ((val (plist-get face-spec prop)))
      (when (integerp val)
        (plist-put face-spec prop (fnord--get-colour val)))))
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
       ;; (fnord-region-highlight-foreground (if (or
       ;;                                         (string= fnord-region-highlight "frost")
       ;;                                         (string= fnord-region-highlight "snowstorm")) "#2E3440" nil))
       ;; (fnord-region-highlight-background (if
       ;;                                        (string= fnord-region-highlight "frost") "#88C0D0"
       ;;                                      (if (string= fnord-region-highlight "snowstorm") "#D8DEE9" "#434C5E")))
       ;; (fnord-uniform-mode-lines-background (if fnord-uniform-mode-lines "#4C566A" "#3B4252"))
       )

  (apply #'custom-theme-set-faces
         'fnord
         (mapcar #'fnord--face fnord--faces))
  )

(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'fnord)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; fnord-theme.el ends here

    ;;             ;; +--- Syntax ---+
    ;;             ;; > C
    ;;             `(c-annotation-face :foreground fnord-annotation)

    ;;             ;; > diff
    ;;             `(diff-added :foreground 14)
    ;;             `(diff-changed :foreground 13)
    ;;             `(diff-context :inherit 'default)
    ;;             `(diff-file-header :foreground 8)
    ;;             `(diff-function :foreground 7)
    ;;             `(diff-header :foreground 9 :weight 'bold)
    ;;             `(diff-hunk-header :foreground 9 :background 0)
    ;;             `(diff-indicator-added :foreground 14)
    ;;             `(diff-indicator-changed :foreground 13)
    ;;             `(diff-indicator-removed :foreground 11)
    ;;             `(diff-nonexistent :foreground 11)
    ;;             `(diff-refine-added :foreground 14)
    ;;             `(diff-refine-changed :foreground 13)
    ;;             `(diff-refine-removed :foreground 11)
    ;;             `(diff-removed :foreground 11)

    ;;             ;; +--- UI ---+
    ;;             `(border :foreground 4)
    ;;             `(buffer-menu-buffer :foreground 4 :weight 'bold)
    ;;             `(button :background 0 :foreground 8 :box (list :line-width 1 :color fnord-8 :style 'sunken-button))
    ;;             `(completions-annotations :foreground 9)
    ;;             `(completions-common-part :foreground 8 :weight 'bold)
    ;;             `(completions-first-difference :foreground 11)
    ;;             `(custom-button :background 0 :foreground 8 :box (list :line-width 1 :color fnord-8 :style 'sunken-button))
    ;;             `(custom-button-mouse :background 4 :foreground 0 :box (list :line-width 1 :color fnord-8 :style 'sunken-button))
    ;;             `(custom-button-pressed :background 6 :foreground 0 :box (list :line-width 1 :color fnord-8 :style 'sunken-button))
    ;;             `(custom-button-pressed-unraised ,(fnord--face :background 4 :foreground 0 :box
    ;;                                                            (list :line-width 1 :color fnord-8 :style 'sunken-button)))
    ;;             `(custom-button-unraised ,(fnord--face :background 0 :foreground 8 :box
    ;;                                                    (list :line-width 1 :color fnord-8 :style 'sunken-button)))
    ;;             `(custom-changed :foreground 13)
    ;;             `(custom-comment :foreground fnord-comment)
    ;;             `(custom-comment-tag :foreground 7)
    ;;             `(custom-documentation :foreground 4)
    ;;             `(custom-group-tag :foreground 8 :weight 'bold)
    ;;             `(custom-group-tag-1 :foreground 8 :weight 'bold)
    ;;             `(custom-invalid :foreground 11)
    ;;             `(custom-modified :foreground 13)
    ;;             `(custom-rogue :foreground 12 :background 2)
    ;;             `(custom-saved :foreground 14)
    ;;             `(custom-set :foreground 8)
    ;;             `(custom-state :foreground 14)
    ;;             `(custom-themed :foreground 8 :background 2)
    ;;             `(cursor :background 4)
    ;;             `(fringe :foreground 4 :background 0)
    ;;             `(file-name-shadow :inherit 'shadow)
    ;;             `(header-line :foreground 4 :background 2)
    ;;             `(help-argument-name :foreground 8)
    ;;             `(highlight :foreground 8 :background 2)
    ;;             `(hl-line :background 1)
    ;;             `(info-menu-star :foreground 9)
    ;;             `(isearch :foreground 0 :background 8)
    ;;             `(isearch-fail :foreground 11)
    ;;             `(link :underline t)
    ;;             `(link-visited :underline t)
    ;;             `(linum :foreground 3 :background 0)
    ;;             `(linum-relative-current-face :foreground 3 :background 0)
    ;;             `(line-number-current-line :foreground 6)
    ;;             `(match :inherit 'isearch)
    ;;             `(message-cited-text :foreground 4)
    ;;             `(message-header-cc :foreground 9)
    ;;             `(message-header-name :foreground 7)
    ;;             `(message-header-newsgroup :foreground 14)
    ;;             `(message-header-other :foreground 4)
    ;;             `(message-header-subject :foreground 8)
    ;;             `(message-header-to :foreground 9)
    ;;             `(message-header-xheader :foreground 13)
    ;;             `(message-mml :foreground 10)
    ;;             `(message-separator :inherit 'shadow)
    ;;             `(minibuffer-prompt :foreground 8 :weight 'bold)
    ;;             `(mm-command-output :foreground 8)
    ;;             `(mode-line :foreground 8 :background 3)
    ;;             `(mode-line-buffer-id :weight 'bold)
    ;;             `(mode-line-highlight :inherit 'highlight)
    ;;             `(mode-line-inactive :foreground 4 :background fnord-uniform-mode-lines-background)
    ;;             `(next-error :inherit 'error)
    ;;             `(nobreak-space :foreground 3)
    ;;             `(outline-1 :foreground 8 :weight 'bold)
    ;;             `(outline-2 :inherit 'outline-1)
    ;;             `(outline-3 :inherit 'outline-1)
    ;;             `(outline-4 :inherit 'outline-1)
    ;;             `(outline-5 :inherit 'outline-1)
    ;;             `(outline-6 :inherit 'outline-1)
    ;;             `(outline-7 :inherit 'outline-1)
    ;;             `(outline-8 :inherit 'outline-1)
    ;;             `(package-description :foreground 4)
    ;;             `(package-help-section-name :foreground 8 :weight 'bold)
    ;;             `(package-name :foreground 8)
    ;;             `(package-status-available :foreground 7)
    ;;             `(package-status-avail-obso :foreground 7 :slant 'italic)
    ;;             `(package-status-built-in :foreground 9)
    ;;             `(package-status-dependency :foreground 8 :slant 'italic)
    ;;             `(package-status-disabled :foreground 3)
    ;;             `(package-status-external :foreground 12 :slant 'italic)
    ;;             `(package-status-held :foreground 4 :weight 'bold)
    ;;             `(package-status-new :foreground 14)
    ;;             `(package-status-incompat :foreground 11)
    ;;             `(package-status-installed :foreground 7 :weight 'bold)
    ;;             `(package-status-unsigned :underline t)
    ;;             `(query-replace :foreground 8 :background 2)
    ;;             `(scroll-bar :background 3)
    ;;             `(secondary-selection :background 2)

    ;;             ;; `show-paren-match-face` and `show-paren-mismatch-face` are deprecated since Emacs version 22.1 and were
    ;;             ;; removed in Emacs 25.
    ;;             ;; https://github.com/arcticicestudio/fnord-emacs/issues/75
    ;;             ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=c430f7e23fc2c22f251ace4254e37dea1452dfc3
    ;;             ;; https://github.com/emacs-mirror/emacs/commit/c430f7e23fc2c22f251ace4254e37dea1452dfc3
    ;;             `(show-paren-match-face :foreground 0 :background 8)
    ;;             `(show-paren-mismatch-face :background 11)

    ;;             `(show-paren-match :foreground 0 :background 8)
    ;;             `(show-paren-mismatch :background 11)
    ;;             `(success :foreground 14)
    ;;             `(term :foreground 4 :background 0)
    ;;             `(term-color-black :foreground 1 :background 1)
    ;;             `(term-color-white :foreground 5 :background 5)
    ;;             `(term-color-cyan :foreground 7 :background 7)
    ;;             `(term-color-blue :foreground 8 :background 8)
    ;;             `(term-color-red :foreground 11 :background 11)
    ;;             `(term-color-yellow :foreground 13 :background 13)
    ;;             `(term-color-green :foreground 14 :background 14)
    ;;             `(term-color-magenta :foreground 15 :background 15)
    ;;             `(ansi-color-black :foreground 1 :background 1)
    ;;             `(ansi-color-white :foreground 5 :background 5)
    ;;             `(ansi-color-cyan :foreground 7 :background 7)
    ;;             `(ansi-color-blue :foreground 8 :background 8)
    ;;             `(ansi-color-red :foreground 11 :background 11)
    ;;             `(ansi-color-yellow :foreground 13 :background 13)
    ;;             `(ansi-color-green :foreground 14 :background 14)
    ;;             `(ansi-color-magenta :foreground 15 :background 15)
                
    ;;             `(tool-bar :foreground 4 :background 3)
    ;;             `(tooltip :foreground 0 :background 4)
    ;;             `(trailing-whitespace :foreground 3)
    ;;             `(tty-menu-disabled-face :foreground 1)
    ;;             `(tty-menu-enabled-face :background 2 :foreground 4)
    ;;             `(tty-menu-selected-face :foreground 8 :underline t)
    ;;             `(undo-tree-visualizer-current-face :foreground 8)
    ;;             `(undo-tree-visualizer-default-face :foreground 4)
    ;;             `(undo-tree-visualizer-unmodified-face :foreground 4)
    ;;             `(undo-tree-visualizer-register-face :foreground 9)
    ;;             `(vc-conflict-state :foreground 12)
    ;;             `(vc-edited-state :foreground 13)
    ;;             `(vc-locally-added-state :underline t)
    ;;             `(vc-locked-state :foreground 10)
    ;;             `(vc-missing-state :foreground 11)
    ;;             `(vc-needs-update-state :foreground 12)
    ;;             `(vc-removed-state :foreground 11)
    ;;             `(vc-state-base :foreground 4)
    ;;             `(vc-up-to-date-state :foreground 8)
    ;;             `(vertical-border :foreground 2)
    ;;             `(which-func :foreground 8)
    ;;             `(whitespace-big-indent :foreground 3 :background 0)
    ;;             `(whitespace-empty :foreground 3 :background 0)
    ;;             `(whitespace-hspace :foreground 3 :background 0)
    ;;             `(whitespace-indentation :foreground 3 :background 0)
    ;;             `(whitespace-line :background 0)
    ;;             `(whitespace-newline :foreground 3 :background 0)
    ;;             `(whitespace-space :foreground 3 :background 0)
    ;;             `(whitespace-space-after-tab :foreground 3 :background 0)
    ;;             `(whitespace-space-before-tab :foreground 3 :background 0)
    ;;             `(whitespace-tab :foreground 3 :background 0)
    ;;             `(whitespace-trailing :inherit 'trailing-whitespace :background 15)  
    ;;             `(widget-button-pressed :foreground 9 :background 1)
    ;;             `(widget-documentation :foreground 4)
    ;;             `(widget-field :background 2 :foreground 4)
    ;;             `(widget-single-line-field :background 2 :foreground 4)
    ;;             `(window-divider :background 3)
    ;;             `(window-divider-first-pixel :background 3)
    ;;             `(window-divider-last-pixel :background 3)

    ;;             `(tab-bar ((t (:inherit mode-line-inactive))))
    ;;             `(tab-bar-tab ((t (:inherit mode-line-highlight))))
    ;;             `(tab-bar-tab-inactive ((t (:inherit tab-bar)))) '(tab-line ((t nil)))

    ;; ;;;; +-----------------+
    ;; ;;;; + Package Support +
    ;; ;;;; +-----------------+
    ;;             ;; +--- Syntax ---+
    ;;             ;; > Auctex
    ;;             `(font-latex-bold-face :inherit 'bold)
    ;;             `(font-latex-italic-face :inherit 'italic)
    ;;             `(font-latex-math-face :foreground 8)
    ;;             `(font-latex-sectioning-0-face :foreground 8 :weight 'bold)
    ;;             `(font-latex-sectioning-1-face :inherit 'font-latex-sectioning-0-face)
    ;;             `(font-latex-sectioning-2-face :inherit 'font-latex-sectioning-0-face)
    ;;             `(font-latex-sectioning-3-face :inherit 'font-latex-sectioning-0-face)
    ;;             `(font-latex-sectioning-4-face :inherit 'font-latex-sectioning-0-face)
    ;;             `(font-latex-sectioning-5-face :inherit 'font-latex-sectioning-0-face)
    ;;             `(font-latex-script-char-face :inherit 'font-lock-warning-face)
    ;;             `(font-latex-string-face :inherit 'font-lock-string-face)
    ;;             `(font-latex-warning-face :inherit 'font-lock-warning-face)

    ;;             ;; > Elixir
    ;;             `(elixir-attribute-face :foreground fnord-annotation)
    ;;             `(elixir-atom-face :foreground 4 :weight 'bold)

    ;;             ;; > Enhanced Ruby
    ;;             `(enh-ruby-heredoc-delimiter-face :foreground 14)
    ;;             `(enh-ruby-op-face :foreground 9)
    ;;             `(enh-ruby-regexp-delimiter-face :foreground 13)
    ;;             `(enh-ruby-regexp-face :foreground 13)
    ;;             `(enh-ruby-string-delimiter-face :foreground 14)
    ;;             `(erm-syn-errline :foreground 11 :underline t)
    ;;             `(erm-syn-warnline :foreground 13 :underline t)

    ;;             ;; > Java Development Environment for Emacs
    ;;             `(jdee-db-active-breakpoint-face :background 2 :weight 'bold)
    ;;             `(jdee-bug-breakpoint-cursor :background 2)
    ;;             `(jdee-db-requested-breakpoint-face :foreground 13 :background 2 :weight 'bold)
    ;;             `(jdee-db-spec-breakpoint-face :foreground 14 :background 2 :weight 'bold)
    ;;             `(jdee-font-lock-api-face :foreground 4)
    ;;             `(jdee-font-lock-code-face :slant 'italic)
    ;;             `(jdee-font-lock-constant-face :foreground fnord-keyword)
    ;;             `(jdee-font-lock-constructor-face :foreground fnord-method)
    ;;             `(jdee-font-lock-doc-tag-face :foreground 7)
    ;;             `(jdee-font-lock-link-face :underline t)
    ;;             `(jdee-font-lock-modifier-face :foreground fnord-keyword)
    ;;             `(jdee-font-lock-number-face :foreground fnord-numeric)
    ;;             `(jdee-font-lock-operator-fac :foreground fnord-operator)
    ;;             `(jdee-font-lock-package-face :foreground fnord-class)
    ;;             `(jdee-font-lock-pre-face :foreground fnord-comment :slant 'italic)
    ;;             `(jdee-font-lock-private-face :foreground fnord-keyword)
    ;;             `(jdee-font-lock-public-face :foreground fnord-keyword)
    ;;             `(jdee-font-lock-variable-face :foreground fnord-variable)

    ;;             ;; > JavaScript 2
    ;;             `(js2-function-call :foreground 8)
    ;;             `(js2-private-function-call :foreground 8)
    ;;             `(js2-jsdoc-html-tag-delimiter :foreground 6)
    ;;             `(js2-jsdoc-html-tag-name :foreground 9)
    ;;             `(js2-external-variable :foreground 4)
    ;;             `(js2-function-param :foreground 4)
    ;;             `(js2-jsdoc-value :foreground fnord-comment)
    ;;             `(js2-jsdoc-tag :foreground 7)
    ;;             `(js2-jsdoc-type :foreground 7)
    ;;             `(js2-private-member :foreground 4)
    ;;             `(js2-object-property :foreground 4)
    ;;             `(js2-error :foreground 11)
    ;;             `(js2-warning :foreground 13)
    ;;             `(js2-instance-member :foreground 4)

    ;;             ;; > JavaScript 3
    ;;             `(js3-error-face :foreground 11)
    ;;             `(js3-external-variable-face :foreground 4)
    ;;             `(js3-function-param-face :foreground 4)
    ;;             `(js3-instance-member-face :foreground 4)
    ;;             `(js3-jsdoc-html-tag-delimiter-face :foreground 6)
    ;;             `(js3-jsdoc-html-tag-name-face :foreground 9)
    ;;             `(js3-jsdoc-tag-face :foreground 9)
    ;;             `(js3-jsdoc-type-face :foreground 7)
    ;;             `(js3-jsdoc-value-face :foreground 4)
    ;;             `(js3-magic-paren-face :inherit 'show-paren-match-face)
    ;;             `(js3-private-function-call-face :foreground 8)
    ;;             `(js3-private-member-face :foreground 4)
    ;;             `(js3-warning-face :foreground 13)

    ;;             ;; > Markdown
    ;;             `(markdown-blockquote-face :foreground fnord-comment)
    ;;             `(markdown-bold-face :inherit 'bold)
    ;;             `(markdown-header-face-1 :foreground 8)
    ;;             `(markdown-header-face-2 :foreground 8)
    ;;             `(markdown-header-face-3 :foreground 8)
    ;;             `(markdown-header-face-4 :foreground 8)
    ;;             `(markdown-header-face-5 :foreground 8)
    ;;             `(markdown-header-face-6 :foreground 8)
    ;;             `(markdown-inline-code-face :foreground 7)
    ;;             `(markdown-italic-face :inherit 'italic)
    ;;             `(markdown-link-face :foreground 8)
    ;;             `(markdown-markup-face :foreground 9)
    ;;             `(markdown-reference-face :inherit 'markdown-link-face)
    ;;             `(markdown-url-face :foreground 4 :underline t)

    ;;             ;; > Rainbow Delimeters
    ;;             `(rainbow-delimiters-depth-1-face :foreground 7)
    ;;             `(rainbow-delimiters-depth-2-face :foreground 8)
    ;;             `(rainbow-delimiters-depth-3-face :foreground 9)
    ;;             `(rainbow-delimiters-depth-4-face :foreground 10)
    ;;             `(rainbow-delimiters-depth-5-face :foreground 12)
    ;;             `(rainbow-delimiters-depth-6-face :foreground 13)
    ;;             `(rainbow-delimiters-depth-7-face :foreground 14)
    ;;             `(rainbow-delimiters-depth-8-face :foreground 15)
    ;;             `(rainbow-delimiters-unmatched-face :foreground 11)

    ;;             ;; > Web Mode
    ;;             `(web-mode-attr-tag-custom-face :foreground fnord-attribute)
    ;;             `(web-mode-builtin-face :foreground fnord-keyword)
    ;;             `(web-mode-comment-face :foreground fnord-comment)
    ;;             `(web-mode-comment-keyword-face :foreground fnord-comment)
    ;;             `(web-mode-constant-face :foreground fnord-variable)
    ;;             `(web-mode-css-at-rule-face :foreground fnord-annotation)
    ;;             `(web-mode-css-function-face :foreground fnord-method)
    ;;             `(web-mode-css-property-name-face :foreground fnord-keyword)
    ;;             `(web-mode-css-pseudo-class-face :foreground fnord-class)
    ;;             `(web-mode-css-selector-face :foreground fnord-keyword)
    ;;             `(web-mode-css-string-face :foreground fnord-string)
    ;;             `(web-mode-doctype-face :foreground fnord-preprocessor)
    ;;             `(web-mode-function-call-face :foreground fnord-method)
    ;;             `(web-mode-function-name-face :foreground fnord-method)
    ;;             `(web-mode-html-attr-name-face :foreground fnord-attribute)
    ;;             `(web-mode-html-attr-equal-face :foreground fnord-punctuation)
    ;;             `(web-mode-html-attr-value-face :foreground fnord-string)
    ;;             `(web-mode-html-entity-face :foreground fnord-keyword)
    ;;             `(web-mode-html-tag-bracket-face :foreground fnord-punctuation)
    ;;             `(web-mode-html-tag-custom-face :foreground fnord-tag)
    ;;             `(web-mode-html-tag-face :foreground fnord-tag)
    ;;             `(web-mode-html-tag-namespaced-face :foreground fnord-keyword)
    ;;             `(web-mode-json-key-face :foreground fnord-class)
    ;;             `(web-mode-json-string-face :foreground fnord-string)
    ;;             `(web-mode-keyword-face :foreground fnord-keyword)
    ;;             `(web-mode-preprocessor-face :foreground fnord-preprocessor)
    ;;             `(web-mode-string-face :foreground fnord-string)
    ;;             `(web-mode-symbol-face :foreground fnord-variable)
    ;;             `(web-mode-type-face :foreground fnord-class)
    ;;             `(web-mode-warning-face :inherit 'font-lock-warning-face)
    ;;             `(web-mode-variable-name-face :foreground fnord-variable)

    ;;             ;; +--- UI ---+
    ;;             ;; > Anzu
    ;;             `(anzu-mode-line :foreground 8)
    ;;             `(anzu-mode-line-no-match :foreground 11)

    ;;             ;; > Avy
    ;;             `(avy-lead-face :background 11 :foreground 5)
    ;;             `(avy-lead-face-0 :background 10 :foreground 5)
    ;;             `(avy-lead-face-1 :background 3 :foreground 5)
    ;;             `(avy-lead-face-2 :background 15 :foreground 5)

    ;;             ;; > Company
    ;;             `(company-echo-common :foreground 0 :background 4)
    ;;             `(company-preview :foreground 4 :background 10)
    ;;             `(company-preview-common :foreground 0 :background 8)
    ;;             `(company-preview-search :foreground 0 :background 8)
    ;;             `(company-scrollbar-bg :foreground 1 :background 1)
    ;;             `(company-scrollbar-fg :foreground 2 :background 2)
    ;;             `(company-template-field :foreground 0 :background 7)
    ;;             `(company-tooltip :foreground 4 :background 2)
    ;;             `(company-tooltip-annotation :foreground 12)
    ;;             `(company-tooltip-annotation-selection :foreground 12 :weight 'bold)
    ;;             `(company-tooltip-common :foreground 8)
    ;;             `(company-tooltip-common-selection :foreground 8 :background 3)
    ;;             `(company-tooltip-mouse :inherit 'highlight)
    ;;             `(company-tooltip-selection :background 3 :weight 'bold)

    ;;             ;; > diff-hl
    ;;             `(diff-hl-change :background 13)
    ;;             `(diff-hl-insert :background 14)
    ;;             `(diff-hl-delete :background 11)
                
    ;;             ;; > Evil
    ;;             `(evil-ex-info :foreground 8)
    ;;             `(evil-ex-substitute-replacement :foreground 9)
    ;;             `(evil-ex-substitute-matches :inherit 'isearch)

    ;;             ;; > Flycheck
    ;;             `(flycheck-error :underline (list :style 'wave :color fnord-11))
    ;;             `(flycheck-fringe-error :foreground 11 :weight 'bold)
    ;;             `(flycheck-fringe-info :foreground 8 :weight 'bold)
    ;;             `(flycheck-fringe-warning :foreground 13 :weight 'bold)
    ;;             `(flycheck-info :underline (list :style 'wave :color fnord-8))
    ;;             `(flycheck-warning :underline (list :style 'wave :color fnord-13))

    ;;             ;; > Git Gutter
    ;;             `(git-gutter:modified :foreground 13)
    ;;             `(git-gutter:added :foreground 14)
    ;;             `(git-gutter:deleted :foreground 11)

    ;;             ;; > Git Gutter Plus
    ;;             `(git-gutter+-modified :foreground 13)
    ;;             `(git-gutter+-added :foreground 14)
    ;;             `(git-gutter+-deleted :foreground 11)

    ;;             ;; > Helm
    ;;             `(helm-bookmark-addressbook :foreground 7)
    ;;             `(helm-bookmark-directory :foreground 9)
    ;;             `(helm-bookmark-file :foreground 8)
    ;;             `(helm-bookmark-gnus :foreground 10)
    ;;             `(helm-bookmark-info :foreground 14)
    ;;             `(helm-bookmark-man :foreground 4)
    ;;             `(helm-bookmark-w3m :foreground 9)
    ;;             `(helm-buffer-directory :foreground 9)
    ;;             `(helm-buffer-file :foreground 8)
    ;;             `(helm-buffer-not-saved :foreground 13)
    ;;             `(helm-buffer-process :foreground 10)
    ;;             `(helm-candidate-number :foreground 4 :weight 'bold)
    ;;             `(helm-candidate-number-suspended :foreground 4)
    ;;             `(helm-ff-directory :foreground 9 :weight 'bold)
    ;;             `(helm-ff-dirs :foreground 9)
    ;;             `(helm-ff-dotted-director :foreground 9 :underline t)
    ;;             `(helm-ff-dotted-symlink-director :foreground 7 :weight 'bold)
    ;;             `(helm-ff-executable :foreground 8)
    ;;             `(helm-ff-file :foreground 4)
    ;;             `(helm-ff-invalid-symlink :foreground 11 :weight 'bold)
    ;;             `(helm-ff-prefix :foreground 0 :background 9)
    ;;             `(helm-ff-symlink :foreground 7)
    ;;             `(helm-grep-cmd-line :foreground 4 :background 0)
    ;;             `(helm-grep-file :foreground 8)
    ;;             `(helm-grep-finish :foreground 5)
    ;;             `(helm-grep-lineno :foreground 4)
    ;;             `(helm-grep-match :inherit 'isearch)
    ;;             `(helm-grep-running :foreground 8)
    ;;             `(helm-header :foreground 9 :background 2)
    ;;             `(helm-header-line-left-margin :foreground 9 :background 2)
    ;;             `(helm-history-deleted :foreground 11)
    ;;             `(helm-history-remote :foreground 4)
    ;;             `(helm-lisp-completion-info :foreground 4 :weight 'bold)
    ;;             `(helm-lisp-show-completion :inherit 'isearch)
    ;;             `(helm-locate-finish :foreground 14)
    ;;             `(helm-match :foreground 8)
    ;;             `(helm-match-item :inherit 'isearch)
    ;;             `(helm-moccur-buffer :foreground 8)
    ;;             `(helm-resume-need-update :foreground 0 :background 13)
    ;;             `(helm-selection :inherit 'highlight)
    ;;             `(helm-selection-line :background 2)
    ;;             `(helm-source-header :height 1.44 :foreground 8 :background 2)
    ;;             `(helm-swoop-line-number-face :foreground 4 :background 0)
    ;;             `(helm-swoop-target-word-face :foreground 0 :background 7)
    ;;             `(helm-swoop-target-line-face :background 13 :foreground 3)
    ;;             `(helm-swoop-target-line-block-face :background 13 :foreground 3)
    ;;             `(helm-separator :background 2)
    ;;             `(helm-visible-mark :background 2)

    ;;             ;; > Magit
    ;;             `(magit-branch :foreground 7 :weight 'bold)
    ;;             `(magit-diff-context-highlight :background 2)
    ;;             `(magit-diff-file-header :foreground 8 :box '(:color 8))
    ;;             `(magit-diffstat-added :foreground 14)
    ;;             `(magit-diffstat-removed :foreground 11)
    ;;             `(magit-hash :foreground 8)
    ;;             `(magit-hunk-heading :foreground 9)
    ;;             `(magit-hunk-heading-highlight :foreground 9 :background 2)
    ;;             `(magit-item-highlight :foreground 8 :background 2)
    ;;             `(magit-log-author :foreground 7)
    ;;             `(magit-process-ng :foreground 13 :weight 'bold)
    ;;             `(magit-process-ok :foreground 14 :weight 'bold)
    ;;             `(magit-section-heading :foreground 7 :weight 'bold)
    ;;             `(magit-section-highlight :background 2)
    ;; (message-cited-text :foreground 4)
    ;; (message-header-cc :foreground 9)
    ;; (message-header-name :foreground 7)
    ;; (message-header-newsgroup :foreground 14)
    ;; (message-header-other :foreground 4)
    ;; (message-header-subject :foreground 8)
    ;; (message-header-to :foreground 9)
    ;; (message-header-xheader :foreground 13)
    ;; (message-mml :foreground 10)
    ;; (message-separator :inherit shadow)

    ;;             ;; gnus
    ;;             `(gnus-header-name :foreground 9)
    ;;             `(gnus-header :foreground 8)
    ;;             `(gnus-header-content :foreground 8)
    ;;             `(gnus-header-from :foreground 8)
    ;;             `(gnus-header-newsgroups :foreground 8)
    ;;             `(gnus-header-subject :foreground 8)
                
    ;;             ;; > MU4E
    ;;             `(mu4e-header-marks-face :foreground 13)
                
    ;;             `(mu4e-highlight-face :highlight t)
    ;;             `(mu4e-flagged-face :foreground 12)
    ;;             `(mu4e-forwarded-face :foreground 7)
    ;;             `(mu4e-unread-face :foreground 8 :weight 'bold)
    ;;             `(mu4e-read-face :foreground 9)
    ;;             `(mu4e-replied-face :foreground 7)
    ;;             `(mu4e-link-face :underline t)
    ;;             `(mu4e-draft-face :foreground 15)
                
    ;;             ;; > Powerline
    ;;             `(powerline-active1 :foreground 4 :background 1)
    ;;             `(powerline-active2 :foreground 4 :background 3)
    ;;             `(powerline-inactive1 :background 2)
    ;;             `(powerline-inactive2 :background 2)

    ;;             ;; > Powerline Evil
    ;;             `(powerline-evil-base-face :foreground 4)
    ;;             `(powerline-evil-normal-face :background 8)
    ;;             `(powerline-evil-insert-face :foreground 0 :background 4)
    ;;             `(powerline-evil-visual-face :foreground 0 :background 7)
    ;;             `(powerline-evil-replace-face :foreground 0 :background 9)

    ;;             ;; > NeoTree
    ;;             `(neo-banner-face :foreground 10)
    ;;             `(neo-dir-link-face :foreground 9)
    ;;             `(neo-expand-btn-face :foreground 6 :bold t)
    ;;             `(neo-file-link-face :foreground 4)
    ;;             `(neo-root-dir-face :foreground 7 :weight 'bold)
    ;;             `(neo-vc-added-face :foreground 14)
    ;;             `(neo-vc-conflict-face :foreground 11)
    ;;             `(neo-vc-default-face :foreground 4)
    ;;             `(neo-vc-edited-face :foreground 13)
    ;;             `(neo-vc-ignored-face :foreground 3)
    ;;             `(neo-vc-missing-face :foreground 12)
    ;;             `(neo-vc-needs-merge-face :background 12 :foreground 4)
    ;;             `(neo-vc-needs-update-face :background 10 :foreground 4)
    ;;             `(neo-vc-removed-face :foreground 11 :strike-through nil)
    ;;             `(neo-vc-up-to-date-face :foreground 4)
    ;;             `(neo-vc-user-face :foreground 4)

    ;;             ;; > Cider
    ;;             `(cider-result-overlay-face ((t (:background unspecified))))

    ;; (font-latex-bold-face :inherit 'bold)
    ;; (font-latex-italic-face :slant 'italic)
    ;; (font-latex-string-face :foreground 14)
    ;; (font-latex-match-reference-keywords :foreground 9)
    ;; (font-latex-match-variable-keywords :foreground 4)
    ;; (ido-first-match :foreground 8 :weight 'bold)
    ;; (ido-subdir :foreground 9)    
    ;; (ido-only-match :foreground 8)
    ;;             ;; > ivy-mode
    ;;             `(ivy-current-match :inherit 'region)
    ;;             `(ivy-minibuffer-match-face-1 :inherit 'default)
    ;;             `(ivy-minibuffer-match-face-2 :background 7 :foreground 0)
    ;;             `(ivy-minibuffer-match-face-3 :background 8 :foreground 0)
    ;;             `(ivy-minibuffer-match-face-4 :background 9 :foreground 0)
    ;;             `(ivy-remote :foreground 14)
    ;;             `(ivy-posframe :background 1)
    ;;             `(ivy-posframe-border :background 1)
    ;;             `(ivy-remote :foreground 14)

    ;;             ;; > perspective
    ;;             `(persp-selected-face :foreground 8 :weight 'bold)
