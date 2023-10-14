;;; fnord-core.el --- Core faces for the fnord theme

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

(require 'fnord-colours)
(require 'fnord-custom)

;; TODO customize raised buttons
(setopt custom-raised-buttons t)
(setopt help-clean-buttons t)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)


(setq holiday-other-holidays '((holiday-fixed 10 20 "Bastille Day")))

(defvar fnord--faces
  `(;; add a dummy face  so we can detect whether theme was loaded
    (fnord--loaded-dummy :weight bold)

    (bold :weight bold)
    (bold-italic :weight bold :slant italic)
    (cursor :background 4)
    
    (default :foreground 4 :background 0)
    (error :foreground 11 :weight bold)
;    (next-error :inherit error)
    (escape-glyph :foreground 12)
    (homoglyph :foreground 15)
    (nobreak-hyphen :foreground 15)
    (nobreak-space :foreground 3)
    (italic :slant italic)
    (shadow :foreground 3)
    (underline :underline t)
    (warning :foreground 13 :weight bold)
    (success :foreground 14)
    (textsec-suspicious :background 11 :foreground 6)
    (tooltip :background 3 :foreground 4)
    
    (link :underline t :foreground 8)
    (link-visited :underline t :foreground 10)

    (bookmark-face :foreground 12 :distant-foreground 12)

    ;; edit keyboard macro
    (edmacro-label :foreground 7 :weight normal)
    
    ;; highlighting
    (region :inherit ,fnord-region-face)
    (highlight :foreground 8 :background 3)
    (hl-line :background 1 :distant-foreground 4)
    (hl-todo :foreground 11 :weight bold)
    (secondary-selection :inherit ,fnord-secondary-selection-face)

    ;; hi-lock mode
    (hi-aquamarine :background 7 :distant-foreground 0)
    (hi-blue :background 8 :distant-foreground 0)
    (hi-blue-b :foreground 8 :weight bold)
    (hi-green :background 14 :distant-foreground 0)
    (hi-green-b :foreground 14 :distant-foreground 0)
    (hi-pink :background 11 :foreground 4)
    (hi-red-b :foreground 11 :weight bold)
    (hi-salmon :background 12 :distant-foreground 0)
    (hi-yellow :background 13 :distant-foreground 0)
    
    ;; windows and frames
    (border :foreground 4)
    (fringe :foreground 4 :background 0)
    (header-line :foreground 4 :background 2)

    (calendar-month-header :foreground 7)
    (calendar-weekday-header :foreground 8)
    (calendar-weekend-header :foreground 15)
    (calendar-today :foreground 14 :weight bold)
    (diary :foreground 7)
    (diary-anniversary :foreground 13)
    (holiday :foreground 10 :background 0)
    
    (completions-annotations :foreground 9)
    (completions-common-part :foreground 8 :weight bold)
    (completions-first-difference :foreground 11)
    (completions-group-title :foreground 7)
    (completions-group-title :foreground 7)
    
    (compilation-mode-line-exit :foreground 14)
    (compilation-mode-line-fail :foreground 11)

    (confusingly-reordered :underline (:style wave :color 11))

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
    
    ;; font-lock
    (font-lock-builtin-face :foreground 9)
    (font-lock-comment-face :foreground 16)
    (font-lock-comment-delimiter-face :foreground 16)
    (font-lock-constant-face :foreground 9)
    (font-lock-doc-face :foreground 16)
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
    (font-lock-variable-name-face :foreground 4)
    (font-lock-warning-face :foreground 13)
    
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
    (custom-comment :foreground 16)
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
    (help-for-help-header :foreground 7 :height 'unspecified)
    (help-key-binding :inherit button :background 0)
    (shortdoc-heading :inherit info-menu-header)
    
    ;; info-mode
    (info-title-4 :foreground 7 :weight bold :inherit variable-pitch)
    (info-title-1 :height 1.2 :inherit info-title-2)
    (info-title-2 :height 1.2 :inherit info-title-3)
    (info-title-3 :height 1.2 :inherit info-title-4)
    (info-menu-header :inherit info-title-3 :foreground 6)
    (Info-quoted :foreground 6)
    (info-menu-star :foreground 15)

    ;; search/replace
    (isearch :foreground 0 :background 7)
    (isearch-group-1 :foreground 0 :background 12)
    (isearch-group-2 :foreground 0 :background 13)
    (isearch-fail :foreground 11)
    (match :inherit isearch)
    (query-replace :inherit isearch)
    (lazy-highlight :foreground 4 :background 10)
    

    (linum :foreground 3 :background 0)
    (linum-relative-current-face :foreground 3 :background 0)
    (line-number-current-line :foreground 6)
    
    (minibuffer-prompt :foreground 8 :weight bold)
    (mm-command-output :foreground 8)
    (mode-line :foreground 8 :background 3)
    (mode-line-buffer-id :weight bold)
    (mode-line-highlight :inherit highlight)
    (mode-line-inactive :foreground 4 :background 1)
    ;; TODO customize:
    ;; org block and table backgrounds
    ;; org header heights (zie info headers) - ook gebruiken voor outline
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
    (org-column-title :inherit org-column :weight bold :underline t)
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
    (org-quote :inherit org-block :slant italic)
    (org-verse :inherit org-block :slant italic)
    (org-verbatim :foreground 7)

    
    (outline-1 :foreground 7 :weight bold)
    (outline-2 :foreground 8 :inherit outline-1)
    (outline-3 :foreground 9 :inherit outline-1)
    (outline-4 :foreground 7 :inherit outline-1)
    (outline-5 :foreground 8 :inherit outline-1)
    (outline-6 :foreground 9 :inherit outline-1)
    (outline-7 :foreground 7 :inherit outline-1)
    (outline-8 :foreground 8 :inherit outline-1)
   
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

    (scroll-bar :background 3)
    
    (show-paren-match :foreground 4 :background 10)
    (show-paren-mismatch :foreground 4 :background 11)
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
    
    (tool-bar :foreground 4 :background 3)
    (tooltip :foreground 0 :background 4)
    (trailing-whitespace :foreground 3)
    (tty-menu-disabled-face :foreground 1)
    (tty-menu-enabled-face :background 2 :foreground 4)
    (tty-menu-selected-face :foreground 8 :underline t)

    ;; TODO 
    (undo-tree-visualizer-current-face :foreground 8)
    (undo-tree-visualizer-default-face :foreground 4)
    (undo-tree-visualizer-unmodified-face :foreground 4)
    (undo-tree-visualizer-register-face :foreground 9)

    
    (vc-conflict-state :foreground 12)
    (vc-edited-state :foreground 13)
    (vc-locally-added-state :underline 14)
    (vc-locked-state :foreground 10)
    (vc-missing-state :foreground 11)
    (vc-needs-update-state :foreground 12)
    (vc-removed-state :foreground 11)
    (vc-state-base :foreground 4)
    (vc-up-to-date-state :foreground 8)
    (vertical-border :foreground 2)

    ;; TODO
    (which-func :foreground 8)

    ;; TODO 
    (whitespace-big-indent :foreground 3 :background 0)
    (whitespace-empty :foreground 3 :background 0)
    (whitespace-hspace :foreground 3 :background 0)
    (whitespace-indentation :foreground 3 :background 0)
    (whitespace-line :background 0)
    (whitespace-newline :foreground 3 :background 0)
    (whitespace-space :foreground 3 :background 0)
    (whitespace-space-after-tab :foreground 3 :background 0)
    (whitespace-space-before-tab :foreground 3 :background 0)
    (whitespace-tab :foreground 3 :background 0)
    (whitespace-trailing :inherit trailing-whitespace :background 15)

    (window-divider :background 3)
    (window-divider-first-pixel :background 3)
    (window-divider-last-pixel :background 3)

    (tab-bar :inherit mode-line-inactive)
    (tab-bar-tab :inherit mode-line-highlight)
    (tab-bar-tab-inactive :inherit tab-bar))
  
  "The list of faces defined by the fnord theme.
Foreground and background colours can be ints, in which
case they will be converted to fnord theme colours.")

(provide 'fnord-core)

;;; fnord-core.el ends here
