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


(defface fnord--loaded-dummy
  '((t (:weight normal)))
  "A dummy face we can use to check whether fnord theme was loaded.")


(defun fnord--theme-loaded-p ()
  "Return t when fnord theme has been loaded.
If you loaded another theme after fnord, this will still return t."
  (eq 'bold
      (face-attribute 'fnord--loaded-dummy :weight )))


(setopt custom-raised-buttons t)

(defvar fnord--faces
  `(;; add a dummy face  so we can detect whether theme was loaded
    (fnord--loaded-dummy :weight bold)

    (bold :weight bold)
    (bold-italic :weight bold :slant italic)
    (cursor :background 4)
    
    (default :foreground 4 :background 0)
    (error :foreground 11 :weight bold)
    (escape-glyph :foreground 12)
    (italic :slant italic)
    (shadow :foreground 3)
    (underline :underline t)
    (warning :foreground 13 :weight bold)
    (success :foreground 14)

    (link :underline t :foreground 8)
    (link-visited :underline t)
    
    ;; highlighting
    (region :foreground ,fnord-region-foreground
            :background ,fnord-region-background
            :distant-foreground ,fnord-region-distant-foreground)
    (highlight :foreground 8 :background 3)
    (hl-line :background 3 :distant-foreground 4)
    
    ;; windows and frames
    (border :foreground 4)
    (fringe :foreground 4 :background 0)
    (header-line :foreground 4 :background 2)

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

    (completions-annotations :foreground 9)
    (completions-common-part :foreground 8 :weight bold)
    (completions-first-difference :foreground 11)
    
    (buffer-menu-buffer :foreground 4 :weight bold)

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
    
    (file-name-shadow :inherit shadow)
    (help-argument-name :foreground 8)

    ;; info-mode
    (info-title-4 :foreground 7 :weight bold :inherit variable-pitch)
    (info-title-1 :height 1.2 :inherit info-title-2)
    (info-title-2 :height 1.2 :inherit info-title-3)
    (info-title-3 :height 1.2 :inherit info-title-4)
    (info-menu-header :inherit info-title-3 :foreground 6)
    (Info-quoted :foreground 6)
    (info-menu-star :foreground 15)

    ;; search/replace
    (isearch :foreground 0 :background 8)
    (isearch-fail :foreground 11)
    (match :inherit isearch)
    (query-replace :inherit isearch)
    (lazy-highlight :inherit isearch :foreground 6)
    

    (linum :foreground 3 :background 0)
    (linum-relative-current-face :foreground 3 :background 0)
    (line-number-current-line :foreground 6)
    
    (minibuffer-prompt :foreground 8 :weight bold)
    (mm-command-output :foreground 8)
    (mode-line :foreground 8 :background 3)
    (mode-line-buffer-id :weight bold)
    (mode-line-highlight :inherit highlight)
    (mode-line-inactive :foreground 4 :background 1)
    (next-error :inherit error)
    (nobreak-space :foreground 3)
    
    (org-level-1 :foreground 7 :weight extra-bold)
    (org-level-2 :foreground 8 :weight bold)
    (org-level-3 :foreground 9 :weight semi-bold)
    (org-level-4 :foreground 7 :weight normal)
    (org-level-5 :foreground 8)
    (org-level-6 :foreground 9)
    (org-level-7 :foreground 7)
    (org-level-8 :foreground 8)
    (org-level-8 :foreground 9)
    (org-agenda-structure :foreground 9)
    (org-agenda-date :foreground 8 :underline nil)
    (org-agenda-done :foreground 14)
    (org-agenda-dimmed-todo-face :background 13)
    (org-block :foreground 4)
    (org-block-background :background 0)
    (org-block-begin-line :foreground 7)
    (org-block-end-line :foreground 7)
    (org-checkbox :foreground 9)
    (org-checkbox-statistics-done :foreground 14)
    (org-checkbox-statistics-todo :foreground 13)
    (org-code :foreground 7)
    (org-column :background 2)
    (org-column-title :inherit org-column :weight bold :underline t)
    (org-date :foreground 8)
    (org-document-info :foreground 4)
    (org-document-info-keyword :foreground 3 :weight bold)
    (org-document-title :foreground 8 :weight bold)
    (org-done :foreground 14 :weight bold)
    (org-ellipsis :foreground 3)
    (org-footnote :foreground 8)
    (org-formula :foreground 9)
    (org-headline-done :foreground 10)
    (org-hide :foreground 0 :background 0)
    (org-link :underline t)
    (org-scheduled :foreground 14)
    (org-scheduled-previously :foreground 13)
    (org-scheduled-today :foreground 8)
    (org-special-keyword :foreground 9)
    (org-table :foreground 9)
    (org-todo :foreground 13 :weight bold)
    (org-upcoming-deadline :foreground 12)
    (org-warning :foreground 13 :weight bold)
    (org-sexp-date :foreground 7)
    (org-quote :inherit org-block :slant italic)
    (org-verse :inherit org-block :slant italic)
    (org-agenda-date-weekend :foreground 9)
    (org-agenda-date-today :foreground 8 :weight bold)
    (org-agenda-done :foreground 14)
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
    (package-status-unsigned :underline t)

    (scroll-bar :background 3)
    (secondary-selection :background 2)

    (show-paren-match :foreground 0 :background 8)
    (show-paren-mismatch :background 11)
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
    (undo-tree-visualizer-current-face :foreground 8)
    (undo-tree-visualizer-default-face :foreground 4)
    (undo-tree-visualizer-unmodified-face :foreground 4)
    (undo-tree-visualizer-register-face :foreground 9)
    (vc-conflict-state :foreground 12)
    (vc-edited-state :foreground 13)
    (vc-locally-added-state :underline t)
    (vc-locked-state :foreground 10)
    (vc-missing-state :foreground 11)
    (vc-needs-update-state :foreground 12)
    (vc-removed-state :foreground 11)
    (vc-state-base :foreground 4)
    (vc-up-to-date-state :foreground 8)
    (vertical-border :foreground 2)
    (which-func :foreground 8)
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
