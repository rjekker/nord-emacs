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

(defcustom fnord-region-foreground 'unspecified
  "Foreground color for the region.
If set as an int: use a fnord theme colour.
Alternatively, use a string to choose your own colour code. Note that the string
has to be a valid colour code like \"#A01A02\""
  :type '(choice (integer :tag "fnord theme colour" :default 0)
                 (string :tag "colour code")
                 (symbol :tag "unspecified" :value unspecified))
  :group 'fnord-theme
  :set (lambda (symbol value)
         (fnord--update-custom-face-colour symbol value 'region :foreground)))


(defcustom fnord-region-distant-foreground 4
  "Distant-foreground color for the region.
Used when contrast between foreground and background is low.
If set as an int: use a fnord theme colour.
Alternatively, use a string to choose your own colour code. Note that the string
has to be a valid colour code like \"#A01A02\""
  :type '(choice (integer :tag "fnord theme colour" :default 0)
                 (string :tag "colour code")
                 (symbol :tag "unspecified" :value unspecified))
  :group 'fnord-theme
  :set (lambda (symbol value)
         (fnord--update-custom-face-colour symbol value 'region :distant-foreground)))


(defcustom fnord-region-background 2
  "Background color for the region.
If set as an int: use a fnord theme colour.
Alternatively, use a string to choose your own colour code. Note that the string
has to be a valid colour code like \"#A01A02\""
  :type '(choice (integer :tag "fnord theme colour" :default 0)
                 (string :tag "colour code")
                 (symbol :tag "unspecified" :value unspecified))
  :group 'fnord-theme
  :set (lambda (symbol value)
         (fnord--update-custom-face-colour symbol value 'region :background)))



(provide 'fnord-custom)

;;; fnord-custom.el ends here
