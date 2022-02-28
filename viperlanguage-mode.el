;;; viperlanguage-mode.el --- Syntax highlighting for Viper -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos <dennisspiliopoylos@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/viperlanguage-mode
;; Package-Requires: ((emacs "26.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines syntax highlighting for the
;; Viper language.

;;; Code:

;;(setq my-highlights
;;      '(("method\\|requires\\|ensures" . font-lock-function-name-face)
;;        ("Pi\\|Infinity" . font-lock-constant-face)))
;;
;;(define-derived-mode viperlanguage-mode fundamental-mode
;;  (setq font-lock-defaults '(my-highlights))
;;  (setq mode-name "viper language"))


;; define several category of keywords
(setq viperlanguage-keywords '("method" "while" "var" "import") )
(setq viperlanguage-types '("Ref" "Bool" "Int" "Rational" "Perm" "Seq" "Set" "Multiset"))
(setq viperlanguage-constants '())
(setq viperlanguage-events '("invariant" "requires" "ensures"))
(setq viperlanguage-functions '())

;; generate regex string for each category of keywords
(setq viperlanguage-keywords-regexp (regexp-opt viperlanguage-keywords 'words))
(setq viperlanguage-type-regexp (regexp-opt viperlanguage-types 'words))
(setq viperlanguage-constant-regexp (regexp-opt viperlanguage-constants 'words))
(setq viperlanguage-event-regexp (regexp-opt viperlanguage-events 'words))
(setq viperlanguage-functions-regexp (regexp-opt viperlanguage-functions 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq viperlanguage-font-lock-keywords
      `(
        (,viperlanguage-type-regexp . font-lock-type-face)
        (,viperlanguage-constant-regexp . font-lock-constant-face)
        (,viperlanguage-event-regexp . font-lock-builtin-face)
        (,viperlanguage-functions-regexp . font-lock-function-name-face)
        (,viperlanguage-keywords-regexp . font-lock-keyword-face)
        ;; note: order above matters, because once colored, that part won't change.
        ;; in general, longer words first
        ))

;;;###autoload
(define-derived-mode viperlanguage-mode fundamental-mode
  "viperlanguage mode"
  "Major mode for editing Viper"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((viperlanguage-font-lock-keywords))))

;; clear memory. no longer needed
(setq viperlanguage-keywords nil)
(setq viperlanguage-types nil)
(setq viperlanguage-constants nil)
(setq viperlanguage-events nil)
(setq viperlanguage-functions nil)

;; clear memory. no longer needed
(setq viperlanguage-keywords-regexp nil)
(setq viperlanguage-types-regexp nil)
(setq viperlanguage-constants-regexp nil)
(setq viperlanguage-events-regexp nil)
(setq viperlanguage-functions-regexp nil)

(provide 'viperlanguage-mode)
;;; viperlanguage-mode.el ends here
