;;; viperlanguage-mode.el --- Syntax highlighting for Viper -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos <dennisspiliopoylos@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/viperlanguage-mode
;; Package-Requires: ((emacs "27.1") (request "0.3.2"))

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

;; Variables

(defvar-local viperlanguage-highlight-overlays nil "Highglight overlays of errors reported by Viper.")
(defvar-local viperlanguage-is-verified nil "Holds the status of the program regarding its verification by Viper.")
(defvar viperlanguage-viper-path nil "The location of Viper.")
(defvar viperlanguage-server-port nil "Holds the port where the Viper server is listening.")
(defvar viperlanguage-default-tab-width 4 "Space-tab equivalence in a Viper program.")
(defvar viperlanguage-async-buffer nil "The buffer in which Viper server is running.")
(defvar viperlanguage-async-timer nil "Holds the timer of the function ran to identify the Viper server port.")
(defvar-local viperlanguage-backend "silicon" "The backend that should be used by Viper.")
(defvar-local viperlanguage-backend-options "--disableCaching --z3Exe=/home/shit/ViperToolsLinux/z3/bin/z3" "The backend options that Viper should use.")

;; helper functions
(defun viperlanguage-pos-at-line-col (lc buffer)
  "Find the absolute position given by row column in LC in a BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (let ((l (car lc))
            (c (nth 1 lc)))
        (goto-char (point-min))
        (forward-line (1- l))
        (move-to-column (1- c))
        (point)))))

;; faces
(defgroup viperlanguage-faces nil
  "Viperlanguage highlight faces."
  :group 'tools)

(defface viperlanguage-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Viperlanguage face for errors."
  :group 'viperlanguage-faces)

(defface viperlanguage-verified-face
  '((t (:weight bold :foreground "Green")))
  "The face used to highlight succesful verification.")

(defface viperlanguage-unverified-face
  '((t (:weight bold :foreground "Red")))
  "The face used to highlight failed verification.")

(defface viperlanguage-notran-face
  '((t (:weight bold :foreground "Orange")))
  "The face used to highlight not run verification.")

;; define several category of keywords
(setq viperlanguage-keywords '("domain" "axiom" "method" "while" "var" "import" "function" "predicate" "field" "if" "else" "returns"))
(setq viperlanguage-types '("Ref" "Bool" "Int" "Rational" "Perm" "Seq" "Set" "Multiset"))
(setq viperlanguage-constants '("true" "false"))
(setq viperlanguage-events '("exists" "forall" "invariant" "apply" "requires" "ensures" "fold" "unfold" "inhale" "exhale" "assert" "unfolding" "in" "forperm" "package"))
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
        (,viperlanguage-keywords-regexp . font-lock-keyword-face)))

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


;; indentation
(defun viperlanguage-count-braces ()
  "Return a number that corresponds to how many more curly braces or parentheses have been opened than closed in the current line."
  (let ((s (thing-at-point 'line t))
	      (i 0)
	      (res 0))
    (while (< i (length s))
      (when (or (eq (aref s i) ?}) (eq (aref s i) ?\)))
	      (setq res (1- res)))
      (when (or (eq (aref s i) ?{) (eq (aref s i) ?\())
	      (setq res (1+ res)))
      (setq i (1+ i)))
    res))

(defun viperlanguage-indent-line ()
  "Indent current line as Viper code."
  (interactive)
  (save-excursion
    (let ((curindent 0)
	        (beg t))
      (save-excursion
	      (beginning-of-line)
	      (if (bobp)
	          (indent-line-to 0)
	        (setq curindent (+ curindent (viperlanguage-count-braces)))
	        (while beg
	          (forward-line -1)
	          (setq curindent (+ curindent (viperlanguage-count-braces)))
	          (setq beg (or (looking-at "[ \t]*\n") (and (not (bobp)) (not (eq (current-indentation) 0))))))
	        (when (< curindent 0)
	          (setq curindent 0))))
      (let (fix)
        (if (> (viperlanguage-count-braces) 0)
	          (setq fix -1)
	        (setq fix 0))
        (indent-line-to (* (+ curindent fix) viperlanguage-default-tab-width)))))
  (let ((pos (point))
        begpos)
    (save-excursion
      (beginning-of-line)
      (setq begpos (point)))
    (when (equal pos begpos)
      (skip-syntax-forward "\s"))))

(defun viperlanguage-brace-and-indent ()
  "Insert a closing brace and indent line."
  (interactive)
  (insert-char ?})
  (viperlanguage-indent-line))

;;; make requests to server

(defun viperlanguage-request-url (cmd)
  "Return the url for a request to the viper server given that we want to execute CMD."
  (format "http://localhost:%s/%s" viperlanguage-server-port cmd))

(defun viperlanguage-read-async ()
  "Try to find the port in which the Viper server is listening."
  (interactive)
  (with-current-buffer viperlanguage-async-buffer
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at "ViperServer online at http://localhost:[0123456789]*\n")
        (setq viperlanguage-server-port (substring (thing-at-point 'line t) 39 -1))
        (cancel-timer viperlanguage-async-timer)
        (setq viperlanguage-async-timer nil))
      (forward-line 1))))

(defun viperlanguage-start-server ()
  "Start the Viper server."
  (interactive)
  (when (not viperlanguage-server-port)
    (let ((viperlanguage-viperserver (concat (file-name-as-directory viperlanguage-viper-path) "backends/viperserver.jar")))
      (let ((b (format "%s" (async-shell-command (format "java -jar %s" viperlanguage-viperserver)))))
        (string-match "window [1234567890]* on \\(.*\\)>" b)
        (setq viperlanguage-async-buffer (match-string 1 b))
        (setq viperlanguage-async-timer (run-with-timer 1 1 'viperlanguage-read-async))))))

(defun viperlanguage-stop-server ()
  "Stop the Viper server."
  (interactive)
  (when viperlanguage-server-port
    (request (viperlanguage-request-url "exit")
      (setq viperlanguage-server-port nil)
      (setq viperlanguage-async-timer nil))))

(defun viperlanguage-verify ()
  "Ask the Viper server to verify the file corresponding to the current buffer."
  (interactive)
  (when (eq major-mode 'viperlanguage-mode)
    (if viperlanguage-server-port
        (viperlanguage-verify-file buffer-file-name (current-buffer))
      (setq-local viperlanguage-is-verified nil)
      (message "No active viper server!"))))

(defun viperlanguage-verify-file (file-path buffer)
  "Verify the file with path FILE-PATH in the buffer BUFFER."
  (setq-local viperlanguage-is-verified 3)
  (request (viperlanguage-request-url "verify")
    :type "POST"
    :data (json-encode
           (cons
            (cons "arg"
                  (format
                   "%s %s \"%s\""
                   viperlanguage-backend viperlanguage-backend-options file-path)) ()))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((id (cdr (assoc 'id data))))
                  (viperlanguage-get-verification id buffer))))))

(defun viperlanguage-get-verification (id buffer)
  "Get the verification result for id ID in buffer BUFFER."
  (request (concat (viperlanguage-request-url "verify") (format "/%s" id))
    :type "GET"
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((filtered
                       (-filter
                        (lambda (a)
                          (let ((msgt (format "%s" (alist-get 'msg_type a))))
                            (or
                             (equal msgt "verification_result")
                             (equal msgt "ast_construction_result"))))
                        (mapcar
                         (lambda (a)
                           (json-parse-string
                            a
                            :object-type 'alist
                            :array-type 'list))
                         (split-string data "\n")))))
                  (mapc (lambda (r) 
                          (viperlanguage-parse-results r buffer))
                        filtered))))))

(defun viperlanguage-parse-results (data buffer)
  "Parse the Viper results in DATA of the buffer BUFFER."
  (with-current-buffer buffer
    (seq-do #'delete-overlay viperlanguage-highlight-overlays)
    (let* ((msg_body (alist-get 'msg_body data))
           (status (alist-get 'status msg_body))
           (details (alist-get 'details msg_body))
           (result (alist-get 'result details))
           (errors (alist-get 'errors result)))
      (if (equal (format "%s" (alist-get 'msg_type data)) "verification_result")
          (viperlanguage-use-error-results status errors buffer)
        (viperlanguage-use-ast-results status errors buffer)))))

(defun viperlanguage-use-error-results (status errors buffer)
  "If STATUS is success then the program verified, otherwise parse each error in ERRORS seperately for the file in buffer BUFFER."
  (with-current-buffer buffer
    (if (equal (format "%s" status) "success")
        (progn 
          (message "Program verified succesfully.")
          (setq-local viperlanguage-is-verified 1))
      (setq-local viperlanguage-is-verified 2)
      (mapc (lambda (err) (viperlanguage-handle-error err buffer)) errors))))

(defun viperlanguage-handle-error (err buffer)
  "Handle a specific error ERR for the buffer BUFFER."
  (with-current-buffer buffer
    (let* ((position (alist-get 'position err))
           (starts (alist-get 'start position))
           (ends (alist-get 'end position))
           (start (viperlanguage-pos-at-line-col (mapcar 'string-to-number (split-string starts ":")) buffer))
           (end (viperlanguage-pos-at-line-col (mapcar 'string-to-number (split-string ends ":")) buffer))
           (text (alist-get 'text err)))
      (let ((ov (make-overlay
                 start
                 end)))
        (push ov viperlanguage-highlight-overlays)
        (overlay-put ov 'face 'viperlanguage-error)
        (overlay-put ov 'help-echo text)
        (overlay-put ov
                     'cursor-sensor-functions
                     (list
                      (lambda (window pos action)
                        (when (eq action 'entered)
                          (message "%s" text)))))
        (message "%s" text)))))

(defun viperlanguage-use-ast-results (status errors buffer)
  "Handle the ast construction results using the STATUS of the construction and the ERRORS occured for BUFFER."
  (with-current-buffer buffer
    (when (equal (format "%s" status) "failure")
      (message "AST construction failed.")
      (setq-local viperlanguage-is-verified 2)
      (mapc (lambda (err) (viperlanguage-handle-ast-error err buffer)) errors))))

(defun viperlanguage-handle-ast-error (err buffer)
  "Parse a specific error ERR regarding the ast construction of the program in BUFFER."
  (with-current-buffer buffer
    (let* ((position (alist-get 'position err))
           (start (alist-get 'start position))
           (pos (viperlanguage-pos-at-line-col (mapcar 'string-to-number (split-string start ":")) buffer))
           (text (alist-get 'text err)))
      (let ((ov (make-overlay
                 pos
                 (1+ pos))))
        (push ov viperlanguage-highlight-overlays)
        (overlay-put ov 'face 'viperlanguage-error)
        (overlay-put ov 'help-echo text)))))


(defun viperlanguage-mode-line ()
  "Return the string corresponding to the status of the verification for the current buffer."
  (if (equal major-mode 'viperlanguage-mode)
      (if (not viperlanguage-is-verified)
          (concat "[" (propertize "Unknown" 'face 'viperlanguage-notran-face) "]")
        (if (equal viperlanguage-is-verified 1)
            (concat "[" (propertize "Verified" 'face 'viperlanguage-verified-face) "]")
          (if (equal viperlanguage-is-verified 2)
              (concat "[" (propertize "Unverified" 'face 'viperlanguage-unverified-face) "]")
            (concat "[" (propertize "Verifying..." 'face 'viperlanguage-notran-face) "]"))))
    ""))
;;;###autoload

(defvar viperlanguage-mode-map nil "Keymap for viperlanguage-mode.")

(when (not viperlanguage-mode-map)
  (setq viperlanguage-mode-map (make-sparse-keymap))
  (define-key viperlanguage-mode-map (kbd "C-c C-c") 'viperlanguage-start-server)
  (define-key viperlanguage-mode-map (kbd "C-c C-v") 'viperlanguage-verify)
  (define-key viperlanguage-mode-map (kbd "C-c C-x") 'viperlanguage-stop-server)
  (define-key viperlanguage-mode-map (kbd "}") 'viperlanguage-brace-and-indent))


(defvar viperlanguage-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(define-derived-mode viperlanguage-mode fundamental-mode
  "viperlanguage mode"
  "Major mode for editing Viper"
  :syntax-table viperlanguage-syntax-table
  (setq font-lock-defaults '((viperlanguage-font-lock-keywords)))
  (setq-local indent-line-function #'viperlanguage-indent-line)
  (setq comment-start "//")
  (setq comment-end "")
  (cursor-sensor-mode)
  (setq global-mode-string (or global-mode-string '("")))
  (unless (member '(:eval (viperlanguage-mode-line)) global-mode-string)
    (setq global-mode-string (append global-mode-string '((:eval (viperlanguage-mode-line)))))))

(add-to-list 'auto-mode-alist '("\\.vpr" . viperlanguage-mode))

(provide 'viperlanguage-mode)
;;; viperlanguage-mode.el ends here
