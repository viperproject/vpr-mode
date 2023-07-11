;;; vpr-mode.el --- Emacs IDE for Viper -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos <dennisspiliopoylos@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/vpr-mode
;; Package-Requires: ((emacs "27.1") (request "0.3.2"))

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; Defines syntax highlighting and viperserver frontend capabilities for the
;; Viper language.

;;; Code:

;; Requirements

(require 'param-config)
(require 'vpr-mode-params)

;; Variables

(defvar-local vpr-highlight-overlays nil "Highglight overlays of errors reported by Viper.")
(defvar-local vpr-is-verified nil "Holds the status of the program regarding its verification by Viper.")
(defvar-local vpr-has-errors nil "Is set to true when there are results with a verification status of failure.")
(defvar-local vpr-has-exceptions nil "Is set to true when an exception is raised by the server.")
(defvar vpr-viperserver-path nil "The location of Viperserver jar file.")
(defvar vpr-server-port nil "Holds the port where the Viper server is listening.")
(defvar vpr-default-tab-width 2 "Space-tab equivalence in a Viper program.")
(defvar vpr-async-buffer nil "The buffer in which Viper server is running.")
(defvar vpr-async-timer nil "Holds the timer of the function ran to identify the Viper server port.")
(defvar-local vpr-viperserver-options "--disableCaching --z3Exe=%s")

;; helper functions

(defun vpr-pos-at-line-col (lc buffer)
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

(defgroup vpr-faces nil
  "Vpr highlight faces."
  :group 'tools)

(defface vpr-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Vpr face for errors."
  :group 'vpr-faces)

(defface vpr-verified-face
  '((t (:weight bold :foreground "Green")))
  "The face used to highlight succesful verification.")

(defface vpr-backend-face
  '((t (:weight bold :foreground "#81d4fa")))
  "The face used to highlight the backend in modeline.")

(defface vpr-unverified-face
  '((t (:weight bold :foreground "Red")))
  "The face used to highlight failed verification.")

(defface vpr-notran-face
  '((t (:weight bold :foreground "Orange")))
  "The face used to highlight not run verification.")

(defface vpr-argument-face
  '((t (:foreground "Grey")))
  "The face used to distinguish args from args of args in the arguments construction buffer.")

;; define several category of keywords
(setq vpr-keywords '("domain" "axiom" "method" "while" "label" "goto" "var" "import" "function" "predicate" "field" "if" "else" "returns"))
(setq vpr-types '("Ref" "Bool" "Int" "Rational" "Perm" "Seq" "Set" "Multiset"))
(setq vpr-constants '("true" "false"))
(setq vpr-events '("exists" "forall" "invariant" "apply" "requires" "ensures" "fold" "unfold" "inhale" "assume" "exhale" "assert" "unfolding" "in" "forperm" "package" "decreases"))
(setq vpr-functions '())

;; generate regex string for each category of keywords
(setq vpr-keywords-regexp (regexp-opt vpr-keywords 'words))
(setq vpr-type-regexp (regexp-opt vpr-types 'words))
(setq vpr-constant-regexp (regexp-opt vpr-constants 'words))
(setq vpr-event-regexp (regexp-opt vpr-events 'words))
(setq vpr-functions-regexp (regexp-opt vpr-functions 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq vpr-font-lock-keywords
      `(
        (,vpr-type-regexp . font-lock-type-face)
        (,vpr-constant-regexp . font-lock-constant-face)
        (,vpr-event-regexp . font-lock-builtin-face)
        (,vpr-functions-regexp . font-lock-function-name-face)
        (,vpr-keywords-regexp . font-lock-keyword-face)))

;; clear memory. no longer needed
(setq vpr-keywords nil)
(setq vpr-types nil)
(setq vpr-constants nil)
(setq vpr-events nil)
(setq vpr-functions nil)

;; clear memory. no longer needed
(setq vpr-keywords-regexp nil)
(setq vpr-types-regexp nil)
(setq vpr-constants-regexp nil)
(setq vpr-events-regexp nil)
(setq vpr-functions-regexp nil)


;; indentation
(defun vpr-count-braces ()
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

(defun vpr-only-braces ()
  "Check if the current line has only closing braces or parentheses."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[\t })]+$")))

(defun vpr-indent-line ()
  "Indent current line as Viper code."
  (interactive)
  (save-excursion
    (let ((curindent 0)
	        (beg t))
      (save-excursion
	      (beginning-of-line)
	      (if (bobp)
	          (indent-line-to 0)
	        (setq curindent (vpr-count-braces))
	        (while beg
	          (forward-line -1)
	          (setq curindent (+ curindent (vpr-count-braces)))
	          (setq beg (or (and (not (bobp)) ( looking-at "[ \t]*\n")) (and (not (bobp)) (not (eq (current-indentation) 0))))))
	        (when (< curindent 0)
	          (setq curindent 0))))
      (let (fix)
        (if (> (vpr-count-braces) 0)
	          (setq fix -1)
          (if (and (< (vpr-count-braces) 0) (not (vpr-only-braces)))
              (setq fix +1)
	          (setq fix 0)))
        (indent-line-to (* (+ curindent fix) vpr-default-tab-width)))))
  (let ((pos (point))
        begpos)
    (save-excursion
      (beginning-of-line)
      (setq begpos (point)))
    (when (equal pos begpos)
      (skip-syntax-forward "\s"))))

(defun vpr-brace-and-indent ()
  "Insert a closing brace and indent line."
  (interactive)
  (insert-char ?})
  (vpr-indent-line))

;;; configure

(defun vpr-change-backend ()
  "Alternate the backend from carbon to silicon and vice versa."
  (interactive)
  (if (equal vpr-backend "carbon")
      (progn
        (setq-local vpr-backend "silicon")
        (setq-local vpr-carbon-args-set vpr-args-set)
        (setq-local vpr-carbon-args-of-args vpr-args-of-args)
        (setq-local vpr-args-set vpr-silicon-args-set)
        (setq-local vpr-args-of-args vpr-silicon-args-of-args)
        (setq-local vpr-args-doc vpr-silicon-args-doc)
        (setq-local vpr-args-that-need-args vpr-silicon-args-that-need-args)
        (setq-local vpr-args-that-need-many-args vpr-silicon-args-that-need-many-args))
    (setq-local vpr-backend "carbon")
    (setq-local vpr-silicon-args-set vpr-args-set)
    (setq-local vpr-silicon-args-of-args vpr-args-of-args)
    (setq-local vpr-args-set vpr-carbon-args-set)
    (setq-local vpr-args-of-args vpr-carbon-args-of-args)
    (setq-local vpr-args-doc vpr-carbon-args-doc)
    (setq-local vpr-args-that-need-args vpr-carbon-args-that-need-args)
    (setq-local vpr-args-that-need-many-args vpr-carbon-args-that-need-many-args))
  (force-mode-line-update))

(defun vpr-edit-args ()
  "Spawn the construction buffer for the arguments."
  (interactive)
  (let ((cur-buf (buffer-name))
        (arg-buf (format "%s%s" (current-buffer) "~args"))
        (arg-set vpr-args-set)
        (args-of-args vpr-args-of-args)
        (args-doc vpr-args-doc)
        (args-that-need-args vpr-args-that-need-args)
        (args-that-need-many-args vpr-args-that-need-many-args))
    (with-current-buffer (get-buffer-create arg-buf)
      (vpr-args-mode)
      (setq-local vpr-args-original-buffer cur-buf)
      (setq-local vpr-args-set arg-set)
      (setq-local vpr-args-of-args args-of-args)
      (setq-local vpr-args-doc args-doc)
      (setq-local vpr-args-that-need-args args-that-need-args)
      (setq-local vpr-args-that-need-many-args args-that-need-many-args)
      (vpr-populate-args-buffer))
    (pop-to-buffer arg-buf)))

;;; make requests to server

(defun vpr-request-url (cmd)
  "Return the url for a request to the viper server given that we want to execute CMD."
  (format "http://localhost:%s/%s" vpr-server-port cmd))

(defun vpr-read-async ()
  "Try to find the port in which the Viper server is listening."
  (interactive)
  (with-current-buffer vpr-async-buffer
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at "ViperServer online at http://localhost:[0123456789]*\n")
        (setq vpr-server-port (substring (thing-at-point 'line t) 39 -1))
        (cancel-timer vpr-async-timer)
        (setq vpr-async-timer nil))
      (forward-line 1))))

(defun vpr-start-server ()
  "Start the Viper server."
  (interactive)
  (when (not vpr-server-port)
    (let ((vpr-viperserver vpr-viperserver-path))
      (let ((b (format "%s" (async-shell-command (format "java -jar -Xss128m %s" vpr-viperserver)))))
        (string-match "window [1234567890]* on \\(.*\\)>" b)
        (setq vpr-async-buffer (match-string 1 b))
        (setq vpr-async-timer (run-with-timer 1 1 'vpr-read-async))))))

(defun vpr-stop-server ()
  "Stop the Viper server."
  (interactive)
  (when vpr-server-port
    (request (vpr-request-url "exit")
      (setq vpr-server-port nil)
      (setq vpr-async-timer nil))))

(defun vpr-verify ()
  "Ask the Viper server to verify the file corresponding to the current buffer."
  (interactive)
  (when (eq major-mode 'vpr-mode)
    (if vpr-server-port
        (vpr-verify-file buffer-file-name (current-buffer))
      (setq-local vpr-is-verified nil)
      (setq-local vpr-has-errors nil)
      (setq-local vpr-has-exceptions nil)
      (message "No active viper server!"))))

(defun vpr-verify-file (file-path buffer)
  "Verify the file with path FILE-PATH in the buffer BUFFER."
  (setq-local vpr-is-verified 3)
  (setq-local vpr-has-errors nil)
  (setq-local vpr-has-exceptions nil)
  (force-mode-line-update)
  (let ((opts (format "%s %s" (format  vpr-backend-options vpr-z3-path) (vpr-args-serialize))))
    (request (vpr-request-url "verify")
      :type "POST"
      :data (json-encode
             (cons
              (cons "arg"
                    (format
                     "%s %s \"%s\""
                     vpr-backend opts file-path)) ()))
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((id (cdr (assoc 'id data))))
                    (vpr-get-verification id buffer)))))))

(defun vpr-get-verification (id buffer)
  "Get the verification result for id ID in buffer BUFFER."
  (request (concat (vpr-request-url "verify") (format "/%s" id))
    :type "GET"
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((filtered
                       (-filter
                        (lambda (a)
                          (let ((msgt (format "%s" (alist-get 'msg_type a))))
                            (or
                             (equal msgt "verification_result")
                             (equal msgt "ast_construction_result")
                             (equal msgt "exception_report"))))
                        (mapcar
                         (lambda (a)
                           (json-parse-string
                            a
                            :object-type 'alist
                            :array-type 'list))
                         (split-string data "\n")))))
                  (mapc (lambda (r)
                          (vpr-parse-results r buffer))
                        filtered))))))

(defun vpr-parse-results (data buffer)
  "Parse the Viper results in DATA of the buffer BUFFER."
  (with-current-buffer buffer
    (seq-do #'delete-overlay vpr-highlight-overlays)
    (let* ((msg_body (alist-get 'msg_body data))
           (status (alist-get 'status msg_body))
           (details (alist-get 'details msg_body))
           (result (alist-get 'result details))
           (errors (alist-get 'errors result))
           (msg (alist-get 'message msg_body)))
      (when (equal (format "%s" (alist-get 'msg_type data)) "verification_result")
        (vpr-use-error-results status errors buffer))
      (when (equal (format "%s" (alist-get 'msg_type data)) "ast_construction_result")
        (vpr-use-ast-results status errors buffer))
      (when (equal (format "%s" (alist-get 'msg_type data)) "exception_report")
        (vpr-use-exception-results msg)))))

(defun vpr-use-exception-results (msg)
  "Use the exception result.  Right now just print the MSG argument and stop the server."
  (message "%s" msg)
  (message "Exception! Shutting down server...")
  (setq-local vpr-has-exceptions t)
  (vpr-stop-server))

(defun vpr-use-error-results (status errors buffer)
  "If STATUS is success then the program verified, otherwise parse each error in ERRORS seperately for the file in buffer BUFFER."
  (with-current-buffer buffer
    (if (equal (format "%s" status) "success")
        (setq-local vpr-is-verified 1)
      (setq-local vpr-is-verified 2)
      (setq-local vpr-has-errors t)
      (force-mode-line-update)
      (mapc (lambda (err) (vpr-handle-error err buffer)) errors))))

(defun vpr-handle-error (err buffer)
  "Handle a specific error ERR for the buffer BUFFER."
  (with-current-buffer buffer
    (let* ((position (alist-get 'position err))
           (starts (alist-get 'start position))
           (ends (alist-get 'end position))
           (start (vpr-pos-at-line-col (mapcar 'string-to-number (split-string starts ":")) buffer))
           (end (vpr-pos-at-line-col (mapcar 'string-to-number (split-string ends ":")) buffer))
           (text (alist-get 'text err)))
      (let ((ov (make-overlay
                 start
                 end)))
        (push ov vpr-highlight-overlays)
        (overlay-put ov 'face 'vpr-error)
        (overlay-put ov 'help-echo text)
        (overlay-put ov
                     'cursor-sensor-functions
                     (list
                      (lambda (window pos action)
                        (when (eq action 'entered)
                          (message "%s" text)))))
        (message "%s" text)))))

(defun vpr-use-ast-results (status errors buffer)
  "Handle the ast construction results using the STATUS of the construction and the ERRORS occured for BUFFER."
  (with-current-buffer buffer
    (when (equal (format "%s" status) "failure")
      (message "AST construction failed.")
      (setq-local vpr-is-verified 2)
      (setq-local vpr-has-errors t)
      (force-mode-line-update)
      (mapc (lambda (err) (vpr-handle-ast-error err buffer)) errors))))

(defun vpr-handle-ast-error (err buffer)
  "Parse a specific error ERR regarding the ast construction of the program in BUFFER."
  (with-current-buffer buffer
    (let* ((position (alist-get 'position err))
           (start (alist-get 'start position))
           (pos (vpr-pos-at-line-col (mapcar 'string-to-number (split-string start ":")) buffer))
           (text (alist-get 'text err)))
      (let ((ov (make-overlay
                 pos
                 (1+ pos))))
        (push ov vpr-highlight-overlays)
        (overlay-put ov 'face 'vpr-error)
        (overlay-put ov 'help-echo text)))))

(defun vpr-mode-line ()
  "Return the string corresponding to the status of the verification for the current buffer."
  (let ((b (concat "[Backend: " (propertize (format "%s" vpr-backend) 'face 'vpr-backend-face)
                   " | State: ")))
    (if (equal major-mode 'vpr-mode)
        (if (not vpr-is-verified)
            (concat b (propertize "Unknown" 'face 'vpr-notran-face) "]")
          (if (and (equal vpr-is-verified 1) (not vpr-has-exceptions))
              (concat b (propertize "Verified" 'face 'vpr-verified-face) "]")
            (if (equal vpr-is-verified 2)
                (concat b (propertize "Unverified" 'face 'vpr-unverified-face) "]")
              (if vpr-has-exceptions
                  (concat b (propertize "Exception" 'face 'vpr-unverified-face) "]")
                (concat b (propertize "Verifying..." 'face 'vpr-notran-face) "]")))))
      "")))

;;;###autoload

(defvar vpr-mode-map nil "Keymap for vpr-mode.")

(when (not vpr-mode-map)
  (setq vpr-mode-map (make-sparse-keymap))
  (define-key vpr-mode-map (kbd "C-c C-c") 'vpr-start-server)
  (define-key vpr-mode-map (kbd "C-c C-v") 'vpr-verify)
  (define-key vpr-mode-map (kbd "C-c C-x") 'vpr-stop-server)
  (define-key vpr-mode-map (kbd "C-c C-b") 'vpr-change-backend)
  (define-key vpr-mode-map (kbd "C-c C-a") 'vpr-edit-args)
  (define-key vpr-mode-map (kbd "}") 'vpr-brace-and-indent))


(defvar vpr-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(define-derived-mode vpr-mode fundamental-mode
  "vpr mode"
  "Major mode for editing Viper"
  :syntax-table vpr-syntax-table
  (setq font-lock-defaults '((vpr-font-lock-keywords)))
  (setq-local indent-line-function #'vpr-indent-line)
  (setq comment-start "//")
  (setq comment-end "")
  (cursor-sensor-mode)
  (setq global-mode-string (or global-mode-string '("")))
  (vpr-args-initialize)
  (unless (member '(:eval (vpr-mode-line)) global-mode-string)
    (setq global-mode-string (append global-mode-string '((:eval (vpr-mode-line)))))))

(add-to-list 'auto-mode-alist '("\\.vpr$" . vpr-mode))

(provide 'vpr-mode)
;;; vpr-mode.el ends here
