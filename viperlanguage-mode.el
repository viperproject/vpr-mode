;;; viperlanguage-mode.el --- Syntax highlighting for Viper -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dionisios Spiliopoulos

;; Author: Dionisios Spiliopoulos <dennisspiliopoylos@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/viperlanguage-mode
;; Package-Requires: ((emacs "26.2") (request "20211107.1907"))

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

;; local variables

(setq-local viperlanguage-highlight-overlays nil)
(setq-default viperlanguage-highlight-overlays nil)

;; helper functions
(defun viperlanguage-pos-at-line-col (lc buffer)
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
  "Viperlanguage highlight faces.")

(defface viperlanguage-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Viperlanguage face for errors."
  :group 'viperlanguage-faces)

;; define several category of keywords
(setq viperlanguage-keywords '("method" "while" "var" "import" "function" "predicate" "field" "if" "else" "returns"))
(setq viperlanguage-types '("Ref" "Bool" "Int" "Rational" "Perm" "Seq" "Set" "Multiset"))
(setq viperlanguage-constants '())
(setq viperlanguage-events '("invariant" "requires" "ensures" "fold" "unfold" "inhale" "exhale" "assert" "unfolding" "in"))
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
	("//.*\n" . font-lock-comment-face)))

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
(setq viperlanguage-default-tab-width 4)
(defun viperlanguage-count-braces ()
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
      (if (> (viperlanguage-count-braces) 0)
	  (setq fix -1)
	(setq fix 0))
      (indent-line-to (* (+ curindent fix) viperlanguage-default-tab-width)))))

;;; make requests to server
(setq viperlanguage-viper-path nil)
(setq viperlanguage-server-port nil)
(setq viperlanguage-async-timer nil)
(setq viperlanguage-async-buffer nil)
(setq viperlanguage-assoc-files nil)


;; viperserver options
(setq viperlanguage-backend "silicon")

(defun viperlanguage-request-url (cmd)
  (format "http://localhost:%s/%s" viperlanguage-server-port cmd))

(defun viperlanguage-read-async ()
  (interactive)
  (with-current-buffer viperlanguage-async-buffer
    (beginning-of-buffer)
    (while (not (eobp))
      (when (looking-at "ViperServer online at http://localhost:[0123456789]*\n")
        (setq viperlanguage-server-port (substring (thing-at-point 'line t) 39 -1))
        (cancel-timer viperlanguage-async-timer)
        (setq viperlanguage-async-timer nil))
      (forward-line 1))))

(defun viperlanguage-start-server ()
  (interactive)
  (when (not viperlanguage-server-port)
    (let ((viperlanguage-viperserver (concat (file-name-as-directory viperlanguage-viper-path) "backends/viperserver.jar")))
      (setq b (format "%s" (async-shell-command (format "java -jar %s" viperlanguage-viperserver))))
      (string-match "window [1234567890]* on \\(.*\\)>" b)
      (setq viperlanguage-async-buffer (match-string 1 b))
      (setq viperlanguage-async-timer (run-with-timer 1 1 'viperlanguage-read-async)))))

(defun viperlanguage-stop-server ()
  (interactive)
  (when viperlanguage-server-port
    (request (viperlanguage-request-url "exit")
      (setq viperlanguage-server-port nil)
      (setq viperlanguage-async-timer nil))))

(defun viperlanguage-verify ()
  (interactive)
  (if viperlanguage-server-port
      (viperlanguage-verify-file buffer-file-name (current-buffer))
    (message "No active viper server!")))

(defun viperlanguage-verify-file (file-path buffer)
  (let ((viperlanguage-z3-path (concat (file-name-as-directory viperlanguage-viper-path) "z3/bin/z3")))
    (request (viperlanguage-request-url "verify")
      :type "POST"
      :data (json-encode
             (cons
              (cons "arg"
                    (format
                     "silicon --disableCaching --z3Exe=%s \"%s\""
                     viperlanguage-z3-path file-path)) ()))
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((id (cdr (assoc 'id data))))
                    (viperlanguage-get-verification id buffer)))))))

(defun viperlanguage-get-verification (id buffer)
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
  (with-current-buffer buffer
    (if (equal (format "%s" status) "success")
        (message "Program verified succesfully.")
      (mapc (lambda (err) (viperlanguage-handle-error err buffer)) errors))))

(defun viperlanguage-handle-error (err buffer)
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
  (with-current-buffer buffer
    (when (equal (format "%s" status) "failure")
      (message "AST construction failed.")
      (mapc (lambda (err) (viperlanguage-handle-ast-error err buffer)) errors))))

(defun viperlanguage-handle-ast-error (err buffer)
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


;;;###autoload

(defvar viperlanguage-mode-map nil "Keymap for viperlanguage-mode.")

(when (not viperlanguage-mode-map)
  (setq viperlanguage-mode-map (make-sparse-keymap))
  (define-key viperlanguage-mode-map (kbd "C-c C-c") 'viperlanguage-start-server)
  (define-key viperlanguage-mode-map (kbd "C-c C-v") 'viperlanguage-verify)
  (define-key viperlanguage-mode-map (kbd "C-c C-x") 'viperlanguage-stop-server))

(define-derived-mode viperlanguage-mode fundamental-mode
  "viperlanguage mode"
  "Major mode for editing Viper"
  (setq font-lock-defaults '((viperlanguage-font-lock-keywords)))
  (setq-local indent-line-function #'viperlanguage-indent-line)
  (cursor-sensor-mode))

(add-to-list 'auto-mode-alist '("\\.vpr" . viperlanguage-mode))

(provide 'viperlanguage-mode)
;;; viperlanguage-mode.el ends here
