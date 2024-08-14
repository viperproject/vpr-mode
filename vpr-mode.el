;;; vpr-mode.el --- Syntax highlighting for Viper -*- lexical-binding: t -*-

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

;; Variables

(defvar-local vpr-highlight-overlays nil "Highglight overlays of errors reported by Viper.")
(defvar-local vpr-is-verified nil "Holds the status of the program regarding its verification by Viper.")
(defvar-local vpr-has-errors nil "Is set to true when there are results with a verification status of failure.")
(defvar-local vpr-has-exceptions nil "Is set to true when an exception is raised by the server.")
(defvar vpr-viperserver-path nil "The location of Viperserver jar file.")
(defvar vpr-z3-path nil "The location of Z3.")
(defvar vpr-boogie-path nil "The location of Boogie.")
(defvar vpr-server-port nil "Holds the port where the Viper server is listening.")
(defvar vpr-default-tab-width 2 "Space-tab equivalence in a Viper program.")
(defvar vpr-async-buffer nil "The buffer in which Viper server is running.")
(defvar vpr-async-timer nil "Holds the timer of the function ran to identify the Viper server port.")
(defvar vpr-pretty-symbols
  '(("<=" . 8804)
    (">=" . 8805)
    ("!=" . 8800)
    (":=" . 8788)
    ("&&" . 10043)
    ("||" . 8744)
    ("=>" . 8658)
    ("<==>" . 10234)
    ("==>" . 10233)
    ("<==" . 10232)
    ("exists" . 8707)
    ("::" . 8729)
    ("in" . 8712))
  "Symbols that vpr-mode will automatically prettify")
(defvar-local vpr-backend "silicon" "The backend that should be used by Viper.")
(defvar-local vpr-backend-options "--disableCaching --z3Exe=%s")
(defvar-local vpr-carbon-options "--boogieExe=%s" "The carbon backend option that Viper should use.")



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

;; ========
;; Keywords
;; ========

(defun vpr-build-keywords ()
  ;; define several category of keywords
  (let* ((vpr-keywords '("domain"
                         "axiom"
                         "method"
                         "while"
                         "label"
                         "goto"
                         "var"
                         "import"
                         "function"
                         "predicate"
                         "field"
                         "if"
                         "else"
                         "returns"))
         (vpr-types '("Ref"
                      "Bool"
                      "Int"
                      "Rational"
                      "Perm"
                      "Seq"
                      "Set"
                      "Multiset"))
         (vpr-constants '("true"
                          "false"))
         (vpr-events '("exists"
                       "invariant"
                       "apply"
                       "requires"
                       "ensures"
                       "fold"
                       "unfold"
                       "inhale"
                       "assume"
                       "exhale"
                       "assert"
                       "unfolding"
                       "in"
                       "forperm"
                       "package"
                       "decreases"))
         (vpr-functions '())
         
         (vpr-keywords-regexp (regexp-opt vpr-keywords 'words))
         (vpr-type-regexp (regexp-opt vpr-types 'words))
         (vpr-constant-regexp (regexp-opt vpr-constants 'words))
         (vpr-event-regexp (regexp-opt vpr-events 'words))
         (vpr-functions-regexp (regexp-opt vpr-functions 'words)))
    `((,vpr-type-regexp . font-lock-type-face)
      (,vpr-constant-regexp . font-lock-constant-face)
      (,vpr-event-regexp . font-lock-builtin-face)
      (,vpr-functions-regexp . font-lock-function-name-face)
      (,vpr-keywords-regexp . font-lock-keyword-face)
      ("\\(\\_<forall\\_>\\).*?::"
       (1 (compose-region (match-beginning 1) (match-end 1) ?∀))
       (1 font-builtin-face append))
      ("\\(-\\)\\(-\\)\\(*\\)"
       (1 (compose-region (match-beginning 1) (match-end 1) ?‒))
       (1 (compose-region (match-beginning 2) (match-end 2) ?‒))
       (1 (compose-region (match-beginning 3) (match-end 3) ?✻))))))


;; ===========
;; indentation
;; ===========

(defun vpr-only-closing-braces (cur-l)
  "Check if the current line has only closing braces or parentheses. Return 1 if yes, 0 if no."
  (if (string-match-p "^[\t })]+$" cur-l)
      1
    0))

(defun vpr-part-of-spec (cur-l)
  "Return 1 if we are in a spec or an invariant, otherwise 0."
  (save-excursion
    (let ((begp (forward-line -1)))
      (if (equal begp -1)
          0
        (let ((l (thing-at-point 'line t)))
          (if (and (or (string-match-p "^method [^{]*$" l)
                       (string-match-p "^function [^{]*$" l)
                       (string-match-p "^[\t ]+\\<\\(while\\|invariant\\|requires\\|ensures\\)\\>[^{]*$" l))
                   (not (string-match-p "^[\t ]*{.*" cur-l)))
              1
            0))))))

(defun vpr-indent-line ()
  "Indent current line as Viper code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((cur-l (thing-at-point 'line t))
           (state (syntax-ppss))
           (indent-depth (- (+ (car state) (vpr-part-of-spec cur-l))
                            (vpr-only-closing-braces cur-l))))
      (indent-line-to (* indent-depth vpr-default-tab-width))))
  (when (string-match-p "^[\t ]+$" (thing-at-point 'line t))
    (end-of-line)))

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

;; ===============
;; arguments stuff
;; ===============

(defvar-local vpr-args-original-buffer nil "Holds the name of the vpr file that corresponds to a vpr arguments construction buffer.")
(defvar-local vpr-args-set nil "Holds the arguments of the viper backend.")
(defvar-local vpr-args-of-args nil "Holds arguments of arguments of the viper backend.")
(defvar-local vpr-args-doc nil "Holds arguments with their documentation depending on the backend.")
(defvar-local vpr-args-that-need-args nil "Holds arguments that need arguments depending on the backend.")
(defvar-local vpr-args-that-need-many-args nil "Holds arguments that need namy argumentds depending on the backend.")

(defvar-local vpr-silicon-args-set nil "Holds the arguments of the silicon backend.")
(defvar-local vpr-silicon-args-of-args nil "Holds arguments of arguments of the silicon backend.")

(defvar-local vpr-carbon-args-set nil "Holds the arguments of the silicon backend.")
(defvar-local vpr-carbon-args-of-args nil "Holds arguments of arguments of the silicon backend.")

(defun vpr-args-initialize ()
  "Initialize all variables having something to do with arguments."
  (setq-local vpr-args-doc vpr-silicon-args-doc)
  (setq-local vpr-args-that-need-args vpr-silicon-args-that-need-args)
  (setq-local vpr-args-that-need-many-args vpr-silicon-args-that-need-many-args)
  (setq-local vpr-carbon-args-set '("boogieExe"))
  (setq-local vpr-carbon-args-of-args `(("boogieExe" . ,vpr-boogie-path))))
;; getters

(defun vpr-args-mode-getter (prompt modes)
  "Get a mode from a list of MODES presenting a PROMPT."
  (lambda ()
    (completing-read prompt modes nil t)))

(defun vpr-args-int-getter (prompt)
  "Get an integer presenting a PROMPT."
  (lambda () (read-number prompt)))

(defun vpr-args-multi-string-getter (prompt1 prompt2)
  "Get multiple strings.  Each string is taken with PROMPT1 and between strings PROMPT2 asks the user if he wants to continue."
  (lambda ()
    (let ((s (read-string prompt1)))
      (while (y-or-n-p prompt2)
        (setq s (concat s " " (read-string prompt1))))
      s)))

;; arguments for silicon

(defvar vpr-silicon-args-doc
  '(("alternativeFunctionVerificationOrder" . "Calculate the order in which functions are verified and function axioms become available in an alternative way that takes dependencies between functions through predicate unfoldings into account. This is more complete in some cases (see Silicon issue #355) but less complete in others (see test all/issues/silicon/unofficial007).")
    ("assertionMode" . "Determines how assertion checks are encoded in SMTLIB. Options are 'pp' (push-pop) and 'cs' (soft constraints) (default: cs).")
    ("assertTimeout" . "Timeout (in ms) per SMT solver assertion (default: 0, i.e. no timeout).")
    ("assumeInjectivityOnInhale" . "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it.")
    ("checkTimeout" . "Timeout (in ms) per SMT solver check. Solver checks differ from solver asserts in that a failing assert always yields a verification error whereas a failing check doesn't, at least not directly. However, failing checks might result in performance degradation, e.g. when a dead program path is nevertheless explored, and indirectly in verification failures due to incompletenesses, e.g. when the held permission amount is too coarsely underapproximated (default: 10).")
    ("conditionalizePermissions" . "Potentially reduces the number of symbolic execution paths, by conditionalising permission expressions. E.g. rewrite \"b ==> acc(x.f, p)\" to \"acc(x.f, b ? p : none)\".This is an experimental feature; report problems if you observe any.")
    ("counterexample" . "Return counterexample for errors. Pass 'native' for returning the native model from the backend, 'variables' for returning a model of all local Viper variables, or 'mapped' (only available on Silicon) for returning a model with Ref variables resolved to object-like structures.")
    ("disableCaches" . "Disables various caches in Silicon's state.")
    ("disableCatchingExceptions" . "Don't catch exceptions (can be useful for debugging problems with Silicon)")
    ("disableChunkOrderHeuristics" . "Disable heuristic ordering of quantified chunks (context: iterated separating conjunctions).")
    ("disableHavocHack407" . "A Viper method call to ___silicon_hack407_havoc_all_R, where R is a field or predicate, results in Silicon havocking all instances of R. See also Silicon issue #407.")
    ("disableISCTriggers" . "Don't pick triggers for quantifiers, let the SMT solver do it (context: iterated separating conjunctions).")
    ("disableShortCircuitingEvaluations" . "Disable short-circuiting evaluation of AND, OR. If disabled, evaluating e.g., i > 0 && f(i), will fail if f's precondition requires i > 0.")
    ("disableSubsumption" . "Don't add assumptions gained by verifying an assert statement")
    ("disableTempDirectory" . "Disable the creation of temporary data (default: ./tmp)")
    ("disableValueMapCaching" . "Disable caching of value maps (context: iterated separating conjunctions).")
    ("enableBranchconditionReporting" . "Report branch conditions (can be useful for assertions that fail on multiple branches)")
    ("enableMoreCompleteExhale" . "Enable a more complete exhale version.")
    ("enablePredicateTriggersOnInhale" . "Emit predicate-based function trigger on each inhale of a predicate instance (context: heap-dependent functions).")
    ("excludeMethods" . "Exclude methods from verification (default: ''). Is applied after the include pattern.")
    ("handlePureConjunctsIndividually" . "Handle pure conjunction individually.Increases precision of error reporting, but may slow down verification.")
    ("includeMethods" . "Include methods in verification (default: '*'). Wildcard characters are '?' and '*'.")
    ("logConfig" . "Path to config file specifying SymbExLogger options")
    ("logLevel" . "One of the log levels ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF")
    ("mapAxiomatizationFile" . "Source file with map axiomatisation. If omitted, built-in one is used.")
    ("maxHeuristicsDepth" . "Maximal number of nested heuristics applications (default: 3)")
    ("multisetAxiomatizationFile" . "Source file with multiset axiomatisation. If omitted, built-in one is used.")
    ("numberOfErrorsToReport" . "Number of errors per member before the verifier stops. If this number is set to 0, all errors are reported.")
    ("numberOfParallelVerifiers" . "Number of verifiers run in parallel. This number plus one is the number of provers run in parallel (default: 12)")
    ("plugin" . "Load plugin(s) with given class name(s). Several plugins can be separated by ':'. The fully qualified class name of the plugin should be specified.")
    ("printMethodCFGs" . "Print a DOT (Graphviz) representation of the CFG of each method to verify to a file '<tempDirectory>/<methodName>.dot'.")
    ("printTranslatedProgram" . "Print the final program that is going to be verified to stdout.")
    ("qpSplitTimeout" . "Timeout (in ms) used by QP's split algorithm when 1) checking if a chunk holds no further permissions, and 2) checking if sufficiently many permissions have already been split off.")
    ("recursivePredicateUnfoldings" . "Evaluate n unfolding expressions in the body of predicates that (transitively) unfold other instances of themselves (default: 1)")
    ("sequenceAxiomatizationFile" . "Source file with sequence axiomatisation. If omitted, built-in one is used.")
    ("setAxiomatizationFile" . "Source file with set axiomatisation. If omitted, built-in one is used.")
    ("stateConsolidationMode" . "One of the following modes: 0: Minimal work, many incompletenesses 1: Most work, fewest incompletenesses 2: Similar to 1, but less eager 3: Less eager and less complete than 1 4: Intended for use with moreCompleteExhale")
    ("tempDirectory" . "Path to which all temporary data will be written (default: ./tmp)")
    ("timeout" . "Time out after approx. n seconds. The timeout is for the whole verification, not per method or proof obligation (default: 0, i.e. no timeout).")
    ("z3Args" . "Command-line arguments which should be forwarded to Z3. The expected format is \"<opt> <opt> ... <opt>\", excluding the quotation marks.")
    ("z3ConfigArgs" . "Configuration options which should be forwarded to Z3. The expected format is \"<key>=<val> <key>=<val> ... <key>=<val>\", excluding the quotation marks. The configuration options given here will override those from Silicon's Z3 preamble.")
    ("z3EnableResourceBounds" . "Use Z3's resource bounds instead of timeouts")
    ("z3Exe" . "Z3 executable. The environment variable Z3_EXE can also be used to specify the path of the executable.")
    ("z3LogFile" . "Log file containing the interaction with Z3, extension smt2 will be appended. (default: <tempDirectory>/logfile.smt2)")
    ("z3RandomizeSeeds" . "Set various Z3 random seeds to random values")
    ("z3ResourcesPerMillisecond" . "Z3 resources per milliseconds. Is used to convert timeouts to resource bounds.")
    ("z3SaturationTimeout" . "Timeout (in ms) used for Z3 state saturation calls (default: 100). A timeout of 0 disables all saturation checks.")
    ("z3SaturationTimeoutWeights" . "Weights used to compute the effective timeout for Z3 state saturation calls, which are made at various points during a symbolic execution. The effective timeouts for a particular saturation call is computed by multiplying the corresponding weight with the base timeout for saturation calls. Defaults to the following weights: after program preamble: 1.0 after inhaling contracts: 0.5 after unfold: 0.4 after inhale: 0.2 before repeated Z3 queries: 0.02 Weights must be non-negative, a weight of 0 disables the corresponding saturation call and a minimal timeout of 10ms is enforced.")
    ("help" . "Show help message"))
  "Documentation for silicon arguments.")

(defvar vpr-silicon-args-that-need-args
  `(("assertionMode" . ,(vpr-args-mode-getter "Assertion mode: " '("cs" "pp")))
    ("assertTimeout" . ,(vpr-args-int-getter "Assert timeout (in ms): "))
    ("checkTimeout" . ,(vpr-args-int-getter "Z3 check timeout (in ms): "))
    ("counterexample" . ,(vpr-args-mode-getter "Counterexample mode: " '("native" "variables" "mapped")))
    ("excludeMethods" . (lambda () (read-string "Exclude methods: ")))
    ("includeMethods" . (lambda () (read-string "Include methods: ")))
    ("logConfig" . (lambda () (read-file-name "SymbExLogger config file: ")))
    ("logLevel" . ,(vpr-args-mode-getter "Log level: " '("ALL" "TRACE" "DEBUG" "INFO" "WARN" "ERROR" "OFF")))
    ("mapAxiomatizationFile" . (lambda () (read-file-name "Map axiomatization file: ")))
    ("maxHeuristicsDepth" . ,(vpr-args-int-getter "Max heuristics depth: "))
    ("multisetAxiomatizationFile" . (lambda () (read-file-name "Multiset axiomatization file: ")))
    ("numberOfErrorsToReport" . ,(vpr-args-int-getter "Number of errors (0 for all): "))
    ("numberOfParallelVerifiers" . ,(vpr-args-int-getter "Number of parallel verifiers: "))
    ("plugin" . (lambda () (read-string "Plugins (multiple seperated with ':': ")))
    ("qpSplitTimeout" . ,(vpr-args-int-getter "QP split timeout (in ms): "))
    ("recursivePredicateUnfoldings" . ,(vpr-args-int-getter "Number of unfoldings: "))
    ("sequenceAxiomatizationFile" . (lambda () (read-file-name "Sequence axiomatization file: ")))
    ("setAxiomatizationFile" . (lambda () (read-file-name "Set axiomatization file: ")))
    ("stateConsolidationMode" . ,(vpr-args-mode-getter "Assertion Mode: " '("0" "1" "2" "3" "4")))
    ("tempDirectory" . (lambda () (read-directory-name "Temp directory: ")))
    ("timeout" . ,(vpr-args-int-getter "Timeout (in s): "))
    ("z3Args" . ,(vpr-args-multi-string-getter "Z3 argument: " "Enter more arguments? "))
    ("z3ConfigArgs" . ,(vpr-args-multi-string-getter "Z3 config argument: " "Enter more arguments? "))
    ("z3Exe" . (lambda () (read-file-name "Z3 executable: ")))
    ("z3LogFile" . (lambda () (read-file-name "Z3 log file: ")))
    ("z3ResourcesPerMillisecond" . (lambda () (read-string "Resources per milliseconds: ")))
    ("z3SaturationTimeout" . ,(vpr-args-int-getter "Saturation timeout (in ms): "))
    ("z3SaturationTimeoutWeights" . ,(vpr-args-multi-string-getter "Saturation timeout weight: " "Enter more weights? ")))
  "Viper arguments that take arguments and functions to ask them from the user.")

(defvar vpr-silicon-args-that-need-many-args
  '("z3Args"
    "z3ConfigArgs"
    "z3SaturationTimeoutWeights")
  "Viper arguments that take arguments and functions to ask them from the user.")

;; arguments for carbon

(defvar vpr-carbon-args-doc
  '(("assumeInjectivityOnInhale" . "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it.")
    ("boogieExe" . "Manually-specified full path to Boogie.exe executable (default: ${BOOGIE_EXE})")
    ("boogieOpt" . "Option(s) to pass-through as options to Boogie (changing the output generated by Boogie is not supported) (default: none)")
    ("counterexample" . "Return counterexample for errors. Pass 'native' for returning the native model from the backend, 'variables' for returning a model of all local Viper variables, or 'mapped' (only available on Silicon) for returning a model with Ref variables resolved to object-like structures.")
    ("disableAllocEncoding" . "Disable Allocation-related assumptions (default: enabled)")
    ("plugin" . "Load plugin(s) with given class name(s). Several plugins can be separated by ':'. The fully qualified class name of the plugin should be specified.")
    ("print" . "Write the Boogie output file to the provided filename (default: none)")
    ("proverLog" . "Prover log file written by Boogie (default: none)")
    ("z3Exe" . "Manually-specified full path to Z3.exe executable (default: ${Z3_EXE})")
    ("help" . "Show help message"))
  "Documentation for carbon arguments.")

(defvar vpr-carbon-args-that-need-args
  `(("boogieExe" . (lambda () (read-file-name "Boogie executable: ")))
    ("boogieOpt" . (lambda () (read-string "Boogie options: ")))
    ("counterexample" . ,(vpr-args-mode-getter "Counterexample mode: " '("native" "variables" "mapped")))
    ("plugin" . (lambda () (read-string "Plugins (multiple seperated with ':': ")))
    ("print" . (lambda () (read-file-name "Filename: ")))
    ("proverLog" . (lambda () (read-file-name "Filename: ")))
    ("z3Exe" . (lambda () (read-file-name "Z3 executable: "))))
  "Viper arguments that take arguments and functions to ask them from the user.")

(defvar vpr-carbon-args-that-need-many-args nil
  "Viper arguments that take many arguments.")

;; argument checking and unchecking logic

(defun vpr-args-serialize ()
  "Return the arguments string."
  (let ((i vpr-args-set)
        (s ""))
    (while i
      (let ((cur (car i))
            (next (cdr i)))
        (setq s (format "%s --%s" s cur))
        (when (assoc cur vpr-args-that-need-args)
          (setq s (format "%s %s" s (cdr (assoc cur vpr-args-of-args)))))
        (setq i next)))
    s))

(defun vpr-dump (data filename)
  "Dump DATA in the file FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun vpr-load (filename)
  "Restore data from the file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (bobp))
    (read (current-buffer))))

(defun vpr-args-save ()
  "Save the current argument configuration to the disk."
  (interactive)
  (let ((f (read-file-name "File to save configuration: ")))
    (with-current-buffer vpr-args-original-buffer
      (vpr-dump (list vpr-backend (cons vpr-silicon-args-set vpr-silicon-args-of-args) (cons vpr-carbon-args-set vpr-carbon-args-of-args)) f))))

(defun vpr-args-load ()
  "Load an argument configuration from the disk."
  (interactive)
  (let* ((f (read-file-name "File name to load configuration: "))
         (data (vpr-load f))
         (backend (car data))
         (silicon-args (nth 1 data))
         (carbon-args (nth 2 data))
         (silicon-args-set (car silicon-args))
         (silicon-args-of-args (cdr silicon-args))
         (carbon-args-set (car carbon-args))
         (carbon-args-of-args (cdr carbon-args)))
    (with-current-buffer vpr-args-original-buffer
      (setq-local vpr-backend backend)
      (setq-local vpr-silicon-args-set silicon-args-set)
      (setq-local vpr-carbon-args-set carbon-args-set)
      (setq-local vpr-silicon-args-of-args silicon-args-of-args)
      (setq-local vpr-carbon-args-of-args carbon-args-of-args)
      (if (equal vpr-backend "silicon")
          (progn
            (setq-local vpr-args-set vpr-silicon-args-set)
            (setq-local vpr-args-of-args vpr-silicon-args-of-args)
            (setq-local vpr-args-doc vpr-silicon-args-doc)
            (setq-local vpr-args-that-need-args vpr-silicon-args-that-need-args)
            (setq-local vpr-args-that-need-many-args vpr-silicon-args-that-need-many-args))
        (setq-local vpr-args-set vpr-carbon-args-set)
        (setq-local vpr-args-of-args vpr-carbon-args-of-args)
        (setq-local vpr-args-doc vpr-carbon-args-doc)
        (setq-local vpr-args-that-need-args vpr-carbon-args-that-need-args)
        (setq-local vpr-args-that-need-many-args vpr-carbon-args-that-need-many-args))
      (vpr-edit-args))))

(defun vpr-populate-args-buffer ()
  "Insert the prelude and arguments wit their values so far in the current buffer."
  (setq-local buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min))
  (insert "Vpr argument selection buffer.\nCheck any argument needed with 'c'.\nAdd arguments to an argument with 'a'\nPrint documentation of argument with 'd'.\nSave configuraton with 's'.\nLoad configuration with 'l'.\nPress 'q' to exit.\n\n")
  (let ((start-pos (point)))
    (let ((i vpr-args-doc))
      (while i
        (let ((cur (car i))
              (next (cdr i)))
          (insert-char ?\[)
          (if (member (car cur) vpr-args-set)
              (insert (propertize "X" 'face 'vpr-verified-face))
            (insert-char ? ))
          (insert "] ")
          (insert (car cur))
          (when (and (member (car cur) vpr-args-set) (assoc (car cur) vpr-args-that-need-args))
            (insert (concat ": " (propertize (format "%s" (cdr (assoc (car cur) vpr-args-of-args))) 'face 'vpr-argument-face))))
          (insert-char ?\n)
          (setq i next))))
    (setq-local buffer-read-only t)
    (goto-char start-pos)))

(defun vpr-args-transfer ()
  "Transfer the change to the arguments at the main vpr buffer."
  (let ((args vpr-args-set)
        (args-of-args vpr-args-of-args))
    (with-current-buffer vpr-args-original-buffer
      (setq-local vpr-args-set args)
      (setq-local vpr-args-of-args args-of-args)
      (if (equal vpr-backend "carbon")
          (progn
            (setq-local vpr-carbon-args-set vpr-args-set)
            (setq-local vpr-carbon-args-of-args vpr-args-of-args))
        (setq-local vpr-silicon-args-set vpr-args-set)
        (setq-local vpr-silicon-args-of-args vpr-args-of-args)))))

(defun vpr-args-add-arg (arg)
  "Add argument ARG to the argument list."
  (when (not (member arg vpr-args-set))
    (setq-local vpr-args-set (cons arg vpr-args-set))
    (vpr-args-transfer)))

(defun vpr-args-region-after-colon ()
  "Return the beginning and and of the region after ':' in the construction buffer at the current line."
  (save-excursion
    (beginning-of-line)
    (if (equal (char-after) ?\[)
        (progn
          (forward-char 4)
          (let ((s (point)))
            (while (and (not (equal (char-after) ?\n)) (not (equal (char-after) ?:)) (not (eobp)))
              (forward-char))
            (when (equal (char-after) ?:)
              (let ((s1 (point)))
                (while (and (not (equal (char-after) ?\n)) (not (eobp)))
                  (forward-char))
                (cons s1 (point))))))
      nil)))

(defun vpr-args-remove-arg (arg)
  "Remove argument ARG from the argument list."
  (setq-local vpr-args-set (delete arg vpr-args-set))
  (setq-local vpr-args-of-args (assoc-delete-all arg vpr-args-of-args))
  (let ((r (vpr-args-region-after-colon)))
    (when r
      (delete-region (car r) (cdr r))))
  (vpr-args-transfer))

(defun vpr-args-get-arg ()
  "Return the argument text contained in a line of the args construction buffer."
  (save-excursion
    (beginning-of-line)
    (forward-char 4)
    (let ((s (point)))
      (while (and (not (equal (char-after) ?\n)) (not (equal (char-after) ?:)) (not (eobp)))
        (forward-char))
      (buffer-substring s (point)))))

(defun vpr-args-print-doc ()
  "Print the documentation of the argument under point."
  (interactive)
  (message "%s" (cdr (assoc (vpr-args-get-arg) vpr-args-doc 'equal))))

(defun vpr-args-check-uncheck-arg (&optional append)
  "Toggle the appearance of the argument in the current line of the construction buffer in the argument list.  When APPEND is set to t, args of args are appended to the current arg."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (equal (char-after) ?\[)
      (forward-char)
      (setq-local buffer-read-only nil)
      (if (or (eq (char-after) ? ) append)
          (progn
            (let ((sofar "")
                  (reg (vpr-args-region-after-colon)))
              (delete-char 1)
              (insert (propertize "X" 'face 'vpr-verified-face))
              (forward-char 2)
              (let ((arg (vpr-args-get-arg)))
                (when (and append reg (member arg vpr-args-that-need-many-args))
                  (setq sofar (buffer-substring (1+ (car reg)) (cdr reg))))
                (vpr-args-add-arg arg)
                (when (assoc arg vpr-args-that-need-args)
                  (let ((arg-of-arg (concat (format "%s" (funcall (cdr (assoc arg vpr-args-that-need-args)))) sofar)))
                    (setq-local vpr-args-of-args (cons (cons arg arg-of-arg) (assoc-delete-all arg vpr-args-of-args)))
                    (when reg
                      (delete-region (car reg) (cdr reg)))
                    (end-of-line)
                    (insert (concat ": " (propertize (format "%s" arg-of-arg) 'face 'vpr-argument-face)))))))
            (vpr-args-transfer))
        (delete-char 1)
        (insert-char ? )
        (vpr-args-remove-arg (vpr-args-get-arg))))
    (setq-local buffer-read-only t)))

(defun vpr-args-add-arg-of-arg ()
  "Toggle on the argument in this line and if it already has arguments, add to the existing ones."
  (interactive)
  (vpr-args-check-uncheck-arg t))

(defun vpr-args-quit ()
  "Quit the arguments construction buffer."
  (interactive)
  (let ((og vpr-args-original-buffer))
    (kill-buffer)
    (pop-to-buffer og)))

(defvar vpr-args-mode-map nil "Keymap for vpr-args.")

(when (not vpr-args-mode-map)
  (setq vpr-args-mode-map (make-sparse-keymap))
  (define-key vpr-args-mode-map (kbd "n") 'next-line)
  (define-key vpr-args-mode-map (kbd "p") 'previous-line)
  (define-key vpr-args-mode-map (kbd "c") 'vpr-args-check-uncheck-arg)
  (define-key vpr-args-mode-map (kbd "a") 'vpr-args-add-arg-of-arg)
  (define-key vpr-args-mode-map (kbd "d") 'vpr-args-print-doc)
  (define-key vpr-args-mode-map (kbd "q") 'vpr-args-quit)
  (define-key vpr-args-mode-map (kbd "s") 'vpr-args-save)
  (define-key vpr-args-mode-map (kbd "l") 'vpr-args-load))

(define-derived-mode vpr-args-mode fundamental-mode
  "vpr-args mode"
  "Major mode for selecting arguments passed to vpr in a construction buffer"
  (use-local-map vpr-args-mode-map)
  (read-only-mode t))

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
  (setq vpr-font-lock-keywords (vpr-build-keywords))
  (setq font-lock-defaults '((vpr-font-lock-keywords)))
  (setq-local indent-line-function #'vpr-indent-line)
  (setq-local prettify-symbols-alist vpr-pretty-symbols)
  (prettify-symbols-mode)
  (setq comment-start "//")
  (setq comment-end "")
  (cursor-sensor-mode)
  (setq global-mode-string (or global-mode-string '("")))
  (vpr-args-initialize)
  (unless (member '(:eval (vpr-mode-line)) global-mode-string)
    (setq global-mode-string (append global-mode-string '((:eval (vpr-mode-line)))))))

(add-to-list 'auto-mode-alist '("\\.vpr" . vpr-mode))

(provide 'vpr-mode)
;;; vpr-mode.el ends here
