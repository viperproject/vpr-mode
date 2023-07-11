;;; vpr-mode.el --- Viper backend configuration -*- lexical-binding: t -*-

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

;; requirements

(require 'param-config)

;; variables

(defvar-local vpr-backend "silicon" "The backend that should be used by Viper.")
(defvar-local vpr-carbon-options "--boogieExe=%s" "The carbon backend option that Viper should use.")
(defvar vpr-z3-path nil "The location of Z3.")
(defvar vpr-boogie-path nil "The location of Boogie.")

(defvar-local vpr-mode-silicon-config '())
(defvar-local vpr-mode-carbon-config `(("boogieExe" . (list vpr-boogie-path))))

(defvar-local vpr-mode-silicon-z3-args '())


(defun vpr-args-initialize ()
  "Initialize all variables having something to do with arguments."
  (setq-local vpr-carbon-args-set '("boogieExe"))
  (setq-local vpr-carbon-args-of-args ))

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

(defvar vpr-silicon-params
  `(,(make-param-config-param
      :name "alternativeFunctionVerificationOrder"
      :doc "Calculate the order in which functions are verified and function axioms become available in an alternative way that takes dependencies between functions through predicate unfoldings into account. This is more complete in some cases (see Silicon issue #355) but less complete in others (see test all/issues/silicon/unofficial007).")
    ,(make-param-config-param
      :name "assertionMode"
      :doc "Determines how assertion checks are encoded in SMTLIB. Options are 'pp' (push-pop) and 'cs' (soft constraints) (default: cs)."
      :args t
      :getter ,(vpr-args-mode-getter "Assertion mode: " '("cs" "pp")))
    ,(make-param-config-param
      :name "assertTimeout"
      :doc "Timeout (in ms) per SMT solver assertion (default: 0, i.e. no timeout)."
      :args t
      :getter ,(vpr-args-int-getter "Assert timeout (in ms): "))
    ,(make-param-config-param
      :name "assumeInjectivityOnInhale"
      :doc "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it.")
    ,(make-param-config-param
      :name "checkTimeout"
      :doc "Timeout (in ms) per SMT solver check. Solver checks differ from solver asserts in that a failing assert always yields a verification error whereas a failing check doesn't, at least not directly. However, failing checks might result in performance degradation, e.g. when a dead program path is nevertheless explored, and indirectly in verification failures due to incompletenesses, e.g. when the held permission amount is too coarsely underapproximated (default: 10)."
      :args t
      :getter ,(vpr-args-int-getter "Z3 check timeout (in ms): "))
    ,(make-param-config-param
      :name "conditionalizePermissions"
      :doc "Potentially reduces the number of symbolic execution paths, by conditionalising permission expressions. E.g. rewrite \"b ==> acc(x.f, p)\" to \"acc(x.f, b ? p : none)\".This is an experimental feature; report problems if you observe any.")
    ,(make-param-config-param
      :name "counterexample"
      :doc "Return counterexample for errors. Pass 'native' for returning the native model from the backend, 'variables' for returning a model of all local Viper variables, or 'mapped' (only available on Silicon) for returning a model with Ref variables resolved to object-like structures."
      :args t
      :getter ,(vpr-args-mode-getter "Counterexample mode: " '("native" "variables" "mapped")))
    ,(make-param-config-param
      :name "disableCaches"
      :doc "Disables various caches in Silicon's state.")
    ,(make-param-config-param
      :name "disableCatchingExceptions"
      :doc "Don't catch exceptions (can be useful for debugging problems with Silicon)")
    ,(make-param-config-param
      :name "disableChunkOrderHeuristics"
      :doc "Disable heuristic ordering of quantified chunks (context: iterated separating conjunctions).")
    ,(make-param-config-param
      :name "disableHavocHack407"
      :doc "A Viper method call to ___silicon_hack407_havoc_all_R, where R is a field or predicate, results in Silicon havocking all instances of R. See also Silicon issue #407.")
    ,(make-param-config-param
      :name "disableISCTriggers"
      :doc "Don't pick triggers for quantifiers, let the SMT solver do it (context: iterated separating conjunctions).")
    ,(make-param-config-param
      :name "disableShortCircuitingEvaluations"
      :doc "Disable short-circuiting evaluation of AND, OR. If disabled, evaluating e.g., i > 0 && f(i), will fail if f's precondition requires i > 0.")
    ,(make-param-config-param
      :name "disableSubsumption"
      :doc "Don't add assumptions gained by verifying an assert statement")
    ,(make-param-config-param
      :name "disableValueMapCaching"
      :doc "Disable caching of value maps (context: iterated separating conjunctions).")
    ,(make-param-config-param
      :name "enableBranchconditionReporting"
      :doc "Report branch conditions (can be useful for assertions that fail on multiple branches)")
    ,(make-param-config-param
      :name "enableMoreCompleteExhale"
      :doc "Enable a more complete exhale version.")
    ,(make-param-config-param
      :name "enablePredicateTriggersOnInhale"
      :doc "Emit predicate-based function trigger on each inhale of a predicate instance (context: heap-dependent functions).")
    ,(make-param-config-param
      :name "enableTempDirectory"
      :doc "Enable the creation of temporary directory to log prover interactions (default: ./tmp)")
    ,(make-param-config-param
      :name "excludeMethods"
      :doc "Exclude methods from verification (default: ''). Is applied after the include pattern."
      :args t
      :getter (lambda () (read-string "Exclude methods: ")))
    ,(make-param-config-param
      :name "exhaleMode"
      :doc "Exhale mode. Options are 0 (greedy, default), 1 (more complete exhale), 2 (more complete exhale on demand)."
      :args ,(vpr-args-mode-getter "Exhale mode" '("0" "1" "2")))
    ,(make-param-config-param
      :name "handlePureConjunctsIndividually"
      :doc "Handle pure conjunction individually.Increases precision of error reporting, but may slow down verification.")
    ,(make-param-config-param
      :name "includeMethods"
      :doc "Include methods in verification (default: '*'). Wildcard characters are '?' and '*'."
      :args t
      :getter (lambda () (read-string "Include methods: ")))
    ,(make-param-config-param
      :name "logConfig"
      :doc "Path to config file specifying SymbExLogger options"
      :args t
      :getter (lambda () (read-file-name "SymbExLogger config file: ")))
    ,(make-param-config-param
      :name "logLevel"
      :doc "One of the log levels ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF"
      :args t
      :getter ,(vpr-args-mode-getter "Log level: " '("ALL" "TRACE" "DEBUG" "INFO" "WARN" "ERROR" "OFF")))
    ,(make-param-config-param
      :name "mapAxiomatizationFile"
      :doc "Source file with map axiomatisation. If omitted, built-in one is used."
      :args t
      :getter (lambda () (read-file-name "Map axiomatization file: ")))
    ,(make-param-config-param
      :name "maxHeuristicsDepth"
      :doc "Maximal number of nested heuristics applications (default: 3)"
      :args t
      :getter ,(vpr-args-int-getter "Max heuristics depth: "))
    ,(make-param-config-param
      :name "multisetAxiomatizationFile"
      :doc "Source file with multiset axiomatisation. If omitted, built-in one is used."
      :args t
      :getter (lambda () (read-file-name "Multiset axiomatization file: ")))
    ,(make-param-config-param
      :name "numberOfErrorsToReport"
      :doc "Number of errors per member before the verifier stops. If this number is set to 0, all errors are reported."
      :args t
      :getter ,(vpr-args-int-getter "Number of errors (0 for all): "))
    ,(make-param-config-param
      :name "numberOfParallelVerifiers"
      :doc "Number of verifiers run in parallel. This number plus one is the number of provers run in parallel (default: 12)"
      :args t
      :getter ,(vpr-args-int-getter "Number of parallel verifiers: "))
    ,(make-param-config-param
      :name "plugin"
      :doc "Load plugin(s) with given class name(s). Several plugins can be separated by ':'. The fully qualified class name of the plugin should be specified."
      :args t
      :getter (lambda () (read-string "Plugins (multiple seperated with ':': ")))
    ,(make-param-config-param
      :name "printMethodCFGs"
      :doc "Print a DOT (Graphviz) representation of the CFG of each method to verify to a file '<tempDirectory>/<methodName>.dot'.")
    ,(make-param-config-param
      :name "printTranslatedProgram"
      :doc "Print the final program that is going to be verified to stdout.")
    ,(make-param-config-param
      :name "qpSplitTimeout"
      :doc "Timeout (in ms) used by QP's split algorithm when 1) checking if a chunk holds no further permissions, and 2) checking if sufficiently many permissions have already been split off."
      :args t
      :getter ,(vpr-args-int-getter "QP split timeout (in ms): "))
    ,(make-param-config-param
      :name "recursivePredicateUnfoldings"
      :doc "Evaluate n unfolding expressions in the body of predicates that (transitively) unfold other instances of themselves (default: 1)"
      :args t
      :getter ,(vpr-args-int-getter "Number of unfoldings: "))
    ,(make-param-config-param
      :name "sequenceAxiomatizationFile"
      :doc "Source file with sequence axiomatisation. If omitted, built-in one is used."
      :args t
      :getter (lambda () (read-file-name "Sequence axiomatization file: ")))
    ,(make-param-config-param
      :name "setAxiomatizationFile"
      :doc "Source file with set axiomatisation. If omitted, built-in one is used."
      :args t
      :getter (lambda () (read-file-name "Set axiomatization file: ")))
    ,(make-param-config-param
      :name "stateConsolidationMode"
      :doc "One of the following modes: 0: Minimal work, many incompletenesses 1: Most work, fewest incompletenesses 2: Similar to 1, but less eager 3: Less eager and less complete than 1 4: Intended for use with moreCompleteExhale"
      :args t
      :getter ,(vpr-args-mode-getter "Assertion Mode: " '("0" "1" "2" "3" "4")))
    ,(make-param-config-param
      :name "tempDirectory"
      :doc "Path to which all temporary data will be written (default: ./tmp)"
      :args t
      :getter (lambda () (read-directory-name "Temp directory: ")))
    ,(make-param-config-param
      :name "timeout"
      :doc "Time out after approx. n seconds. The timeout is for the whole verification, not per method or proof obligation (default: 0, i.e. no timeout)."
      :args t
      :getter ,(vpr-args-int-getter "Timeout (in s): "))
    ,(make-param-config-param
      :name "z3Args"
      :doc "Command-line arguments which should be forwarded to Z3. The expected format is \"<opt> <opt> ... <opt>\", excluding the quotation marks."
      :args t
      :ignore-getter t
      :getter (lambda ()
                (param-config-edit-params backend-params 'backend-args
                                          (lambda ()
                                            (param-config-put-param "backend-args" backend-args)
                                            (param-config-populate-buffer)
                                            '()))))
    ,(make-param-config-param
      :name "z3ConfigArgs"
      :doc "Configuration options which should be forwarded to Z3. The expected format is \"<key>=<val> <key>=<val> ... <key>=<val>\", excluding the quotation marks. The configuration options given here will override those from Silicon's Z3 preamble."
      :args t
      :getter ,(vpr-args-multi-string-getter "Z3 config argument: " "Enter more arguments? ")
      :repeating t)
    ,(make-param-config-param
      :name "z3EnableResourceBounds"
      :doc "Use Z3's resource bounds instead of timeouts")
    ,(make-param-config-param
      :name "z3Exe"
      :doc "Z3 executable. The environment variable Z3_EXE can also be used to specify the path of the executable."
      :args t
      :getter (lambda () (read-file-name "Z3 executable: ")))
    ,(make-param-config-param
      :name "z3LogFile"
      :doc "Log file containing the interaction with Z3, extension smt2 will be appended. (default: <tempDirectory>/logfile.smt2)"
      :args t
      :getter (lambda () (read-file-name "Z3 log file: ")))
    ,(make-param-config-param
      :name "z3RandomizeSeeds"
      :doc "Set various Z3 random seeds to random values")
    ,(make-param-config-param
      :name "z3ResourcesPerMillisecond"
      :doc "Z3 resources per milliseconds. Is used to convert timeouts to resource bounds."
      :args t
      :getter (lambda () (read-string "Resources per milliseconds: ")))
    ,(make-param-config-param
      :name "z3SaturationTimeout"
      :doc "Timeout (in ms) used for Z3 state saturation calls (default: 100). A timeout of 0 disables all saturation checks."
      :args t
      :getter ,(vpr-args-int-getter "Saturation timeout (in ms): "))
    ,(make-param-config-param
      :name "z3SaturationTimeoutWeights"
      :doc "Weights used to compute the effective timeout for Z3 state saturation calls, which are made at various points during a symbolic execution. The effective timeouts for a particular saturation call is computed by multiplying the corresponding weight with the base timeout for saturation calls. Defaults to the following weights: after program preamble: 1.0 after inhaling contracts: 0.5 after unfold: 0.4 after inhale: 0.2 before repeated Z3 queries: 0.02 Weights must be non-negative, a weight of 0 disables the corresponding saturation call and a minimal timeout of 10ms is enforced."
      :args t
      :getter ,(vpr-args-multi-string-getter "Saturation timeout weight: " "Enter more weights? ")
      :repeating t)
    ,(make-param-config-param
      :name "help"
      :doc "Show help message")))

;; arguments for carbon

(defvar vpr-carbon-params
 `(,(make-param-config-param
     :name "assumeInjectivityOnInhale"
     :doc "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it.")
    ,(make-param-config-param
      :name "boogieExe"
      :doc "Manually-specified full path to Boogie.exe executable (default: ${BOOGIE_EXE})"
      :args t
      :getter (lambda () (read-file-name "Boogie executable: ")))
    ,(make-param-config-param
      :name "boogieOpt"
      :doc "Option(s) to pass-through as options to Boogie (changing the output generated by Boogie is not supported) (default: none)"
      :args t
      :getter (lambda () (read-string "Boogie options: ")))
    ,(make-param-config-param
      :name "counterexample"
      :doc "Return counterexample for errors. Pass 'native' for returning the native model from the backend, 'variables' for returning a model of all local Viper variables, or 'mapped' (only available on Silicon) for returning a model with Ref variables resolved to object-like structures."
      :args t
      :getter ,(vpr-args-mode-getter "Counterexample mode: " '("native" "variables" "mapped")))
    ,(make-param-config-param
      :name "disableAllocEncoding"
      :doc "Disable Allocation-related assumptions (default: enabled)")
    ,(make-param-config-param
      :name "plugin"
      :doc "Load plugin(s) with given class name(s). Several plugins can be separated by ':'. The fully qualified class name of the plugin should be specified."
      :args t
      :getter (lambda () (read-string "Plugins (multiple seperated with ':': ")))
    ,(make-param-config-param
      :name "print"
      :doc "Write the Boogie output file to the provided filename (default: none)"
      :args t
      :getter (lambda () (read-file-name "Filename: ")))
    ,(make-param-config-param
      :name "proverLog"
      :doc "Prover log file written by Boogie (default: none)"
      :args t
      :getter (lambda () (read-file-name "Filename: ")))
    ,(make-param-config-param
      :name "z3Exe"
      :doc "Manually-specified full path to Z3.exe executable (default: ${Z3_EXE})"
      :args t
      :getter (lambda () (read-file-name "Z3 executable: ")))
    ,(make-param-config-param
      :name "help"
      :doc "Show help message")))

