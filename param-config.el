;;; param-config.el --- Easily manipulate sets of parameters -*- lexical-binding: t -*-

;; Copyright (c) 2022- ETH Zurich.

;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/viperproject/gobra-mode
;; Package-Requires: ((emacs "26.2"))

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; Makes it easy to create sets of arguments for various uses and
;; enable/disable them, or edit their arguments. Ideal for manipulating
;; arguments of executables with complex configurations.

;;; Code:

;; Variables
(defvar-local param-config-original-buffer nil "Holds the buffer from which the arguments construction buffer was spawned.")
(defvar-local param-config-params nil "Holds the parameters that should be manipulated in the current buffer")
(defvar-local param-config-curr-config nil "Holds the current configuration of parameters as a property list with the parameter names as keys and a list of their arguments (possibly empty) as values.")

;; Faces
(defface param-config-checked-face
  '((t (:weight bold :foreground "Green")))
  "The face used to highlight checked arguments.")

(defface param-config-argument-face
  '((t (:foreground "Grey")))
  "The face used to distinguish arguments from parameters in the parameters construction buffer.")

;; Param struct
(cl-defstruct param-config-param
  "Struct defining a parameter. The fields are:
name: The name of the parameter.
doc: The documentation of the parameter.
args: t if the parameter accepts arguments, else nil.
getter: If the parameter accepts an argument, the getter will be called to set that argument.
serializer: Function to be called to serialize an argument of this parameter. If nil, a simple comma seperated concatenation will be used.
repeating: If non-nil, the parameter accepts multiple arguments, each fetched with the getter.
ignore-getter: If t, the getter will be called but its return value will be ignored."
  name
  doc
  args
  getter
  serializer
  repeating
  ignore-getter)

;; Temporary
(defvar-local backend-args '())
(setq-local curr-config '())

(defvar backend-params
            `(,(make-param-config-param
                :name "assumeInjectivityOnInhale"
                :doc "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it, like in Viper versions previous to 2022.02 (default)")
              ,(make-param-config-param
                :name "noassumeInjectivityOnInhale"
                :doc "Does not assume injectivity on inhales (this will become the default in future versions)")
              ,(make-param-config-param
                :name "backend"
                :doc "Needs <arg>. Specifies the used Viper backend. The default is SILICON. Choices: SILICON, CARBON, VSWITHSILICON, VSWITHCARBON"
                :args t
                :getter (lambda ()
                          (completing-read "Backend: "
                                           '("SILICON" "CARBON" "VSWITHSILICON" "VSWITHCARBON") nil t)))))
(setq-local params
            `(,(make-param-config-param
                :name "assumeInjectivityOnInhale"
                :doc "Assumes injectivity of the receiver expression when inhaling quantified permissions, instead of checking it, like in Viper versions previous to 2022.02 (default)")
              ,(make-param-config-param
                :name "noassumeInjectivityOnInhale"
                :doc "Does not assume injectivity on inhales (this will become the default in future versions)")
              ,(make-param-config-param
                :name "backend"
                :doc "Needs <arg>. Specifies the used Viper backend. The default is SILICON. Choices: SILICON, CARBON, VSWITHSILICON, VSWITHCARBON"
                :args t
                :getter (lambda ()
                          (completing-read "Backend: "
                                           '("SILICON" "CARBON" "VSWITHSILICON" "VSWITHCARBON") nil t)))
              ,(make-param-config-param
                :name "boogieExe"
                :doc "Needs <arg>. The Boogie executable"
                :args t
                :getter (lambda ()
                          (read-file-name "Boogie Exe path: ")))
              ,(make-param-config-param
                :name "cacheFile"
                :doc "Needs <arg>. Cache file to be used by Viper Server"
                :args t
                :getter (lambda ()
                          (read-file-name "Cache file: ")))
              ,(make-param-config-param
                :name "checkConsistency"
                :doc "Perform consistency checks on the generated Viper code")
              ,(make-param-config-param
                :name "chop"
                :doc "Needs <arg>. Number of parts the generated verification condition is split into (at most)"
                :args t
                :getter (lambda ()
                          (call-interactively
                           (lambda (arg)
                             "dummy docstring"
                             (interactive "nNumber of parts: ")
                             arg))))
              ,(make-param-config-param
                :name "debug"
                :doc "Output additional debug information")
              ,(make-param-config-param
                :name "disableMoreCompleteExhale"
                :doc "Disables the flag --enableMoreCompleteExhale passed by default to Silicon")
              ,(make-param-config-param
                :name "enableLazyImport"
                :doc "Enforces that Gobra parses depending packages only when necessary. Note that this disables certain language features such as global variables.")
              ,(make-param-config-param
                :name "directory"
                :doc "Needs <arg...>. List of directories to verify"
                :args t
                :getter (lambda ()
                          (read-directory-name "Directory: "))
                :repeating t)
              ,(make-param-config-param
                :name "eraseGhost"
                :doc "Print the input program without ghost code")
              ,(make-param-config-param
                :name "excludePackages"
                :doc "Needs <arg...>. Packages to ignore. These packages will not be verified, even if they are found in the specified directories."
                :args t
                :getter (lambda ()
                          (read-string "Package: "))
                :repeating t)
              ,(make-param-config-param
                :name "gobraDirectory"
                :doc "Needs <arg>. Output directory for Gobra"
                :args t
                :getter (lambda ()
                          (read-directory-name "Gobra output directory: ")))
              ,(make-param-config-param
                :name "goify"
                :doc "Print the input program with the ghost code commented out")
              ,(make-param-config-param
                :name "include"
                :doc "Needs <arg...>. Uses the provided directories to perform package-related lookups before falling back to $GOPATH"
                :args t
                :getter (lambda ()
                          (read-directory-name "Directory: "))
                :repeating t)
              ,(make-param-config-param
                :name "includePackages"
                :doc "Needs <arg...>. Packages to verify. All packages found in the specified directories are verified by default."
                :args t
                :getter (lambda ()
                          (read-string "Package: "))
                :repeating t)
              ,(make-param-config-param
                :name "input"
                :doc "Needs <arg...>. List of files to verify. Optionally, specific members can be verified by passing their line numbers (e.g. foo.gobra@42,111 corresponds to the members in lines 42 and 111) "
                :args t
                :getter (lambda ()
                          (let ((s (read-file-name "File: ")))
                            (while (y-or-n-p "Enter more files? ")
                              (setq s (concat s " " (read-file-name "File: "))))
                            s))
                :repeating t)
              ,(make-param-config-param
                :name "int32"
                :doc "Run with 32-bit sized integers (the default is 64-bit ints)")
              ,(make-param-config-param
                :name "logLevel"
                :doc "Needs <arg>. Specifies the log level. The default is OFF. Choices: ALL, TRACE, DEBUG, INFO, WARN, ERROR, OFF"
                :args t
                :getter (lambda ()
                          (completing-read "Log level: "
                                           '("ALL" "TRACE" "DEBUG" "INFO" "WARN" "ERROR" "OFF") nil t)))
              ,(make-param-config-param
                :name "module"
                :doc "Needs <arg>. Name of current module that should be used for resolving imports"
                :args t
                :getter (lambda ()
                          (read-string "Module: ")))
              ,(make-param-config-param
                :name "noStreamErrors"
                :doc "Do not stream errors produced by Gobra but instead print them all organized by package in the end.")
              ,(make-param-config-param
                :name "noVerify"
                :doc "Skip the verification step performed after encoding the Gobra program into Viper.")
              ,(make-param-config-param
                :name "onlyFilesWithHeader"
                :doc "When enabled, Gobra only looks at files that contain the header comment '// +gobra'")
              ,(make-param-config-param
                :name "overflow"
                :doc "Find expressions that may lead to integer overflow")
              ,(make-param-config-param
                :name "packageTimeout"
                :doc "Needs <arg>. Duration till the verification of a package times out"
                :args t
                :getter (lambda ()
                          (call-interactively
                           (lambda (arg)
                             "dummy docstring"
                             (interactive "sPackage timeout: ") arg))))
              ,(make-param-config-param
                :name "parallelizeBranches"
                :doc "Performs parallel branch verification if the chosen backend is either SILICON or VSWITHSILICON")
              ,(make-param-config-param
                :name "parseOnly"
                :doc "Perform only the parsing step")
              ,(make-param-config-param
                :name "printInternal"
                :doc "Print the internal program representation")
              ,(make-param-config-param
                :name "printVpr"
                :doc "Print the encoded Viper program")
              ,(make-param-config-param
                :name "projectRoot"
                :doc "Needs <arg>. The root directory of the project"
                :args t
                :getter (lambda ()
                          (read-file-name "Project root: ")))
              ,(make-param-config-param
                :name "recursive"
                :doc "Verify nested packages recursively")
              ,(make-param-config-param
                :name "unparse"
                :doc "Print the parsed program")
              ,(make-param-config-param
                :name "z3Exe"
                :doc "Needs <arg>. The Z3 executable"
                :args t
                :getter (lambda ()
                          (read-file-name "Z3 Exe path: ")))
              ,(make-param-config-param
                :name "help"
                :doc "Show help message")
              ,(make-param-config-param
                :name "version"
                :doc "Show version of this program")
              ,(make-param-config-param
                :name "backend-args"
                :doc "add backend args"
                :args t
                :ignore-getter t
                :getter (lambda ()
                          (param-config-edit-params backend-params 'backend-args
                                                    (lambda ()
                                                      (param-config-put-param "backend-args" backend-args)
                                                      (param-config-populate-buffer)
                                                      '())))
                :serializer (lambda (args)
                              (string-trim-left
                               (cl-reduce (lambda (sofar arg)
                                            (format "%s --%s%s"
                                                    sofar
                                                    (car arg)
                                                    (if (cdr arg)
                                                        (format " %s" (cdr arg))
                                                      "")))
                                          args
                                          :initial-value ""))))))

;; General helper functions

(defun param-config-file-dump (data filename)
  "Dump DATA in the file FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun param-config-file-load (filename)
  "Restore data from the file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (bobp))
    (read (current-buffer))))

(defun param-config-del-nth-helper (lst place curr-place acc)
  (if lst
      (if (equal curr-place place)
          (param-config-del-nth-helper (cdr lst) place (1+ curr-place) acc)
        (param-config-del-nth-helper (cdr lst) place (1+ curr-place) (cons (car lst) acc)))
    acc))

(defun param-config-del-nth (lst place)
  (reverse (param-config-del-nth-helper lst place 0 nil)))

;; Helper functions for param manipulation

(defun param-config-put-param (param &optional arg repeating)
  "Add PARAM to the param list with its optional ARG."
  (let ((params (if (assoc param param-config-curr-config)
                    (assoc-delete-all param param-config-curr-config)
                  param-config-curr-config)))
    (if repeating
        (setq-local param-config-curr-config (cons (cons param arg) params))
      (setq-local param-config-curr-config (cons (cons param (when arg (list arg))) params)))))

(defun param-config-add-param-arg (param arg &optional repeating)
  (let* ((param-exists (assoc param param-config-curr-config))
         (params (if param-exists
                     (assoc-delete-all param param-config-curr-config)
                   param-config-curr-config))
         (args (if param-exists
                   (cdr param-exists)
                 '())))
    (if repeating
        (setq-local param-config-curr-config (cons (cons param (append arg args)) params))
      (setq-local param-config-curr-config (cons (cons param (cons arg args)) params)))))

(defun param-config-del-param (param)
  "Remove parameter PARAM from the parameter list."
  (setq-local param-config-curr-config (assoc-delete-all param param-config-curr-config)))

(defun param-config-del-arg-of-param (param place)
  "Remove argument with index PLACE from the argument list of parameter PARAM."
  (let* ((param-exists (assoc param param-config-curr-config))
         (params (if param-exists
                     (assoc-delete-all param param-config-curr-config)
                   param-config-curr-config))
         (args (if param-exists
                   (cdr param-exists)
                 '())))
    (if (length= args 1)
        (setq-local param-config-curr-config params)
        (setq-local param-config-curr-config (cons (cons param (param-config-del-nth args place)) params)))))

(defun param-config-get-param-helper (name params)
  (when params
    (if (equal name (param-config-param-name (car params)))
        (car params)
      (param-config-get-param-helper name (cdr params)))))

(defun param-config-get-param (name)
  "Return the parameter struct of parameter with name NAME."
  (param-config-get-param-helper name param-config-params))

(defun param-config-repeating-getter (param getter)
  (if (param-config-param-repeating param)
      (lambda ()
        (let ((s (list (funcall getter))))
          (while (y-or-n-p "Enter more values? ")
            (setq s (cons (funcall getter) s)))
          (reverse s)))
    getter))

(defun param-config-get-param-getter (param)
  (let ((getter (param-config-param-getter param)))
    (if getter
        (param-config-repeating-getter param getter)
      (param-config-repeating-getter param
                                     (lambda ()
                                       (call-interactively
                                        (lambda (arg)
                                          "dummy docstring"
                                          (interactive "sValue: ")
                                          arg)))))))

(defun param-config-get-param-serializer (param)
  (let ((serializer (param-config-param-serializer param)))
    (if serializer
        serializer
      (lambda (arg) (format "%s" arg)))))

(defun param-config-get-param-name-from-buffer ()
  "Return the argument text contained in a line of the args construction buffer."
  (save-excursion
    (beginning-of-line)
    (let ((c (char-after)))
      (if c
          (if (eq c ?\[)
              (progn
                (forward-char 4)
                (let ((s (point)))
                  (while (and (not (equal (char-after) ?\n)) (not (equal (char-after) ?:)) (not (eobp)))
                    (forward-char))
                  (buffer-substring s (point))))
            nil)
        nil))))

(defun param-config-prev-param ()
  (save-excursion
    (beginning-of-line)
    (let ((counter 0)
          param)
      (while (and (not (bobp)) (not param))
        (let ((name (param-config-get-param-name-from-buffer)))
          (when name
            (setq param name)))
        (previous-line)
        (setq counter (1+ counter)))
      (cons param (1- counter)))))

;; Helper functions for construction buffer formatting

(defun param-config-populate-buffer ()
  "Insert the prelude and parameters with their values so far in the current buffer."
  (setq-local buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min))
  (insert "Parameter selection buffer.\nEnable any parameter needed with 'c'.\nAdd arguments to an enabled paremeter with 'a'\nDelete an argument of a parameter with 'd'\nPrint documentation of argument with '?'.\nSave configuraton with 's'.\nLoad configuration with 'l'.\nPress 'q' to exit.\n\n")
  (let ((start-pos (point)))
    (let ((i param-config-params))
      (while i
        (let* ((cur (car i))
               (cur-in-config (assoc (param-config-param-name cur) param-config-curr-config))
               (next (cdr i)))
          (insert-char ?\[)
          (if cur-in-config
              (insert (propertize "X" 'face 'param-config-checked-face))
            (insert-char ? ))
          (insert "] ")
          (insert (param-config-param-name cur))
          (when (and cur-in-config (cdr cur-in-config))
            (let ((args (cdr cur-in-config)))
              (when args
                (insert ": ")
                (let ((col (current-column)))
                  (while args
                    (insert (funcall (param-config-get-param-serializer cur) (car args)))
                    (setq args (cdr args))
                    (when args
                      (insert (format "\n%s" (make-string col ? )))))))))
          (insert-char ?\n)
          (setq i next))))
    (setq-local buffer-read-only t)
    (goto-char start-pos)))

(defun param-config-find-param-line (param)
  "Find the line where PARAM is on."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil)
          (line nil))
      (while (and (not found) (not (eobp)))
        (let ((fparam (param-config-get-param-name-from-buffer)))
          (when fparam
            (when (equal param fparam)
              (setq line (line-number-at-pos))
              (setq found t)))
          (forward-line)))
      line)))

;; Main function

(defun param-config-transfer-params ()
  (let ((curr-config param-config-curr-config)
        (curr-params param-config-param-symbol)
        (sideffect (or param-config-transfer-sideffect (lambda () nil))))
    (with-current-buffer param-config-original-buffer
      (set curr-params curr-config)
      (funcall sideffect))))

(defun param-config-edit-params (params curr-params &optional sideffect)
  "Spawn the construction buffer for the arguments."
  (let ((cur-buf-name (buffer-name))
        (cur-buf (current-buffer))
        (arg-buf (format "%s%s" (current-buffer) "~args"))
        (curr-config (symbol-value curr-params)))
    (with-current-buffer (get-buffer-create arg-buf)
      (param-config-mode)
      (setq-local param-config-original-buffer cur-buf)
      (setq-local param-config-curr-config curr-config)
      (setq-local param-config-params params)
      (setq-local param-config-transfer-sideffect sideffect)
      (setq-local param-config-param-symbol curr-params)
      (param-config-populate-buffer))
    (pop-to-buffer arg-buf)
    nil))

;; Interactive commands for construction buffer

(defun param-config-save ()
  "Save the current parameter configuration to the disk."
  (interactive)
  (let ((f (read-file-name "File to save configuration: ")))
    (param-config-file-dump param-config-curr-config f)))

(defun param-config-load ()
  "Load an argument configuration from the disk."
  (interactive)
  (let* ((f (read-file-name "File name to load configuration: "))
         (data (param-config-file-load f)))
    (setq-local param-config-curr-config data)
    (param-config-populate-buffer))
  (param-config-transfer-params))

(defun param-config-print-doc ()
  "Print the documentation of the argument under point."
  (interactive)
  (let* ((data (param-config-prev-param))
         (param (car data)))
    (when param
      (message "%s" (param-config-param-doc (param-config-get-param param))))))

(defun param-config-check-uncheck-arg (&optional append)
  "Toggle the appearance of the argument in the current line of the construction buffer in the argument list.  When APPEND is set to t, args of args are appended to the current arg."
  (interactive)
  (let ((pos (point)))
    (let* ((param-data (param-config-prev-param))
           (param (car param-data))
           (param-struct (param-config-get-param param))
           (line (when param (param-config-find-param-line param))))
      (when param
        (if (or (not (assoc param param-config-curr-config)) append)
            (progn
              (when param-struct
                (if (param-config-param-args param-struct)
                    (let ((args (funcall (param-config-get-param-getter param-struct))))
                      (if append
                          (param-config-add-param-arg param args (param-config-param-repeating param-struct))
                        (when (not (param-config-param-ignore-getter param-struct))
                          (param-config-put-param param args (param-config-param-repeating param-struct)))))
                  (when (not (param-config-param-ignore-getter param-struct))
                    (param-config-put-param param)))))
          (param-config-del-param param))
        (param-config-populate-buffer)
        (goto-char (point-min))
        (forward-line (1- line))))
    (param-config-transfer-params)))

(defun param-config-add-arg-of-current-param ()
  "Toggle on the argument in this line and if it already has arguments, add to the existing ones."
  (interactive)
  (param-config-check-uncheck-arg t))

(defun param-config-del-arg-of-current-param ()
  "Remove the argument in the current line from the arguments of the parameter that it belongs to."
  (interactive)
  (let* ((data (param-config-prev-param))
         (param (car data))
         (place (cdr data))
         (line (line-number-at-pos)))
    (when param
      (param-config-del-arg-of-param param place)
      (param-config-populate-buffer)
      (goto-char (point-min))
      (forward-line (1- line))))
  (param-config-transfer-params))

(defun param-config-jump-to-param ()
  "Jump to a specific parameter in the construction buffer"
  (interactive)
  (let* ((param (completing-read "Parameter: " (cl-map 'list (lambda (param) (param-config-param-name param)) param-config-params) nil t))
         (line (param-config-find-param-line param)))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun param-config-quit ()
  "Quit the arguments construction buffer."
  (interactive)
  (let ((og param-config-original-buffer))
    (param-config-transfer-params)
    (kill-buffer)
    (pop-to-buffer og)))

;; Param buffer keymap keymap

(defvar param-config-mode-map nil "Keymap for param-config.")

(when (not param-config-mode-map)
  (setq param-config-mode-map (make-sparse-keymap))
  (define-key param-config-mode-map (kbd "n") 'next-line)
  (define-key param-config-mode-map (kbd "p") 'previous-line)
  (define-key param-config-mode-map (kbd "c") 'param-config-check-uncheck-arg)
  (define-key param-config-mode-map (kbd "a") 'param-config-add-arg-of-current-param)
  (define-key param-config-mode-map (kbd "?") 'param-config-print-doc)
  (define-key param-config-mode-map (kbd "d") 'param-config-del-arg-of-current-param)
  (define-key param-config-mode-map (kbd "j") 'param-config-jump-to-param)
  (define-key param-config-mode-map (kbd "q") 'param-config-quit)
  (define-key param-config-mode-map (kbd "s") 'param-config-save)
  (define-key param-config-mode-map (kbd "l") 'param-config-load))

;; Param buffer mode

(define-derived-mode param-config-mode fundamental-mode
  "param-config mode"
  "Major mode for selecting arguments passed to param-config in a construction buffer"
  (use-local-map param-config-mode-map)
  (read-only-mode t))

(provide 'param-config)
;;; param-config.el ends here
