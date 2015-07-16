;;; ob-fsharp.el --- org-babel functions for fsharp evaluation

;; Copyright (C) 2009-2015 F.-D. Collin

;; Author: F.-D. Collin
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating fsharp source code. This is a
;; quick-and-dirt port of ob-ocaml and needs more support for fsharp
;; types.

;;; Requirements: fsharp-mode

;;; Code:
(require 'ob)
(require 'comint)
(eval-when-compile (require 'cl))
(require 'inf-fsharp-mode)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("fsharp" . "fsx"))

(defvar org-babel-default-header-args:fsharp '())

(defvar org-babel-fsharp-eoe-indicator "\"org-babel-fsharp-eoe\";;")
(defvar org-babel-fsharp-eoe-output "org-babel-fsharp-eoe")

(defcustom org-babel-fsharp-command "fsharp"
  "Name of the command for executing Fsharp code."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:fsharp (body params)
  "Execute a block of Fsharp code with Babel."
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         (session (org-babel-fsharp-initiate-session (first processed-params)))
         ;; variables assigned for use in the block
         (vars (second processed-params))
         (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         ;; expand the body with `org-babel-expand-body:fsharp'
         (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:fsharp params)))
         (raw (org-babel-comint-with-output
		  (session org-babel-fsharp-eoe-output t full-body)
		(insert
		 (concat
		  (org-babel-chomp full-body) ";;\n"
		  org-babel-fsharp-eoe-indicator))
		(comint-send-input nil t)))
	 (clean
	  (car (let ((re (regexp-quote org-babel-fsharp-eoe-output)) out)
		 (delq nil (mapcar (lambda (line)
				     (if out
					 (progn (setq out nil) line)
				       (when (string-match re line)
					 (progn (setq out t) nil))))
				   (mapcar #'org-babel-trim (reverse raw)))))))
	 (raw (org-babel-trim clean))
	 (result-params (cdr (assoc :result-params params)))
	 (parsed 
	  (string-match 
	   "\\(\\(.*\n\\)*\\)[^:\n]+ : \\([^=\n]+\\) =\\(\n\\| \\)\\(.+\\)$" 
	   raw))
	 (output (match-string 1 raw))
	 (type (match-string 3 raw))
	 (value (match-string 5 raw)))
    (org-babel-reassemble-table
     (org-babel-result-cond result-params
       (cond
	((member "verbatim" result-params) raw)
	((member "output" result-params) output)
	(t raw))
       (if (and value type)
	   (org-babel-fsharp-parse-output value type)
	 raw))
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defconst inferior-ob-fsharp-buffer-subname "inferior-ob-fsharp")
(defconst inferior-ob-fsharp-buffer-name
  (concat "*" inferior-ob-fsharp-buffer-subname "*"))

(defun fsharp-run-process-if-needed (&optional cmd)
  "Launch fsi if needed, using CMD if supplied."
  (if (comint-check-proc inferior-ob-fsharp-buffer-name) nil
    (if (not cmd)
        (if (comint-check-proc inferior-ob-fsharp-buffer-name)
            (setq cmd inferior-fsharp-program)
          (setq cmd (read-from-minibuffer "fsharp toplevel to run: "
                                          inferior-fsharp-program))))
    (setq inferior-fsharp-program cmd)
    (let ((cmdlist (inferior-fsharp-args-to-list cmd))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint)
                         inferior-ob-fsharp-buffer-subname
                         (car cmdlist) nil (cdr cmdlist)))
;;      (inferior-fsharp-mode)
      (display-buffer inferior-ob-fsharp-buffer-name)
      t)
    ))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:fsharp (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-fsharp-initiate-session session)))
      (org-babel-comint-in-buffer buffer
      	(mapc (lambda (line)
		(insert line)
		(comint-send-input nil t))
	      (org-babel-variable-assignments:fsharp params)))
      (current-buffer)))
    )

(defun org-babel-fsharp-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (or (get-buffer inferior-ob-fsharp-buffer-name)
      (save-window-excursion (fsharp-run-process-if-needed) (sleep-for 0.25) (current-buffer))))

(defun org-babel-variable-assignments:fsharp (params)
  "Return list of fsharp statements assigning the block's variables."
  (mapcar
   (lambda (pair) (format "let %s = %s;;" (car pair)
			  (org-babel-fsharp-elisp-to-fsharp (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-fsharp-elisp-to-fsharp (val)
  "Return a string of fsharp code which evaluates to VAL."
  (if (listp val)
      (concat "[|" (mapconcat #'org-babel-fsharp-elisp-to-fsharp val "; ") "|]")
    (format "%S" val)))

(defun org-babel-fsharp-parse-output (value type)
  "Parse VALUE of type TYPE.
VALUE and TYPE are string output from an fsharp process."
  (cond
   ((string= "string" type)
    (org-babel-read value))
   ((or (string= "int" type)
	(string= "float" type))
    (string-to-number value))
   ((string-match "list" type)
    (org-babel-fsharp-read-list value))
   ((string-match "\\\[\\\]" type)
    (org-babel-fsharp-read-array value))
   (t (message "don't recognize type %s" type) value)))

(defun org-babel-fsharp-read-list (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  ;; XXX: This probably does not behave as expected when a semicolon
  ;; is in a string in a list.  The same comment applies to
  ;; `org-babel-fsharp-read-array' below (with even more failure
  ;; modes).
  (org-babel-script-escape (replace-regexp-in-string ";" "," results)))

(defun org-babel-fsharp-read-array (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape
   (replace-regexp-in-string
    "\\[|" "[" (replace-regexp-in-string
		"|\\]" "]" (replace-regexp-in-string
			    "; " "," results)))))

(provide 'ob-fsharp)



;;; ob-fsharp.el ends here
