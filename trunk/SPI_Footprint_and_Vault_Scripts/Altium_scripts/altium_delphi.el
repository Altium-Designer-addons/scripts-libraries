;;; altium_delphi.el --- major mode for editing Altium Delphi scripts in Emacs
;;
;; {***************************************************************************
;;  * Sierra Photonics Inc. has derived this file from pascal.el.
;;  *  Modified / updated code is:
;;  *
;;  * Copyright (c) 2012 by Sierra Photonics Inc.  All rights reserved.
;;  *  Author:        Jeff Collins, jcollins@sierraphotonics.com
;;  *  Author:        $Author: jcollins $
;;  *  Check-in Date: $Date: 2012-12-13 10:17:29 -0800 (Thu, 13 Dec 2012) $ 
;;  *  Version #:     $Revision: 2803 $
;;  *  
;;  *  License:  GNU General Public License version 2, or (at your option)
;;  *   any later version.
;;  *  
;;  ***************************************************************************}

;; ***************************************************************************
;; * Copyright (C) 1993, 1994, 1995, 1996, 1997 Free Software Foundation, Inc.
;; ***************************************************************************


;; Author: Espen Skoglund (espensk@stud.cs.uit.no)

;;; pascal.el --- major mode for editing pascal source in Emacs

;; Copyright (C) 1993, 1994, 1995, 1996, 1997 Free Software Foundation, Inc.

;; Author: Espen Skoglund (espensk@stud.cs.uit.no)
;; Keywords: languages

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.34

;;; Commentary:

;; USAGE
;; =====

;; Emacs should enter Altium_Delphi mode when you find a Altium_Delphi source file.
;; When you have entered Altium_Delphi mode, you may get more info by pressing
;; C-h m. You may also get online help describing various functions by:
;; C-h f <Name of function you want described>

;; If you want to customize Altium_Delphi mode to fit you better, you may add
;; these lines (the values of the variables presented here are the defaults):
;;
;; ;; User customization for Altium_Delphi mode
;; (setq altium_delphi-indent-level       3
;;       altium_delphi-case-indent        2
;;       altium_delphi-auto-newline       nil
;;       altium_delphi-tab-always-indent  t
;;       altium_delphi-auto-endcomments   t
;;       altium_delphi-auto-lineup        '(all)
;;       altium_delphi-toggle-completions nil
;;       altium_delphi-type-keywords      '("array" "file" "packed" "char" 
;; 				     "integer" "real" "string" "record")
;;       altium_delphi-start-keywords     '("begin" "end" "function" "procedure"
;; 				     "repeat" "until" "while" "read" "readln"
;; 				     "reset" "rewrite" "write" "writeln")
;;       altium_delphi-separator-keywords '("downto" "else" "mod" "div" "then"))

;; KNOWN BUGS / BUGREPORTS
;; =======================
;; As far as I know, there are no bugs in the current version of this
;; package.  This may not be true however, since I never use this mode
;; myself and therefore would never notice them anyway.   If you do
;; find any bugs, you may submit them to: espensk@stud.cs.uit.no
;; as well as to bug-gnu-emacs@prep.ai.mit.edu.

;;; Code:

(defconst altium_delphi-mode-version "2.5"
  "Version of `altium_delphi.el'.")

(defgroup altium_delphi nil
  "Major mode for editing Altium_Delphi source in Emacs"
  :group 'languages)

(defvar altium_delphi-mode-abbrev-table nil
  "Abbrev table in use in Altium_Delphi-mode buffers.")
(define-abbrev-table 'altium_delphi-mode-abbrev-table ())

(defvar altium_delphi-mode-map ()
  "Keymap used in Altium_Delphi mode.")
(if altium_delphi-mode-map
    ()
  (setq altium_delphi-mode-map (make-sparse-keymap))
  (define-key altium_delphi-mode-map ";"        'electric-altium_delphi-semi-or-dot)
  (define-key altium_delphi-mode-map "."        'electric-altium_delphi-semi-or-dot)
  (define-key altium_delphi-mode-map ":"        'electric-altium_delphi-colon)
  (define-key altium_delphi-mode-map "="        'electric-altium_delphi-equal)
  (define-key altium_delphi-mode-map "#"        'electric-altium_delphi-hash)
  (define-key altium_delphi-mode-map "\r"       'electric-altium_delphi-terminate-line)
  (define-key altium_delphi-mode-map "\t"       'electric-altium_delphi-tab)
  (define-key altium_delphi-mode-map "\M-\t"    'altium_delphi-complete-word)
  (define-key altium_delphi-mode-map "\M-?"     'altium_delphi-show-completions)
  (define-key altium_delphi-mode-map "\M-\C-h"  'altium_delphi-mark-defun)
  (define-key altium_delphi-mode-map "\C-c\C-b" 'altium_delphi-insert-block)
  (define-key altium_delphi-mode-map "\M-*"     'altium_delphi-star-comment)
  (define-key altium_delphi-mode-map "\C-c\C-c" 'altium_delphi-comment-area)
  (define-key altium_delphi-mode-map "\C-c\C-u" 'altium_delphi-uncomment-area)
  (define-key altium_delphi-mode-map "\M-\C-a"  'altium_delphi-beg-of-defun)
  (define-key altium_delphi-mode-map "\M-\C-e"  'altium_delphi-end-of-defun)
  (define-key altium_delphi-mode-map "\C-c\C-d" 'altium_delphi-goto-defun)
  (define-key altium_delphi-mode-map "\C-c\C-o" 'altium_delphi-outline)
;;; A command to change the whole buffer won't be used terribly
;;; often, so no need for a key binding.
;  (define-key altium_delphi-mode-map "\C-cd"    'altium_delphi-downcase-keywords)
;  (define-key altium_delphi-mode-map "\C-cu"    'altium_delphi-upcase-keywords)
;  (define-key altium_delphi-mode-map "\C-cc"    'altium_delphi-capitalize-keywords)
  )

(defvar altium_delphi-imenu-generic-expression
  '("^[ \t]*\\(function\\|procedure\\)[ \t\n]+\\([a-zA-Z0-9_.:]+\\)" . (2))
  "Imenu expression for Altium_Delphi-mode.  See `imenu-generic-expression'.")

(defvar altium_delphi-keywords
  '("and" "array" "begin" "case" "const" "div" "do" "downto" "else" "end" 
    "file" "for" "function" "goto" "if" "in" "label" "mod" "nil" "not" "of" 
    "or" "packed" "procedure" "program" "record" "repeat" "set" "then" "to" 
    "type" "until" "var" "while" "with" "true" "false"
    ;; The following are not standard in altium_delphi, but widely used.
    "get" "put" "input" "output" "read" "readln" "reset" "rewrite" "write"
    "writeln"))

;;;
;;; Regular expressions used to calculate indent, etc.
;;;
(defconst altium_delphi-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
(defconst altium_delphi-beg-block-re   "\\<\\(begin\\|case\\|record\\|repeat\\)\\>")
(defconst altium_delphi-end-block-re   "\\<\\(end\\|until\\)\\>")
(defconst altium_delphi-declaration-re "\\<\\(const\\|label\\|type\\|var\\)\\>")
(defconst altium_delphi-defun-re       "\\<\\(function\\|procedure\\|program\\)\\>")
(defconst altium_delphi-sub-block-re   "\\<\\(if\\|else\\|for\\|while\\|with\\)\\>")
(defconst altium_delphi-noindent-re    "\\<\\(begin\\|end\\|until\\|else\\)\\>")
(defconst altium_delphi-nosemi-re      "\\<\\(begin\\|repeat\\|then\\|do\\|else\\)\\>")
(defconst altium_delphi-autoindent-lines-re
  "\\<\\(label\\|var\\|type\\|const\\|until\\|end\\|begin\\|repeat\\|else\\)\\>")

;;; Strings used to mark beginning and end of excluded text
(defconst altium_delphi-exclude-str-start "{-----\\/----- EXCLUDED -----\\/-----")
(defconst altium_delphi-exclude-str-end " -----/\\----- EXCLUDED -----/\\-----}")

(defvar altium_delphi-mode-syntax-table nil
  "Syntax table in use in Altium_Delphi-mode buffers.")

(if altium_delphi-mode-syntax-table
    ()
  (setq altium_delphi-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "."   altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?( "()1"  altium_delphi-mode-syntax-table)  
  (modify-syntax-entry ?) ")(4"  altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?{ "<"    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?} ">"    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?+ "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?- "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?= "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?% "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?< "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?> "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?& "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?| "."    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?_ "_"    altium_delphi-mode-syntax-table)
  (modify-syntax-entry ?\' "\""  altium_delphi-mode-syntax-table))

(defvar altium_delphi-font-lock-keywords (purecopy
  (list
   '("^[ \t]*\\(function\\|pro\\(cedure\\|gram\\)\\)\\>[ \t]*\\(\\[a-z]\\)?"
     1 font-lock-keyword-face)
   '("^[ \t]*\\(function\\|pro\\(cedure\\|gram\\)\\)\\>[ \t]*\\([a-z][a-z0-9_]*\\)"
     3 font-lock-function-name-face t)
;   ("type" "const" "real" "integer" "char" "boolean" "var"
;    "record" "array" "file")
   (cons (concat "\\<\\(array\\|boolean\\|c\\(har\\|onst\\)\\|file\\|"
		 "integer\\|re\\(al\\|cord\\)\\|type\\|var\\|"
		 "tstring\\|tstringlist\\|tlabel\\|tcheckbox\\|tlabel\\|tpanel\\|"
		 "tdynamicstring\\|tbutton\\|txstatusbar\\|trunoutjobs\\|textfile\\|"
		 "iproject\\|idocument\\|iworkspace\\|iparameter\\|txceedzip\\|"
		 "isch_sheet\\|isch_iterator\\|isch_component\\|isch_parameter\\|"
		 "idatabaselibdocument\\|widestring\\|string\\|tbytes\\|string\\|"
		 "isch_implementation\\|isch_modeldatafilelink\\|isch_document\\|"
		 "ipcb_board\\|ipcb_boarditerator\\|ipcb_component\\|"
		 "tcoord\\|ipcb_track\\|ipcb_arc\\|"
		 "tangle\\|tinterfacelist\\|ipcb_text\\|"
		 "tlayer\\|tcoordrect\\|ipcb_contour\\|"
		 "ipcb_region\\|ipcb_pad\\|tpadcache\\|"
		 "ipcb_libcomponent\\|ipcb_library\\|ipcb_mechanicallayer\\|"
		 "ipcb_model\\|ipcb_componentbody\\|ipcb_layerstack\\|"
		 "tdatetime\\|ipcb_componentbody\\|ipcb_layerstack\\|"
		 "icomponent\\|inet\\|iserverdocument\\)\\>")
	 'font-lock-type-face)
   '("\\<\\(label\\|external\\|forward\\)\\>" . font-lock-reference-face)
   '("\\<\\([0-9]+\\)[ \t]*:" 1 font-lock-function-name-face)
;   ("of" "to" "for" "if" "then" "else" "case" "while"
;    "do" "until" "and" "or" "not" "in" "with" "repeat" "begin" "end")
   (concat "\\<\\("
	   "and\\|begin\\|case\\|do\\|e\\(lse\\|nd\\)\\|for\\|i[fn]\\|"
	   "not\\|o[fr]\\|repeat\\|t\\(hen\\|o\\)\\|until\\|w\\(hile\\|ith\\)"
	   "\\)\\>")
   '("\\<\\(goto\\)\\>[ \t]*\\([0-9]+\\)?"
     1 font-lock-keyword-face)
   '("\\<\\(goto\\)\\>[ \t]*\\([0-9]+\\)?"
     2 font-lock-keyword-face nil t)))
  "Additional expressions to highlight in Altium_Delphi mode.")
(put 'altium_delphi-mode 'font-lock-defaults '(altium_delphi-font-lock-keywords nil t))

(defcustom altium_delphi-indent-level 3
  "*Indentation of Altium_Delphi statements with respect to containing block."
  :type 'integer
  :group 'altium_delphi)

(defcustom altium_delphi-case-indent 2
  "*Indentation for case statements."
  :type 'integer
  :group 'altium_delphi)

(defcustom altium_delphi-auto-newline nil
  "*Non-nil means automatically newline after semicolons and the punctuation
mark after an end."
  :type 'boolean
  :group 'altium_delphi)

(defcustom altium_delphi-tab-always-indent t
  "*Non-nil means TAB in Altium_Delphi mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'altium_delphi)

(defcustom altium_delphi-auto-endcomments t
  "*Non-nil means a comment { ... } is set after the ends which ends cases and
functions. The name of the function or case will be set between the braces."
  :type 'boolean
  :group 'altium_delphi)

(defcustom altium_delphi-auto-lineup '(all)
  "*List of contexts where auto lineup of :'s or ='s should be done.
Elements can be of type: 'paramlist', 'declaration' or 'case', which will
do auto lineup in parameterlist, declarations or case-statements
respectively. The word 'all' will do all lineups. '(case paramlist) for
instance will do lineup in case-statements and parameterlist, while '(all)
will do all lineups."
  :type '(repeat (choice (const all)
			 (const paramlist)
			 (const declaration)
			 (const case)))
  :group 'altium_delphi)

(defcustom altium_delphi-toggle-completions nil
  "*Non-nil means that repeated use of \
\\<altium_delphi-mode-map>\\[altium_delphi-complete-word] will toggle the possible
completions in the minibuffer.  Normally, when there is more than one possible
completion, a buffer will display all completions."
  :type 'boolean
  :group 'altium_delphi)

(defcustom altium_delphi-type-keywords
  '("array" "file" "packed" "char" "integer" "real" "string" "record" "tstring" "tstringlist" "tstring" "tstringlist" "tlabel" "tcheckbox" "tlabel" "tpanel" "tdynamicstring" "tbutton" "txstatusbar" "trunoutjobs" "textfile" "iproject" "idocument" "iworkspace" "iparameter" "txceedzip" "isch_sheet" "isch_iterator" "isch_component" "isch_parameter" "idatabaselibdocument" "widestring" "string" "tbytes" "string" "isch_implementation" "isch_modeldatafilelink" "isch_document" "icomponent" "inet" "iserverdocument")
  "*Keywords for types used when completing a word in a declaration or parmlist.
\(eg. integer, real, char.)  The types defined within the Altium_Delphi program
will be completed runtime, and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'altium_delphi)

(defcustom altium_delphi-start-keywords
  '("begin" "end" "function" "procedure" "repeat" "until" "while"
    "read" "readln" "reset" "rewrite" "write" "writeln")
  "*Keywords to complete when standing at the first word of a statement.
\(eg. begin, repeat, until, readln.)
The procedures and variables defined within the Altium_Delphi program
will be completed runtime and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'altium_delphi)

(defcustom altium_delphi-separator-keywords
  '("downto" "else" "mod" "div" "then")
  "*Keywords to complete when NOT standing at the first word of a statement.
\(eg. downto, else, mod, then.) 
Variables and function names defined within the
Altium_Delphi program are completed runtime and should not be added to this list."
  :type '(repeat (string :tag "Keyword"))
  :group 'altium_delphi)

;;;
;;;  Macros
;;;

(defsubst altium_delphi-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst altium_delphi-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defun altium_delphi-declaration-end ()
  (let ((nest 1))
    (while (and (> nest 0)
		(re-search-forward 
		 "[:=]\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" 
		 (save-excursion (end-of-line 2) (point)) t))
      (cond ((match-beginning 1) (setq nest (1+ nest)))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((looking-at "[^(\n]+)") (setq nest 0))))))


(defun altium_delphi-declaration-beg ()
  (let ((nest 1))
    (while (and (> nest 0)
		(re-search-backward "[:=]\\|\\<\\(type\\|var\\|label\\|const\\)\\>\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)" (altium_delphi-get-beg-of-line 0) t))
      (cond ((match-beginning 1) (setq nest 0))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((match-beginning 3) (setq nest (1+ nest)))))
    (= nest 0)))

  
(defsubst altium_delphi-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (altium_delphi-get-beg-of-line) (point)))))


;;;###autoload
(defun altium_delphi-mode ()
  "Major mode for editing Altium_Delphi code. \\<altium_delphi-mode-map>
TAB indents for Altium_Delphi code.  Delete converts tabs to spaces as it moves back.

\\[altium_delphi-complete-word] completes the word around current point with respect \
to position in code
\\[altium_delphi-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[altium_delphi-mark-defun]\t- Mark function.
\\[altium_delphi-insert-block]\t- insert begin ... end;
\\[altium_delphi-star-comment]\t- insert (* ... *)
\\[altium_delphi-comment-area]\t- Put marked area in a comment, fixing nested comments.
\\[altium_delphi-uncomment-area]\t- Uncomment an area commented with \
\\[altium_delphi-comment-area].
\\[altium_delphi-beg-of-defun]\t- Move to beginning of current function.
\\[altium_delphi-end-of-defun]\t- Move to end of current function.
\\[altium_delphi-goto-defun]\t- Goto function prompted for in the minibuffer.
\\[altium_delphi-outline]\t- Enter altium_delphi-outline-mode (see also altium_delphi-outline).

Variables controlling indentation/edit style:

 altium_delphi-indent-level      (default 3)
    Indentation of Altium_Delphi statements with respect to containing block.
 altium_delphi-case-indent       (default 2)
    Indentation for case statements.
 altium_delphi-auto-newline      (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 altium_delphi-tab-always-indent (default t)
    Non-nil means TAB in Altium_Delphi mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 altium_delphi-auto-endcomments  (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 altium_delphi-auto-lineup       (default t)
    List of contexts where auto lineup of :'s or ='s should be done.

See also the user variables altium_delphi-type-keywords, altium_delphi-start-keywords and
altium_delphi-separator-keywords.

Turning on Altium_Delphi mode calls the value of the variable altium_delphi-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map altium_delphi-mode-map)
  (setq major-mode 'altium_delphi-mode)
  (setq mode-name "Altium_Delphi")
  (setq local-abbrev-table altium_delphi-mode-abbrev-table)
  (set-syntax-table altium_delphi-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'altium_delphi-indent-line)
  (setq comment-indent-function 'altium_delphi-indent-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'comment-start)
  (setq comment-start "{")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *\\|{ *")
  (make-local-variable 'comment-end)
  (setq comment-end "}")
  ;; Font lock support
  ;(make-local-variable 'font-lock-defaults)
  ;(setq font-lock-defaults '(altium_delphi-font-lock-keywords nil t))
  ;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression altium_delphi-imenu-generic-expression)
  (run-hooks 'altium_delphi-mode-hook))



;;;
;;;  Electric functions
;;;
(defun electric-altium_delphi-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at altium_delphi-autoindent-lines-re)
	(altium_delphi-indent-line)))
  (delete-horizontal-space) ; Removes trailing whitespaces
  (newline)
  ;; Indent next line
  (altium_delphi-indent-line)
  ;; Maybe we should set some endcomments
  (if altium_delphi-auto-endcomments
      (altium_delphi-set-auto-comments))
  ;; Check if we shall indent inside comment
  (let ((setstar nil))
    (save-excursion
      (forward-line -1)
      (skip-chars-forward " \t")
      (cond ((looking-at "\\*[ \t]+)")
	     ;; Delete region between `*' and `)' if there is only whitespaces.
	     (forward-char 1)
	     (delete-horizontal-space))
	    ((and (looking-at "(\\*\\|\\*[^)]")
		  (not (save-excursion
			 (search-forward "*)" (altium_delphi-get-end-of-line) t))))
	     (setq setstar t))))
    ;; If last line was a star comment line then this one shall be too.
    (if (null setstar)	
	(altium_delphi-indent-line)
      (insert "*  "))))


(defun electric-altium_delphi-semi-or-dot ()
  "Insert `;' or `.' character and reindent the line."
  (interactive)
  (insert last-command-char)
  (save-excursion
    (beginning-of-line)
    (altium_delphi-indent-line))
  (if altium_delphi-auto-newline
      (electric-altium_delphi-terminate-line)))

(defun electric-altium_delphi-colon ()
  "Insert `:' and do all indentions except line indent on this line."
  (interactive)
  (insert last-command-char)
  ;; Do nothing if within string.
  (if (altium_delphi-within-string)
      ()
    (save-excursion
      (beginning-of-line)
      (altium_delphi-indent-line))
    (let ((altium_delphi-tab-always-indent nil))
      (altium_delphi-indent-command))))

(defun electric-altium_delphi-equal ()
  "Insert `=', and do indention if within type declaration."
  (interactive)
  (insert last-command-char)
  (if (eq (car (altium_delphi-calculate-indent)) 'declaration)
      (let ((altium_delphi-tab-always-indent nil))
	(altium_delphi-indent-command))))

(defun electric-altium_delphi-hash ()
  "Insert `#', and indent to column 0 if this is a CPP directive."
  (interactive)
  (insert last-command-char)
  (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
      (save-excursion (beginning-of-line)
		      (delete-horizontal-space))))

(defun electric-altium_delphi-tab ()
  "Function called when TAB is pressed in Altium_Delphi mode."
  (interactive)
  ;; Do nothing if within a string or in a CPP directive.
  (if (or (altium_delphi-within-string)
	  (and (not (bolp))
	       (save-excursion (beginning-of-line) (eq (following-char) ?#))))
      (insert "\t")
    ;; If altium_delphi-tab-always-indent, indent the beginning of the line.
    (if altium_delphi-tab-always-indent
	(save-excursion
	  (beginning-of-line)
	  (altium_delphi-indent-line))
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (bolp))
	  (altium_delphi-indent-line)
	(insert "\t")))
    (altium_delphi-indent-command)))



;;;
;;; Interactive functions
;;;
(defun altium_delphi-insert-block ()
  "Insert Altium_Delphi begin ... end; block in the code with right indentation."
  (interactive)
  (altium_delphi-indent-line)
  (insert "begin")
  (electric-altium_delphi-terminate-line)
  (save-excursion
    (electric-altium_delphi-terminate-line)
    (insert "end;")
    (beginning-of-line)
    (altium_delphi-indent-line)))

(defun altium_delphi-star-comment ()
  "Insert Altium_Delphi star comment at point."
  (interactive)
  (altium_delphi-indent-line)
  (insert "(*")
  (electric-altium_delphi-terminate-line)
  (save-excursion
    (electric-altium_delphi-terminate-line)
    (delete-horizontal-space)
    (insert ")"))
  (insert "  "))

(defun altium_delphi-mark-defun ()
  "Mark the current altium_delphi function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (altium_delphi-end-of-defun)
  (push-mark (point))
  (altium_delphi-beg-of-defun)
  (if (fboundp 'zmacs-activate-region)
      (zmacs-activate-region)))

(defun altium_delphi-comment-area (start end)
  "Put the region into a Altium_Delphi comment.
The comments that are in this area are \"deformed\":
`*)' becomes `!(*' and `}' becomes `!{'.
These deformed comments are returned to normal if you use
\\[altium_delphi-uncomment-area] to undo the commenting.

The commented area starts with `altium_delphi-exclude-str-start', and ends with
`altium_delphi-include-str-end'.  But if you change these variables,
\\[altium_delphi-uncomment-area] won't recognize the comments."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
	     (not (save-excursion (skip-chars-backward " \t") (bolp))))
	(forward-line 1)
      (beginning-of-line))
    (insert altium_delphi-exclude-str-end)
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert altium_delphi-exclude-str-start)
    (newline)
    ;; Replace end-comments within commented area
    (goto-char end)
    (save-excursion
      (while (re-search-backward "\\*)" start t)
	(replace-match "!(*" t t)))
    (save-excursion
      (while (re-search-backward "}" start t)
	(replace-match "!{" t t)))))

(defun altium_delphi-uncomment-area ()
  "Uncomment a commented area; change deformed comments back to normal.
This command does nothing if the pointer is not in a commented
area.  See also `altium_delphi-comment-area'."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	(setq start (progn (search-backward altium_delphi-exclude-str-start nil t)
			   (point)))
	(setq end (progn (search-forward altium_delphi-exclude-str-end nil t)
			 (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
	  (message "Not standing within commented area.")
	(progn
	  ;; Remove endcomment
	  (goto-char end)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point))))
	  ;; Change comments back to normal
	  (save-excursion
	    (while (re-search-backward "!{" start t)
	      (replace-match "}" t t)))
	  (save-excursion
	    (while (re-search-backward "!(\\*" start t)
	      (replace-match "*)" t t)))
	  ;; Remove startcomment
	  (goto-char start)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point)))))))))

(defun altium_delphi-beg-of-defun ()
  "Move backward to the beginning of the current function or procedure."
  (interactive)
  (catch 'found
    (if (not (looking-at (concat "\\s \\|\\s)\\|" altium_delphi-defun-re)))
	(forward-sexp 1))
    (let ((nest 0) (max -1) (func 0)
	  (reg (concat altium_delphi-beg-block-re "\\|" 
		       altium_delphi-end-block-re "\\|"
		       altium_delphi-defun-re)))
      (while (re-search-backward reg nil 'move)
	(cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
	      ((match-end 1)                       ; begin|case|record|repeat
	       (if (and (looking-at "\\<record\\>") (>= max 0))
		   (setq func (1- func)))
	       (setq nest (1+ nest)
		     max (max nest max)))
	      ((match-end 2)                       ; end|until
	       (if (and (= nest max) (>= max 0))
		   (setq func (1+ func)))
	       (setq nest (1- nest)))
	      ((match-end 3)                       ; function|procedure
	       (if (= 0 func)
		   (throw 'found t)
		 (setq func (1- func)))))))
    nil))

(defun altium_delphi-end-of-defun ()
  "Move forward to the end of the current function or procedure."
  (interactive)
  (if (looking-at "\\s ")
      (forward-sexp 1))
  (if (not (looking-at altium_delphi-defun-re))
      (altium_delphi-beg-of-defun))
  (forward-char 1)
  (let ((nest 0) (func 1)
	(reg (concat altium_delphi-beg-block-re "\\|" 
		     altium_delphi-end-block-re "\\|"
		     altium_delphi-defun-re)))
    (while (and (/= func 0)
		(re-search-forward reg nil 'move))
      (cond ((let ((state (save-excursion
			      (parse-partial-sexp (point-min) (point)))))
		 (or (nth 3 state) (nth 4 state))) ; Inside string or comment
	       ())
	    ((match-end 1)
	     (setq nest (1+ nest))
	     (if (save-excursion
		   (goto-char (match-beginning 0))
		   (looking-at "\\<record\\>"))
		 (setq func (1+ func))))
	    ((match-end 2)
	     (setq nest (1- nest))
	     (if (= nest 0)
		 (setq func (1- func))))
	    ((match-end 3)
	     (setq func (1+ func))))))
  (forward-line 1))

(defun altium_delphi-end-of-statement ()
  "Move forward to end of current statement."
  (interactive)
  (let ((parse-sexp-ignore-comments t)
	(nest 0) pos
	(regexp (concat "\\(" altium_delphi-beg-block-re "\\)\\|\\("
			altium_delphi-end-block-re "\\)")))
    (if (not (looking-at "[ \t\n]")) (forward-sexp -1))
    (or (looking-at altium_delphi-beg-block-re)
	;; Skip to end of statement
	(setq pos (catch 'found
		    (while t
		      (forward-sexp 1)
		      (cond ((looking-at "[ \t]*;")
			     (skip-chars-forward "^;")
			     (forward-char 1)
			     (throw 'found (point)))
			    ((save-excursion
			       (forward-sexp -1)
			       (looking-at altium_delphi-beg-block-re))
			     (goto-char (match-beginning 0))
			     (throw 'found nil))
			    ((eobp)
			     (throw 'found (point))))))))
    (if (not pos)
	;; Skip a whole block
	(catch 'found
	  (while t
	    (re-search-forward regexp nil 'move)
	    (setq nest (if (match-end 1) 
			   (1+ nest)
			 (1- nest)))
	    (cond ((eobp)
		   (throw 'found (point)))
		  ((= 0 nest)
		   (throw 'found (altium_delphi-end-of-statement))))))
      pos)))

(defun altium_delphi-downcase-keywords ()
  "Downcase all Altium_Delphi keywords in the buffer."
  (interactive)
  (altium_delphi-change-keywords 'downcase-word))

(defun altium_delphi-upcase-keywords ()
  "Upcase all Altium_Delphi keywords in the buffer."
  (interactive)
  (altium_delphi-change-keywords 'upcase-word))

(defun altium_delphi-capitalize-keywords ()
  "Capitalize all Altium_Delphi keywords in the buffer."
  (interactive)
  (altium_delphi-change-keywords 'capitalize-word))

;; Change the keywords according to argument.
(defun altium_delphi-change-keywords (change-word)
  (save-excursion
    (let ((keyword-re (concat "\\<\\("
			      (mapconcat 'identity altium_delphi-keywords "\\|")
			      "\\)\\>")))
      (goto-char (point-min))
      (while (re-search-forward keyword-re nil t)
	(funcall change-word -1)))))



;;;
;;; Other functions
;;;
(defun altium_delphi-set-auto-comments ()
  "Insert `{ case }' or `{ NAME }' on this line if appropriate.
Insert `{ case }' if there is an `end' on the line which
ends a case block.  Insert `{ NAME }' if there is an `end'
on the line which ends a function or procedure named NAME."
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (if (and (looking-at "\\<end;")
	     (not (save-excursion
		    (end-of-line)
		    (search-backward "{" (altium_delphi-get-beg-of-line) t))))
	(let ((type (car (altium_delphi-calculate-indent))))
	  (if (eq type 'declaration)
	      ()
	    (if (eq type 'case)
		;; This is a case block
		(progn
		  (end-of-line)
		  (delete-horizontal-space)
		  (insert " { case }"))
	      (let ((nest 1))
		;; Check if this is the end of a function
		(save-excursion
		  (while (not (or (looking-at altium_delphi-defun-re) (bobp)))
		    (backward-sexp 1)
		    (cond ((looking-at altium_delphi-beg-block-re)
			   (setq nest (1- nest)))
			  ((looking-at altium_delphi-end-block-re)
			   (setq nest (1+ nest)))))
		  (if (bobp)
		      (setq nest 1)))
		(if (zerop nest)
		    (progn
		      (end-of-line)
		      (delete-horizontal-space)
		      (insert " { ")
		      (let (b e)
			(save-excursion
			  (setq b (progn (altium_delphi-beg-of-defun)
					 (skip-chars-forward "^ \t")
					 (skip-chars-forward " \t")
					 (point))
				e (progn (skip-chars-forward "a-zA-Z0-9_")
					 (point))))
			(insert-buffer-substring (current-buffer) b e))
		      (insert " }"))))))))))



;;;
;;; Indentation
;;;
(defconst altium_delphi-indent-alist
  '((block . (+ ind altium_delphi-indent-level))
    (case . (+ ind altium_delphi-case-indent))
    (caseblock . ind) (cpp . 0)
    (declaration . (+ ind altium_delphi-indent-level))
    (paramlist . (altium_delphi-indent-paramlist t))
    (comment . (altium_delphi-indent-comment t))
    (defun . ind) (contexp . ind)
    (unknown . 0) (string . 0)))

(defun altium_delphi-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (altium_delphi-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (cond ((and (eq type 'paramlist)
		(or (memq 'all altium_delphi-auto-lineup)
		    (memq 'paramlist altium_delphi-auto-lineup)))
	   (altium_delphi-indent-paramlist)
	   (altium_delphi-indent-paramlist))
	  ((and (eq type 'declaration)
		(or (memq 'all altium_delphi-auto-lineup)
		    (memq 'declaration  altium_delphi-auto-lineup)))
	   (altium_delphi-indent-declaration))
	  ((and (eq type 'case) (not (looking-at "^[ \t]*$"))
		(or (memq 'all altium_delphi-auto-lineup)
		    (memq 'case altium_delphi-auto-lineup)))
	   (altium_delphi-indent-case)))
    (if (looking-at "[ \t]+$")
	(skip-chars-forward " \t"))))

(defun altium_delphi-indent-line ()
  "Indent current line as a Altium_Delphi statement."
  (let* ((indent-str (altium_delphi-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (if (looking-at "^[0-9a-zA-Z]+[ \t]*:[^=]")
	(search-forward ":" nil t))
    (delete-horizontal-space)
    ;; Some things should not be indented
    (if (or (and (eq type 'declaration) (looking-at altium_delphi-declaration-re))
	    (eq type 'cpp)
	    (looking-at altium_delphi-defun-re))
	()
      ;; Other things should have no extra indent
      (if (looking-at altium_delphi-noindent-re)
	  (indent-to ind)
	;; But most lines are treated this way:
	(indent-to (eval (cdr (assoc type altium_delphi-indent-alist))))
	))))

(defun altium_delphi-calculate-indent ()
  "Calculate the indent of the current Altium_Delphi line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point))
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (complete (looking-at "[ \t]*end\\>"))
	   (elsed (looking-at "[ \t]*else\\>"))
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (goto-char (scan-lists (point) -1 (car state)))
			  (setq par (1+ (current-column))))
			 ((save-excursion (beginning-of-line)
					  (eq (following-char) ?#))
			  (throw 'nesting 'cpp)))
		   ;; Loop until correct indent is found
		   (while t
		     (backward-sexp 1)
		     (cond (;--Escape from case statements
			    (and (looking-at "[A-Za-z0-9]+[ \t]*:[^=]")
				 (not complete)
				 (save-excursion (skip-chars-backward " \t")
						 (bolp))
				 (= (save-excursion
				      (end-of-line) (backward-sexp) (point))
				    (point))
				 (> (save-excursion (goto-char oldpos)
						    (beginning-of-line)
						    (point))
				    (point)))
			    (throw 'nesting 'caseblock))
			   (;--Nest block outwards
			    (looking-at altium_delphi-beg-block-re)
			    (if (= nest 0)
				(cond ((looking-at "case\\>")
				       (throw 'nesting 'case))
				      ((looking-at "record\\>")
				       (throw 'nesting 'declaration))
				      (t (throw 'nesting 'block)))
			      (setq nest (1- nest))))
			   (;--Nest block inwards
			    (looking-at altium_delphi-end-block-re)
			    (if (and (looking-at "end\\s ")
				     elsed (not complete))
				(throw 'nesting 'block))
			    (setq complete t
				  nest (1+ nest)))
			   (;--Defun (or parameter list)
			    (looking-at altium_delphi-defun-re)
			    (if (= 0 par)
				(throw 'nesting 'defun)
			      (setq par 0)
			      (let ((n 0))
				(while (re-search-forward
					"\\(\\<record\\>\\)\\|\\<end\\>"
					oldpos t)
				  (if (match-end 1)
				      (setq n (1+ n)) (setq n (1- n))))
				(if (> n 0)
				    (throw 'nesting 'declaration)
				  (throw 'nesting 'paramlist)))))
			   (;--Declaration part
			    (looking-at altium_delphi-declaration-re)
			    (if (save-excursion
				  (goto-char oldpos)
				  (forward-line -1)
				  (looking-at "^[ \t]*$"))
				(throw 'nesting 'unknown)
			      (throw 'nesting 'declaration)))
			   (;--If, else or while statement
			    (and (not complete)
				 (looking-at altium_delphi-sub-block-re))
			    (throw 'nesting 'block))
			   (;--Found complete statement
			    (save-excursion (forward-sexp 1)
					    (= (following-char) ?\;))
			    (setq complete t))
			   (;--No known statements
			    (bobp)
			    (throw 'nesting 'unknown))
			   )))))

      ;; Return type of block and indent level.
      (if (> par 0)                               ; Unclosed Parenthesis 
	  (list 'contexp par)
	(list type (altium_delphi-indent-level))))))

(defun altium_delphi-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case-statements or records."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^=]")
	(search-forward ":" nil t)
      (if (looking-at ".*=[ \t]*record\\>")
	  (search-forward "=" nil t)))
    (skip-chars-forward " \t")
    (current-column)))

(defun altium_delphi-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
  (let* ((stcol (save-excursion
		  (re-search-backward "(\\*\\|{" nil t)
		  (1+ (current-column)))))
    (if arg stcol
      (delete-horizontal-space)
      (indent-to stcol))))

(defun altium_delphi-indent-case ()
  "Indent within case statements."
  (let ((savepos (point-marker))
	(end (prog2
		 (end-of-line)
		 (point-marker)
	       (re-search-backward "\\<case\\>" nil t)))
	(beg (point)) oldpos
	(ind 0))
    ;; Get right indent
    (while (< (point) end)
      (if (re-search-forward 
	   "^[ \t]*[^ \t,:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
	   (marker-position end) 'move)
	  (forward-char -1))
      (if (< (point) end)
	  (progn
	    (delete-horizontal-space)
	    (if (> (current-column) ind)
		(setq ind (current-column)))
	    (altium_delphi-end-of-statement))))
    (goto-char beg)
    (setq oldpos (marker-position end))
    ;; Indent all case statements
    (while (< (point) end)
      (if (re-search-forward
	   "^[ \t]*[^][ \t,\\.:]+[ \t]*\\(,[ \t]*[^ \t,:]+[ \t]*\\)*:"
	   (marker-position end) 'move)
	  (forward-char -1))
      (indent-to (1+ ind))
      (if (/= (following-char) ?:)
	  ()
	(forward-char 1)
	(delete-horizontal-space)
	(insert " "))
      (setq oldpos (point))
      (altium_delphi-end-of-statement))
    (goto-char savepos)))

(defun altium_delphi-indent-paramlist (&optional arg)
  "Indent current line in parameterlist.
If optional arg is non-nil, just return the
indent of the current line in parameterlist."
  (save-excursion
    (let* ((oldpos (point))
	   (stpos (progn (goto-char (scan-lists (point) -1 1)) (point)))
	   (stcol (1+ (current-column)))
	   (edpos (progn (altium_delphi-declaration-end) 
			 (search-backward ")" (altium_delphi-get-beg-of-line) t)
			 (point)))
	   (usevar (re-search-backward "\\<var\\>" stpos t)))
      (if arg (progn
		;; If arg, just return indent
		(goto-char oldpos)
		(beginning-of-line)
		(if (or (not usevar) (looking-at "[ \t]*var\\>"))
		    stcol (+ 4 stcol)))
	(goto-char stpos)
	(forward-char 1)
	(delete-horizontal-space)
	(if (and usevar (not (looking-at "var\\>")))
	    (indent-to (+ 4 stcol)))
	(altium_delphi-indent-declaration nil stpos edpos)))))

(defun altium_delphi-indent-declaration (&optional arg start end)
  "Indent current lines as declaration, lining up the `:'s or `='s."
  (let ((pos (point-marker)))
    (if (and (not (or arg start)) (not (altium_delphi-declaration-beg)))
	()
      (let ((lineup (if (or (looking-at "\\<var\\>\\|\\<record\\>") arg start) 
			":" "="))
	    (stpos (if start start
		       (forward-word 2) (backward-word 1) (point)))
	    (edpos (set-marker (make-marker)
			       (if end end
				 (max (progn (altium_delphi-declaration-end)
					     (point))
				      pos))))
	    ind)

	(goto-char stpos)
	;; Indent lines in record block
	(if arg
	    (while (<= (point) edpos)
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (if (looking-at "end\\>")
		  (indent-to arg)
		(indent-to (+ arg altium_delphi-indent-level)))
	      (forward-line 1)))

	;; Do lineup
	(setq ind (altium_delphi-get-lineup-indent stpos edpos lineup))
	(goto-char stpos)
	(while (and (<= (point) edpos) (not (eobp)))
	  (if (search-forward lineup (altium_delphi-get-end-of-line) 'move)
	      (forward-char -1))
	  (delete-horizontal-space)
	  (indent-to ind)
	  (if (not (looking-at lineup))
	      (forward-line 1) ; No more indent if there is no : or =
	    (forward-char 1)
	    (delete-horizontal-space)
	    (insert " ")
	    ;; Indent record block
	    (if (looking-at "record\\>")
		(altium_delphi-indent-declaration (current-column)))
	    (forward-line 1)))))

    ;; If arg - move point
    (if arg (forward-line -1)
      (goto-char pos))))

;  "Return the indent level that will line up several lines within the region
;from b to e nicely. The lineup string is str."
(defun altium_delphi-get-lineup-indent (b e str)
  (save-excursion
    (let ((ind 0)
	  (reg (concat str "\\|\\(\\<record\\>\\)")))
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
	(if (re-search-forward reg (min e (altium_delphi-get-end-of-line 2)) 'move)
	    (progn
	      ;; Skip record blocks
	      (if (match-beginning 1)
		  (altium_delphi-declaration-end)
		(progn
		  (goto-char (match-beginning 0))
		  (skip-chars-backward " \t")
		  (if (> (current-column) ind)
		      (setq ind (current-column)))
		  (goto-char (match-end 0))
		  (end-of-line)
		  )))))
      ;; In case no lineup was found
      (if (> ind 0)
	  (1+ ind)
	;; No lineup-string found
	(goto-char b)
	(end-of-line)
	(skip-chars-backward " \t")
	(1+ (current-column))))))
    


;;;
;;; Completion
;;;
(defvar altium_delphi-str nil)
(defvar altium_delphi-all nil)
(defvar altium_delphi-pred nil)
(defvar altium_delphi-buffer-to-use nil)
(defvar altium_delphi-flag nil)

(defun altium_delphi-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

;; Calculate all possible completions for functions if argument is `function',
;; completions for procedures if argument is `procedure' or both functions and
;; procedures otherwise.

(defun altium_delphi-func-completion (type)
  ;; Build regular expression for function/procedure names
  (if (string= altium_delphi-str "")
      (setq altium_delphi-str "[a-zA-Z_]"))
  (let ((altium_delphi-str (concat (cond
			     ((eq type 'procedure) "\\<\\(procedure\\)\\s +")
			     ((eq type 'function) "\\<\\(function\\)\\s +")
			     (t "\\<\\(function\\|procedure\\)\\s +"))
			    "\\<\\(" altium_delphi-str "[a-zA-Z0-9_.]*\\)\\>"))
	match)
    
    (if (not (looking-at "\\<\\(function\\|procedure\\)\\>"))
	(re-search-backward "\\<\\(function\\|procedure\\)\\>" nil t))
    (forward-char 1)

    ;; Search through all reachable functions
    (while (altium_delphi-beg-of-defun)
      (if (re-search-forward altium_delphi-str (altium_delphi-get-end-of-line) t)
	  (progn (setq match (buffer-substring (match-beginning 2)
					       (match-end 2)))
		 (if (or (null altium_delphi-pred)
			 (funcall altium_delphi-pred match))
		     (setq altium_delphi-all (cons match altium_delphi-all)))))
      (goto-char (match-beginning 0)))))

(defun altium_delphi-get-completion-decl ()
  ;; Macro for searching through current declaration (var, type or const)
  ;; for matches of `str' and adding the occurrence to `all'
  (let ((end (save-excursion (altium_delphi-declaration-end)
			     (point)))
	match)
    ;; Traverse lines
    (while (< (point) end)
      (if (re-search-forward "[:=]" (altium_delphi-get-end-of-line) t)
	  ;; Traverse current line
	  (while (and (re-search-backward 
		       (concat "\\((\\|\\<\\(var\\|type\\|const\\)\\>\\)\\|" 
			       altium_delphi-symbol-re)
		       (altium_delphi-get-beg-of-line) t)
		      (not (match-end 1)))
	    (setq match (buffer-substring (match-beginning 0) (match-end 0)))
	    (if (string-match (concat "\\<" altium_delphi-str) match)
		(if (or (null altium_delphi-pred)
			(funcall altium_delphi-pred match))
		    (setq altium_delphi-all (cons match altium_delphi-all))))))
      (if (re-search-forward "\\<record\\>" (altium_delphi-get-end-of-line) t)
	  (altium_delphi-declaration-end)
	(forward-line 1)))))

(defun altium_delphi-type-completion ()
  "Calculate all possible completions for types."
  (let ((start (point))
	goon)
    ;; Search for all reachable type declarations
    (while (or (altium_delphi-beg-of-defun)
	       (setq goon (not goon)))
      (save-excursion
	(if (and (< start (prog1 (save-excursion (altium_delphi-end-of-defun)
						 (point))
			    (forward-char 1)))
		 (re-search-forward
		  "\\<type\\>\\|\\<\\(begin\\|function\\|procedure\\)\\>"
		  start t)
		 (not (match-end 1)))
	    ;; Check current type declaration
	    (altium_delphi-get-completion-decl))))))

(defun altium_delphi-var-completion ()
  "Calculate all possible completions for variables (or constants)."
  (let ((start (point))
	goon twice)
    ;; Search for all reachable var declarations
    (while (or (altium_delphi-beg-of-defun)
	       (setq goon (not goon)))
      (save-excursion
	(if (> start (prog1 (save-excursion (altium_delphi-end-of-defun)
					    (point))))
	    () ; Declarations not reachable
	  (if (search-forward "(" (altium_delphi-get-end-of-line) t)
	      ;; Check parameterlist
		(altium_delphi-get-completion-decl))
	  (setq twice 2)
	  (while (>= (setq twice (1- twice)) 0)
	    (cond ((and (re-search-forward
			 (concat "\\<\\(var\\|const\\)\\>\\|"
				 "\\<\\(begin\\|function\\|procedure\\)\\>")
			 start t)
			(not (match-end 2)))
		   ;; Check var/const declarations
		   (altium_delphi-get-completion-decl))
		  ((match-end 2)
		   (setq twice 0)))))))))


(defun altium_delphi-keyword-completion (keyword-list)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (mapcar '(lambda (s) 
	     (if (string-match (concat "\\<" altium_delphi-str) s)
		 (if (or (null altium_delphi-pred)
			 (funcall altium_delphi-pred s))
		     (setq altium_delphi-all (cons s altium_delphi-all)))))
	  keyword-list))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.

(defun altium_delphi-completion (altium_delphi-str altium_delphi-pred altium_delphi-flag)
  (save-excursion
    (let ((altium_delphi-all nil))
      ;; Set buffer to use for searching labels. This should be set
      ;; within functions which use altium_delphi-completions
      (set-buffer altium_delphi-buffer-to-use)

      ;; Determine what should be completed
      (let ((state (car (altium_delphi-calculate-indent))))
	(cond (;--Within a declaration or parameterlist
	       (or (eq state 'declaration) (eq state 'paramlist)
		   (and (eq state 'defun)
			(save-excursion
			  (re-search-backward ")[ \t]*:"
					      (altium_delphi-get-beg-of-line) t))))
	       (if (or (eq state 'paramlist) (eq state 'defun))
		   (altium_delphi-beg-of-defun))
	       (altium_delphi-type-completion)
	       (altium_delphi-keyword-completion altium_delphi-type-keywords))
	      (;--Starting a new statement
	       (and (not (eq state 'contexp))
		    (save-excursion
		      (skip-chars-backward "a-zA-Z0-9_.")
		      (backward-sexp 1)
		      (or (looking-at altium_delphi-nosemi-re)
			  (progn
			    (forward-sexp 1)
			    (looking-at "\\s *\\(;\\|:[^=]\\)")))))
	       (save-excursion (altium_delphi-var-completion))
	       (altium_delphi-func-completion 'procedure)
	       (altium_delphi-keyword-completion altium_delphi-start-keywords))
	      (t;--Anywhere else
	       (save-excursion (altium_delphi-var-completion))
	       (altium_delphi-func-completion 'function)
	       (altium_delphi-keyword-completion altium_delphi-separator-keywords))))
      
      ;; Now we have built a list of all matches. Give response to caller
      (altium_delphi-completion-response))))

(defun altium_delphi-completion-response ()
  (cond ((or (equal altium_delphi-flag 'lambda) (null altium_delphi-flag))
	 ;; This was not called by all-completions
	 (if (null altium_delphi-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr altium_delphi-all))
		  (match (car altium_delphi-all))
		  (min (length match))
		  tmp)
	     (if (string= match altium_delphi-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (altium_delphi-string-diff match (car elm))) min)
		     (progn
		       (setq min tmp)
		       (setq match (substring match 0 min))))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) altium_delphi-str)
		     (progn
		       (setq match t)
		       (setq elm nil))
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal altium_delphi-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(altium_delphi-flag
	 altium_delphi-all)))

(defvar altium_delphi-last-word-numb 0)
(defvar altium_delphi-last-word-shown nil)
(defvar altium_delphi-last-completions nil)

(defun altium_delphi-complete-word ()
  "Complete word at current point.
\(See also `altium_delphi-toggle-completions', `altium_delphi-type-keywords',
`altium_delphi-start-keywords' and `altium_delphi-separator-keywords'.)"
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (altium_delphi-str (buffer-substring b e))
	 ;; The following variable is used in altium_delphi-completion
	 (altium_delphi-buffer-to-use (current-buffer))
	 (allcomp (if (and altium_delphi-toggle-completions
			   (string= altium_delphi-last-word-shown altium_delphi-str))
		      altium_delphi-last-completions
		    (all-completions altium_delphi-str 'altium_delphi-completion)))
	 (match (if altium_delphi-toggle-completions
		    "" (try-completion
			altium_delphi-str (mapcar '(lambda (elm)
					      (cons elm 0)) allcomp)))))
    ;; Delete old string
    (delete-region b e)

    ;; Toggle-completions inserts whole labels
    (if altium_delphi-toggle-completions
	(progn
	  ;; Update entry number in list
	  (setq altium_delphi-last-completions allcomp
		altium_delphi-last-word-numb 
		(if (>= altium_delphi-last-word-numb (1- (length allcomp)))
		    0
		  (1+ altium_delphi-last-word-numb)))
	  (setq altium_delphi-last-word-shown (elt allcomp altium_delphi-last-word-numb))
	  ;; Display next match or same string if no match was found
	  (if (not (null allcomp))
	      (insert "" altium_delphi-last-word-shown)
	    (insert "" altium_delphi-str)
	    (message "(No match)")))
      ;; The other form of completion does not necessarily do that.

      ;; Insert match if found, or the original string if no match
      (if (or (null match) (equal match 't))
	  (progn (insert "" altium_delphi-str)
		 (message "(No match)"))
	(insert "" match))
      ;; Give message about current status of completion
      (cond ((equal match 't)
	     (if (not (null (cdr allcomp)))
		 (message "(Complete but not unique)")
	       (message "(Sole completion)")))
	    ;; Display buffer if the current completion didn't help 
	    ;; on completing the label.
	    ((and (not (null (cdr allcomp))) (= (length altium_delphi-str)
						(length match)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list allcomp))
	     ;; Wait for a keypress. Then delete *Completion*  window
	     (momentary-string-display "" (point))
	     (delete-window (get-buffer-window (get-buffer "*Completions*")))
	     )))))

(defun altium_delphi-show-completions ()
  "Show all possible completions at current point."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (altium_delphi-str (buffer-substring b e))
	 ;; The following variable is used in altium_delphi-completion
	 (altium_delphi-buffer-to-use (current-buffer))
	 (allcomp (if (and altium_delphi-toggle-completions
			   (string= altium_delphi-last-word-shown altium_delphi-str))
		      altium_delphi-last-completions
		    (all-completions altium_delphi-str 'altium_delphi-completion))))
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list allcomp))
    ;; Wait for a keypress. Then delete *Completion*  window
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Completions*")))))


(defun altium_delphi-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
			(skip-chars-backward " \t")
			(skip-chars-backward "a-zA-Z0-9_")
			(point))
		      (progn
			(skip-chars-forward "a-zA-Z0-9_")
			(point)))))

(defun altium_delphi-build-defun-re (str &optional arg)
  "Return function/procedure starting with STR as regular expression.
With optional second arg non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "\\)\\>")
    (concat "^\\(function\\|procedure\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun altium_delphi-comp-defun (altium_delphi-str altium_delphi-pred altium_delphi-flag)
  (save-excursion
    (let ((altium_delphi-all nil)
	  match)

      ;; Set buffer to use for searching labels. This should be set
      ;; within functions which use altium_delphi-completions
      (set-buffer altium_delphi-buffer-to-use)

      (let ((altium_delphi-str altium_delphi-str))
	;; Build regular expression for functions
	(if (string= altium_delphi-str "")
	    (setq altium_delphi-str (altium_delphi-build-defun-re "[a-zA-Z_]"))
	  (setq altium_delphi-str (altium_delphi-build-defun-re altium_delphi-str)))
	(goto-char (point-min))
      
	;; Build a list of all possible completions
	(while (re-search-forward altium_delphi-str nil t)
	  (setq match (buffer-substring (match-beginning 2) (match-end 2)))
	  (if (or (null altium_delphi-pred)
		  (funcall altium_delphi-pred match))
	      (setq altium_delphi-all (cons match altium_delphi-all)))))

      ;; Now we have built a list of all matches. Give response to caller
      (altium_delphi-completion-response))))

(defun altium_delphi-goto-defun ()
  "Move to specified Altium_Delphi function/procedure.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (altium_delphi-get-default-symbol))
	 ;; The following variable is used in altium_delphi-comp-function
	 (altium_delphi-buffer-to-use (current-buffer))
	 (default (if (altium_delphi-comp-defun default nil 'lambda)
		      default ""))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Label: (default " default ") ")
				     'altium_delphi-comp-defun nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Label: "
				   'altium_delphi-comp-defun nil t ""))))
    ;; If there was no response on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (goto-char (point-min))
	  (re-search-forward (altium_delphi-build-defun-re label t))
	  (beginning-of-line)))))



;;;
;;; Altium_Delphi-outline-mode
;;;
(defvar altium_delphi-outline-map nil "Keymap used in Altium_Delphi Outline mode.")

(if altium_delphi-outline-map
    nil
  (if (boundp 'set-keymap-name)
      (set-keymap-name altium_delphi-outline-map 'altium_delphi-outline-map))
  (if (not (boundp 'set-keymap-parent))
      (setq altium_delphi-outline-map (copy-keymap altium_delphi-mode-map))
    (setq altium_delphi-outline-map (make-sparse-keymap))
    (set-keymap-parent altium_delphi-outline-map altium_delphi-mode-map))
  (define-key altium_delphi-outline-map "\M-\C-a"  'altium_delphi-outline-prev-defun)
  (define-key altium_delphi-outline-map "\M-\C-e"  'altium_delphi-outline-next-defun)
  (define-key altium_delphi-outline-map "\C-c\C-d" 'altium_delphi-outline-goto-defun)
  (define-key altium_delphi-outline-map "\C-c\C-s" 'altium_delphi-show-all)
  (define-key altium_delphi-outline-map "\C-c\C-h" 'altium_delphi-hide-other-defuns))

(defvar altium_delphi-outline-mode nil "Non-nil while using Altium_Delphi Outline mode.")
(make-variable-buffer-local 'altium_delphi-outline-mode)
(set-default 'altium_delphi-outline-mode nil)
(if (not (assoc 'altium_delphi-outline-mode minor-mode-alist))
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(altium_delphi-outline-mode " Outl")))))

(defun altium_delphi-outline (&optional arg)
  "Outline-line minor mode for Altium_Delphi mode.
When in Altium_Delphi Outline mode, portions
of the text being edited may be made invisible. \\<altium_delphi-outline-map>

Altium_Delphi Outline mode provides some additional commands.

\\[altium_delphi-outline-prev-defun]\
\t- Move to previous function/procedure, hiding everything else.
\\[altium_delphi-outline-next-defun]\
\t- Move to next function/procedure, hiding everything else.
\\[altium_delphi-outline-goto-defun]\
\t- Goto function/procedure prompted for in minibuffer,
\t  hide all other functions.
\\[altium_delphi-show-all]\t- Show the whole buffer.
\\[altium_delphi-hide-other-defuns]\
\t- Hide everything but the current function (function under the cursor).
\\[altium_delphi-outline]\t- Leave altium_delphi-outline-mode."
  (interactive "P")
  (setq altium_delphi-outline-mode
	(if (null arg) (not altium_delphi-outline-mode) t))
  (if (boundp 'redraw-mode-line)
      (redraw-mode-line))
  (if altium_delphi-outline-mode
      (progn
	(setq selective-display t)
	(use-local-map altium_delphi-outline-map))
    (progn
      (setq selective-display nil)
      (altium_delphi-show-all)
      (use-local-map altium_delphi-mode-map))))

(defun altium_delphi-outline-change (b e altium_delphi-flag)
  (let ((modp (buffer-modified-p)))
    (unwind-protect
	(subst-char-in-region b e (if (= altium_delphi-flag ?\n) 
				      ?\^M ?\n) altium_delphi-flag)
      (set-buffer-modified-p modp))))

(defun altium_delphi-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (altium_delphi-outline-change (point-min) (point-max) ?\n))

(defun altium_delphi-hide-other-defuns ()
  "Show only the current defun."
  (interactive)
  (save-excursion
    (let ((beg (progn (if (not (looking-at "\\(function\\|procedure\\)\\>"))
			  (altium_delphi-beg-of-defun))
		      (point)))
	  (end (progn (altium_delphi-end-of-defun)
		      (backward-sexp 1)
		      (search-forward "\n\\|\^M" nil t)
		      (point)))
	  (opoint (point-min)))
      (goto-char (point-min))

      ;; Hide all functions before current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" beg 'move)
	(altium_delphi-outline-change opoint (1- (match-beginning 0)) ?\^M)
	(setq opoint (point))
	;; Functions may be nested
	(if (> (progn (altium_delphi-end-of-defun) (point)) beg)
	    (goto-char opoint)))
      (if (> beg opoint)
	  (altium_delphi-outline-change opoint (1- beg) ?\^M))

      ;; Show current function
      (altium_delphi-outline-change beg end ?\n)
      ;; Hide nested functions
      (forward-char 1)
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" end 'move)
	(setq opoint (point))
	(altium_delphi-end-of-defun)
	(altium_delphi-outline-change opoint (point) ?\^M))

      (goto-char end)
      (setq opoint end)

      ;; Hide all function after current function
      (while (re-search-forward "^\\(function\\|procedure\\)\\>" nil 'move)
	(altium_delphi-outline-change opoint (1- (match-beginning 0)) ?\^M)
	(setq opoint (point))
	(altium_delphi-end-of-defun))
      (altium_delphi-outline-change opoint (point-max) ?\^M)

      ;; Hide main program
      (if (< (progn (forward-line -1) (point)) end)
	  (progn
	    (goto-char beg)
	    (altium_delphi-end-of-defun)
	    (backward-sexp 1)
	    (altium_delphi-outline-change (point) (point-max) ?\^M))))))

(defun altium_delphi-outline-next-defun ()
  "Move to next function/procedure, hiding all others."
  (interactive)
  (altium_delphi-end-of-defun)
  (altium_delphi-hide-other-defuns))

(defun altium_delphi-outline-prev-defun ()
  "Move to previous function/procedure, hiding all others."
  (interactive)
  (altium_delphi-beg-of-defun)
  (altium_delphi-hide-other-defuns))

(defun altium_delphi-outline-goto-defun ()
  "Move to specified function/procedure, hiding all others."
  (interactive)
  (altium_delphi-goto-defun)
  (altium_delphi-hide-other-defuns))

;; XEmacs addition
;;;###autoload(add-to-list 'auto-mode-alist '("\\.p\\(?:as\\)?\\'" . altium_delphi-mode))

;;; altium_delphi.el ends here
