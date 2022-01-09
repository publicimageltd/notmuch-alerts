;;; notmuch-alert.el --- Use notmuch bookmarks as alerts  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021

;; Author:  <joerg@joergvolbers.de>
;; Package-Requires: ((seq "2.20") (emacs "26.1") (notmuch "0.1") (notmuch-bookmarks "0.1"))
;; Version: 0.1
;; Keywords: mail
;; URL: https://github.com/publicimageltd/notmuch-alerts

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows to check bookmarked notmuch queries for 'new'
;; or 'unread' mails. It provides an API to visit, create, install and
;; remove alerts. Alerts are created 'on top' of existing notmuch
;; bookmarks.
;;
;; To activate the package, turn on the global minor mode
;; `notmuch-alert-mode'.
;;
;; An alert simply is a function receiving one argument, a bookmark.
;; It is up to the function to build the query (using the bookmark)
;; and to check for matching mails, etc. The return value of the alert
;; function must be either nil, indicating that nothing happened
;; ("inactive alert"), or the number of relevant mails matched
;; ("active alert").
;;
;; This package provides some helper functions to construct alerts,
;; e.g. to count mails.

;; This file is NOT part of notmuch or the notmuch Emacs suite.

;;; Code:

(require 'notmuch)
(require 'notmuch-bookmarks)
(require 'cl-lib)
(require 'subr-x)
(require 'rx)

;;; * Compatibility stuff

(defalias 'notmuch-alert-seq-contains-p
  (if (fboundp 'seq-contains-p)
      'seq-contains-p
    'seq-contains))

;; Silence Byte Compiler

(defvar selectrum-should-sort)
(defvar ivy-minibuffer-map)
(defvar ivy-mode)
(defvar ivy-sort-functions-alist)

;;; * Global Variables

(defcustom notmuch-alert-bmenu-filter-key "A"
  "Define this key in the bookmarks menu to filter out notmuch alerts.
If this value is nil, do not implement any key."
  :type 'string
  :group 'notmuch-alert)

(defcustom notmuch-alert-change-buffer-titles t
  "Use a more informative buffer title when showing messages."
  :type 'boolean
  :group 'notmuch-alert)

(defcustom notmuch-alert-visit-quit-when-pressed-twice t
  "Quit `notmuch-alert-visit' when the key it is bound to is pressed again."
  :type 'boolean
  :group 'notmuch-alert)

(defvar notmuch-alert-bmenu-original-keymap nil
  "Storage for original keymap of `bookmarks-bmenu'.")

(defvar notmuch-alert-bm-prettyprint-scheme
  '((notmuch-alert-pprinter--state       (:width 8))
    (" ")
    (notmuch-alert-pprinter--count       (:width 8))
    (" ")
    (notmuch-alert-pprinter--bm-name     (:width 40))
    (notmuch-alert-pprinter--alert-info  (:concat-if "."))
    (" ")
    (notmuch-alert-pprinter--bm-tare))
  "Scheme to pretty print bookmark items in interactive selections.

The value is list of lists. Each inner list item consists of two
elements maximum.

The first element (the car) is mandatory: it is the name of a
pretty printing function. This function receives two arguments,
the bookmark object and its associated alert, if any. It returns
a string.

The second, optional element of the inner list can be used to
specify some further modifications of the string. This second
element is a property list.

Schematic example:

   ((functionname (:prop val :prop2 val2 ...)
    (functionname)
    (functionname (:prop val))))

See also `notmuch-alert-pp-column' for a list of the allowed
properties.")

;;; * Easier API for completion

;; This is basically a copy of acomplete.el. It is included as to
;; reduce dependencies for this package.

(defun notmuch-alert--propertize (s data)
  "Add DATA as a text property to S."
  (propertize s 'data data))

(defun notmuch-alert--format (string-fn s &rest args)
  "Format S using STRING-FN.

If STRING-FN is a format string, pass it to `format' with
arguments S and further ARGS."
  (let* ((fn (cond
	      ((functionp string-fn) string-fn)
	      ((stringp   string-fn) (apply-partially #'format string-fn))
	      (t                     #'identity))))
    (apply fn s args)))

(cl-defun notmuch-alert--complete (prompt collection
					  &key (string-fn "%s")
					  (data-fn #'identity)
					  (require-match t))
  "Offer completion on COLLECTION.

COLLECTION is a data list. It is mapped to STRING-FN to create
choices for the user. 

 If STRING-FN is a string, it is used as a format string with the
item as its single argument.

DATA-FN maps a data item to each item of the collection.

Return the selected data item (not its string representation)."
  (let* (;; create a-collection:
	 (string-list (mapcar (apply-partially #'notmuch-alert--format string-fn) collection))
	 (data-list   (mapcar data-fn collection))
	 (a-collection (seq-mapn #'cons string-list data-list))
	 ;; doesn't hurt if selectrum is not installed, does it?
	 (selectrum-should-sort nil)
	 ;; let the user select:
	 (result   (completing-read prompt
				    a-collection
				    nil
				    require-match))
	 ;; ;; extract the data from the text property:
	 ;; (result-data (get-text-property 0 'data result)))
	 (result-data (alist-get result a-collection nil nil #'string=)))
    (if (or require-match result-data)
	result-data
      result)))

;;; * Alert Object

;; This structure defines an alert. An alert can be hooked into a
;; bookmark by associating it with the property `alert'. The alert can
;; then be "checked" via its bookmark. When checking, the bookmark
;; query, in conjunction with the alert's filter query, is used to
;; count any "relevant" mails. This result is then passed to the alert
;; operator, which determines wether the alert is `active' or not. The
;; operator function (with the number of counted mails as its
;; argument) just returns a status: either nil, meaning "inactive
;; alert", or t, meaning "alert should be raised, there are 'new'
;; mails". This boolean status will be stored in a separate slot of
;; the alert object for reuse.

(cl-defstruct notmuch-alert
  ;; Data slots which change after each update of the alert:
  count
  status
  ;; All for deciding about the status:
  filter
  (operator #'notmuch-alert-count>0-p)
  ;; Offer standardized information:
  type                ;; symbol for the type class; not used yet 
  description         ;; Description for status string
  format-string)      ;; String passed to format with one string argument (i.e. '%s uncount mails')

;; Alert operators:

(defun notmuch-alert-count>0-p (count)
  "Check for COUNT being defined and > 0."
  (when count (> count 0)))

;; Predefine alerts:

(defun notmuch-alert-unread ()
  "Create a new 'unread mails' alert object."
  (make-notmuch-alert :filter "tag:unread"
		      :type  'unread
		      :description "Check for unread mails"
		      :format-string "%d unread mails"))

(defun notmuch-alert-any ()
  "Create a new 'any mails' alert object."
  (make-notmuch-alert :type  'any
		      :description "Check for any mails"
		      :format-string "%d mails"))

(defun notmuch-alert-today ()
  "Create a new 'mails today' alert object."
  (make-notmuch-alert :filter "date:today"
		      :type 'today
		      :description "Check for mails from today"
		      :format-string "%d mails from today"))

(defun notmuch-alert-custom-tags ()
  "Create a new alert object with undefined custom tags."
  (make-notmuch-alert :filter nil
		      :type 'custom
		      :description "Create an alert"
		      :format-string nil))

(defvar notmuch-alerts
  '(notmuch-alert-unread
    notmuch-alert-any
    notmuch-alert-today
    notmuch-alert-custom-tags)
  "List of alerts offered by `notmuch-alert-install'.

Each element is a function name. The function returns an
alert object.")

;;; * Stuffing Alerts into Bookmarks:

(defun notmuch-alert-maybe-save-bookmark-alist ()
  "Increase bookmark modification counter and possibly save the bookmark alist."
  (setq bookmark-alist-modification-count
	(+ 1 bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p)
    (bookmark-save)))

(defun notmuch-alert-set (bookmark alert)
  "Install the alert object ALERT in BOOKMARK."
  (bookmark-prop-set bookmark 'alert alert)
  (notmuch-alert-maybe-save-bookmark-alist))

(defun notmuch-alert-set-via-buffer (alert &optional buffer)
  "Install ALERT in the bookmark associated with BUFFER or current buffer.
Returns the installed alert or nil if someting went wrong."
  (when-let* ((bm (notmuch-bookmarks-get-buffer-bookmark buffer)))
    (notmuch-alert-set bm alert)
    alert))

(defun notmuch-alert-get (bookmark)
  "Get the alert object of BOOKMARK."
  (bookmark-prop-get bookmark 'alert))

(defun notmuch-alert-get-via-buffer (&optional buffer)
  "Return the BUFFER's alert, if defined."
  (when-let* ((bm (notmuch-bookmarks-get-buffer-bookmark buffer)))
    (notmuch-alert-get bm)))

(defun notmuch-alert-bm-has-alert-p (bookmark)
  "Check if BOOKMARK has an alert object."
  (and (notmuch-bookmarks-record-p bookmark)
       (not (null (notmuch-alert-get bookmark)))))

(defun notmuch-alert-bm-has-no-alert-p (bookmark)
  "Check if BOOKMARK has no alert object."
  (and (notmuch-bookmarks-record-p bookmark)
       (not (notmuch-alert-get bookmark))))

(defun notmuch-alert-bm-has-active-alert-p (bookmark)
  "Check if BOOKMARK has an active alert object."
  (and (notmuch-bookmarks-record-p bookmark)
       (when-let* ((alert (notmuch-alert-get bookmark)))
	 (notmuch-alert-status alert))))

(defun notmuch-alert-bm-has-inactive-alert-p (bookmark)
  "True if BOOKMARK as an inactive alert."
  (and (notmuch-bookmarks-record-p bookmark)
       (when-let* ((alert (notmuch-alert-get bookmark)))
	 (not (notmuch-alert-status alert)))))

(defun notmuch-alert-filter-record (bookmark keys)
  "Return BOOKMARK without the slots named by KEYS."
  (let* ((name   (cl-first bookmark))
	 (record (cl-second bookmark)))
    (cons name
	  (list
	   (seq-filter
	    (lambda (keyval)
	      (not (notmuch-alert-seq-contains-p keys (car keyval))))
	    record)))))

(defun notmuch-alert-remove-alert (bookmark)
  "Remove BOOKMARK's alert object."
  (notmuch-alert-set-tare bookmark nil)
  (notmuch-alert-set bookmark nil))

(defun notmuch-alert-remove-via-buffer (&optional buffer)
  "Remove BUFFER's bookmark, if defined."
  (when-let* ((bm (notmuch-bookmarks-get-buffer-bookmark buffer)))
    (notmuch-alert-remove-alert bm)))

(defun notmuch-alert-set-tare (bookmark tare)
  "Set TARE for BOOKMARK."
  (bookmark-prop-set bookmark 'tara tare)
  (notmuch-alert-maybe-save-bookmark-alist))

(defun notmuch-alert-get-tare (bookmark)
  "Get the tare for BOOKMARK."
  (bookmark-prop-get bookmark 'tara))

(defun notmuch-alert-status-via-bookmark (bookmark)
  "Get the BOOKMARK's alert's status."
  (notmuch-alert-status (notmuch-alert-get bookmark)))

;;; * Updating Alerts:

(defun notmuch-alert-full-query (bookmark &optional filter)
  "Build a query joining BOOKMARK's query and the FILTER query."
  (concat
   (when filter "(")
   (notmuch-bookmarks-query bookmark)
   (when filter ")")
   (when filter
     (concat " AND " filter))))

(defun notmuch-alert-notmuch-count (query)
  "Count the number of mails matching QUERY."
  (string-to-number
   (notmuch-command-to-string "count" query)))
  
(defun notmuch-alert-do-count (bookmark &optional filter no-correction)
  "Count the mails matched by BOOKMARK and an additional extra FILTER.
Correct the count according to the tara value of bookmark, if
defined. This correction can be turned of by setting NO-CORRECTION."
  (let* ((last-count (bookmark-prop-get bookmark 'tara))
	 (query      (notmuch-alert-full-query bookmark filter))
	 (count      (notmuch-alert-notmuch-count query)))
    (if no-correction
	count
    (max (- count (or last-count 0)) 0))))

(defun notmuch-alert-update (bookmark)
  "Update the alert of BOOKMARK and return its updated status."
  (if-let* ((alert (notmuch-alert-get bookmark)))
      (let* ((query   (notmuch-alert-filter alert))
	     (count   (notmuch-alert-do-count bookmark query))
	     (fn      (notmuch-alert-operator alert))
	     (status  (funcall fn count)))
	(setf (notmuch-alert-count alert)  count)
	(setf (notmuch-alert-status alert) status)
	status)
    (user-error "No alert defined for bookmark '%s'"
		(bookmark-name-from-full-record bookmark))))

(defun notmuch-alert-update-all ()
  "Update all alerts and return all bookmarks with active alerts."
  (seq-filter #'notmuch-alert-update (seq-filter #'notmuch-alert-bm-has-alert-p bookmark-alist)))

;;; * Prettyprinting Alerts and Bookmarks

;; The functions for the columns:

(defface notmuch-alert-state-bookmark
  '((t . (:inherit font-lock-builtin-face)))
  "Face for the status string 'BOOKMARK'."
  :group 'notmuch-alert)

(defface notmuch-alert-state-active-alert
  '((t . (:foreground "green" :weight bold)))
  "Face for the status string 'ACTIVE'."
  :group 'notmuch-alert)

(defface notmuch-alert-state-inactive-alert
  '((t . (:foreground "dimgrey" :weight bold)))
  "Face for the status string 'INACTIVE'."
  :group 'notmuch-alert)

(defun notmuch-alert-pprinter--state (bookmark alert)
  "Pretty print BOOKMARK: Return status of ALERT as a string."
  (ignore bookmark)
  (if (not alert)
      (propertize "BOOKMARK" 'face 'notmuch-alert-state-bookmark)
    (if (notmuch-alert-status alert)
	(propertize "ACTIVE" 'face 'notmuch-alert-state-active-alert)
      (propertize "INACTIVE" 'face 'notmuch-alert-state-inactive-alert))))

(defun notmuch-alert-pprinter--count (bookmark alert)
  "Pretty print BOOKMARK: Return matching count of ALERT.
If an alert exists, just display its current count, else query
the notmuch database directly."
  (let* ((count (if alert
		    (notmuch-alert-count alert)
		  (notmuch-alert-notmuch-count (notmuch-bookmarks-query bookmark)))))
    (concat "(" (notmuch-hello-nice-number count) ")")))

(defun notmuch-alert-pprinter--alert-info (bookmark alert)
  "Pretty print BOOKMARK: Return information on its ALERT."
  (ignore bookmark)
  (when alert
    ;; TODO Durch eigene Berechnung der Anzeige ersetzen,
    ;; dann auch mit propertized string (...-active-state)
    (notmuch-alert-status-string alert
				 (notmuch-alert-description alert))))

(defun notmuch-alert-pprinter--bm-name (bookmark alert)
  "Pretty print BOOKMARK: Return BOOKMARK's name.
ALERT is ignored."
  (ignore alert)
  (bookmark-name-from-full-record bookmark))

(defun notmuch-alert-pprinter--bm-tare (bookmark alert)
  "Pretty print BOOKMARK: Return tare of ALERT."
  (ignore alert)
  (when-let* ((tare (notmuch-alert-get-tare bookmark)))
    (unless (<= tare 0)
      (format "Tare set to %d." tare))))

;; The pretty printing infrastructure:

(defun notmuch-alert-pp-column (bookmark pprinter mods)
  "Pretty print BOOKMARK by passing it to PPRINTER and applying MODS.

The function PPRINTER accepts two arguments, the bookmark object
and an alert object (which might be nil).

As a special case, PPRINTER can also be a string, which is then
printed unchanged.

MODS is a property list and should be either NIL or one of the
following:

 (:width <n>)            ;; restrict or pad output to <n> characters
 (:face (facespec))      ;; print string with this face
 (:concat-if \"string\") ;; if return value is nonempty, add \"string\""
  (let* ((alert (notmuch-alert-get bookmark))
	 (s     (if (stringp pprinter) pprinter
		  (funcall pprinter bookmark alert))))
    (if (null mods)
	s
      ;; mod 'concat-if':
      (when-let* ((concat-if (plist-get mods :concat-if)))
	(when s
	  (setq s (concat s concat-if))))
      ;; mod 'width':
      (when-let* ((width (plist-get mods :width)))
	(let* ((pad (- width (string-width s))))
	  (if (<= pad 0)
	      (setq s (substring s 0 width))
	    (setq s (concat s (make-string pad ?\s))))))
      ;; mod 'face':
      (when-let* ((face-specs (plist-get mods :face)))
	(setq s (funcall #'propertize s 'face face-specs))))
    s))

(defun notmuch-alert-pp-line (scheme bookmark)
  "Pretty print BOOKMARK using the columns defined in SCHEME.
SCHEME is a list of cells, each defining a new column (see
`notmuch-alert-pp-column'.)
Returns a string."
  (string-join
   (seq-map (lambda (scheme-spec)
	      (notmuch-alert-pp-column bookmark (cl-first scheme-spec) (cl-second scheme-spec)))
	    scheme)))


;;; * Information on alerts

(defun notmuch-alert-status-string (alert &optional inactive-string prefix)
  "Return the status of ALERT as an informative string.
If alert is not active, return INACTIVE-STRING.
Prepend the string PREFIX if it is not nil."
  (if (notmuch-alert-status alert)
      (format (concat prefix (notmuch-alert-format-string alert))
	      (notmuch-alert-count alert))
    inactive-string))

(defun notmuch-alert-display-info (bookmark alert)
  "Display information about BOOKMARK's ALERT in the echo area."
  (let* ((status (notmuch-alert-status alert))
	 (tare   (or (notmuch-alert-get-tare bookmark) 0)))
    (message
     (concat "Alert"
	     " '" (or (notmuch-alert-description alert) "no description") "' "
	     "is " (if status "ACTIVE" "INACTIVE")
	     ". "
	     (notmuch-alert-status-string alert nil "Counted ")
	     (when (> tare 0)
	       (format " (ignoring %s mails as tare)" tare))
	     "."))))

(defun notmuch-alert-set-buffer-name-if-unique (buffer name)
  "Set BUFFER's name to NAME if it is not already used by some other buffer."
  (unless (notmuch-alert-seq-contains-p (seq-map #'buffer-name (buffer-list)) name #'string-equal)
    (with-current-buffer buffer
      (rename-buffer name))))

(defun notmuch-alert-set-sensible-buffer-name ()
  "Set a better buffer name for the currently visited notmuch show buffer."
  ;; FIXME Only works for `notmuch-show' because of notmuch's strange
  ;; buffer handling. See `notmuch-search' as an example. Basically,
  ;; notmuch assumes that every search buffer can be identified via
  ;; its name. Refreshing the buffer finds the buffer (!) via that
  ;; function.
  (when (and notmuch-alert-change-buffer-titles
	     (eq major-mode 'notmuch-show-mode))
    (let* ((from (notmuch-show-get-from))
	   (subject (notmuch-show-get-subject))
	   (new-name (format "%s: %s" from subject)))
      (notmuch-alert-set-buffer-name-if-unique (current-buffer) new-name))))

;;; * Easy setting of tags to include or exclude mails from an alert:

(defvar notmuch-alert-single-tag-regexp 
  (rx (: (or "is" "tag") ":" (group (one-or-more (not space)))))
  "Regexp matching a simple notmuch tag search term.")

(defvar notmuch-alert-complex-syntax-regexp
  (rx (: (or "is" "tag") ":" (one-or-more (any "\"" ")" "(" ))))
  "Regexp matching complex notmuch search terms.
This regexp is used to weed out notmuch search term expressions
which are too complex. In particular, this applies to delimiters
such as parentheses or quotation marks, e.g. `tag:\\(pizza
bob\\)'. Parsing such expressions requires to parse the whole
expression syntactically.")

(defun notmuch-alert-query-to-taglist (query)
  "Convert the string QUERY into a list of tags."
  (if (or (string-empty-p query) (null query))
      (user-error "Alert has no filter query"))
  (if (string-match notmuch-alert-complex-syntax-regexp query)
      (user-error "Query '%s' cannot be parsed for tagging" query))
  (notmuch-alert--do-reduce-query nil query))

(defun notmuch-alert--do-reduce-query (acc rest-query)
  "Recursively find tags in REST-QUERY and collect them in ACC."
  (if (null rest-query)
      acc
    (if (string-match notmuch-alert-single-tag-regexp rest-query)
	(let* ((end (match-end 1))
	       (sub (match-string 1 rest-query)))
	  (notmuch-alert--do-reduce-query
	   (append (or acc) (list sub))
	   (unless (= end (length rest-query))
	     (substring rest-query end))))
      acc)))

;;;###autoload
(defun notmuch-alert-remove-filter-tags (&optional set)
  "Remove filter tags from the current mail.
If SET is non-nil, set the tags."
  (interactive "P")
  (let* ((alert        (or (notmuch-alert-get-via-buffer)
			   (user-error "No alert associated with the current buffer")))
	 (taglist      (or (notmuch-alert-query-to-taglist (notmuch-alert-filter alert))
			   (user-error "No tags defined in current alert's filter query")))
	 (tag-changes  (seq-map (lambda (s) (concat (if set "+" "-") s))
				taglist))
	 (tag-fn  (cl-case major-mode
		    ('notmuch-search-mode #'notmuch-search-tag)
		    ('notmuch-tree-mode   #'notmuch-tree-tag)
		    ('notmuch-show-mode   #'notmuch-show-tag))))
    (funcall tag-fn tag-changes)))

;;; * Interactive Functions

(defun notmuch-alert-display-info-via-buffer (&optional buf)
  "Display information about BUF's alert in the echo area."
  (let* ((alert (notmuch-alert-get-via-buffer buf))
	 (bm    (notmuch-bookmarks-get-buffer-bookmark buf)))
    (if alert
	(notmuch-alert-display-info bm alert)
      (user-error "No alert defined"))))

;;;###autoload
(defun notmuch-alert-display ()
  "Display information about the current buffer's alert."
  (interactive)
  (notmuch-alert-display-info-via-buffer))

(defun notmuch-alert-select-one (prompt alerts)
  "PROMPT the user to choose between ALERTS.
ALERTS is a list of alerts."
  (notmuch-alert--complete prompt alerts
	     :string-fn #'notmuch-alert-description))

(defun notmuch-alert-description-or-interactive ())

(defun notmuch-alert-get-minibuffer-map ()
  "Get the minibuffer map which will be used by `completing-read'.
Change this function to add completion backends."
  (if (and (featurep 'ivy)
	   ivy-mode)
      ivy-minibuffer-map
    minibuffer-local-map))

;;;###autoload
(defun notmuch-alert-visit ()
  "Jump to one of the bookmarks with an alert."
  (interactive)
  (let* (result backup
	 (keys   (this-command-keys)))
    (when notmuch-alert-visit-quit-when-pressed-twice
      ;;  modify map so that a repetition of the calling key sequence cancels the selection:
      (setq backup (lookup-key (notmuch-alert-get-minibuffer-map) keys))
      (define-key (notmuch-alert-get-minibuffer-map) keys 'minibuffer-keyboard-quit))
    ;; update alert counts:
    (notmuch-alert-update-all)
    ;; offer selection:
    (unwind-protect
	(let* ((collection (append
			    (seq-filter #'notmuch-alert-bm-has-active-alert-p bookmark-alist)
			    (seq-filter #'notmuch-alert-bm-has-inactive-alert-p bookmark-alist)
			    (seq-filter #'notmuch-alert-bm-has-no-alert-p bookmark-alist))))
	  (setq result (notmuch-alert--complete "Select bookmark: "
			 collection
			 :string-fn (apply-partially #'notmuch-alert-pp-line notmuch-alert-bm-prettyprint-scheme))))
      ;; repair keymap in cleanup form, irrespective of result:
      (when notmuch-alert-visit-quit-when-pressed-twice
	(define-key (notmuch-alert-get-minibuffer-map) keys backup))
      ;; also respond to selection in an already cleaned up environment:
      (when result
	(push-mark)
	(bookmark-jump result)))))

;; Useful macro for accessing an alert indirectly, via the current
;; buffer:
(defmacro notmuch-alert-with-current-buffer (bookmark-symbol alert-symbol &rest body)
  "Set ALERT-SYMBOL and BOOKMARK-SYMBOL and then execute BODY.

Use current buffer's bookmark and alert to bind BOOKMARK-SYMBOL
and ALERT-SYMBOL.

Throw an error if the current buffer is not bookmarked or has no
alert."
  (declare (indent 2)
	   (debug (symbolp symbolp &rest form)))
  (let* ((bm-sym     (intern (symbol-name bookmark-symbol)))
	 (alert-sym  (intern (symbol-name alert-symbol))))
    `(let* ((,bm-sym (notmuch-bookmarks-get-buffer-bookmark)))
       (if (null ,bm-sym)
	   (user-error "Current buffer is not bookmarked")
	 (let* ((,alert-sym (notmuch-alert-get ,bm-sym)))
	   (if (null ,alert-sym)
	       (user-error "Current buffer's bookmark has no alert")
	     ,@body))))))

(defun notmuch-alert-strip-tag-prefix (s)
  "Strip prefix + or - from S."
  (if (string-match (rx string-start (group (one-or-more (or "+" "-")))) s)
      (substring s (match-end 1))
    s))

;;;###autoload
(defun notmuch-alert-install ()
  "Install an alert for the current buffer.
The available alerts are defined by `notmuch-alerts'."
  (interactive)
  (let* ((bm (notmuch-bookmarks-get-buffer-bookmark)))
    (if (null bm)
	(user-error "Current buffer is not bookmarked")
      (let* ((alert (notmuch-alert-select-one
		     "Set alert for current buffer:"
		     (seq-map #'funcall notmuch-alerts))))
	(if (null alert)
	    (user-error "Canceled")
	  ;;
	  ;; Special handling for custom alerts:
	  ;;
	  (when (eq (notmuch-alert-type alert) 'custom)
	    (let* ((taglist (seq-map #'notmuch-alert-strip-tag-prefix
				     (notmuch-read-tag-changes nil "Enter tags to match the filter query (+ and - will be ignored):\n"))))
	      (if (null taglist)
		  (user-error "Canceled.")
		(let* ((filter-query  (string-join (seq-map (lambda (s) (concat "tag:" s)) taglist)
						   " AND "))
		       (description   (format "Custom alert matching '%s'" filter-query))
		       (format-string (read-from-minibuffer (format "Filter query: %s.\nEnter format string for this alert (%%d=# of matches):"
								    filter-query))))
		  (if (string-empty-p format-string)
		      (user-error "No format string; canceled")
		    (setf (notmuch-alert-description alert) description)
		    (setf (notmuch-alert-format-string alert) format-string)
		    (setf (notmuch-alert-filter alert) filter-query))))))
	  ;;
	  (notmuch-alert-set bm alert)
	  (message "Current buffer's bookmark alert: '%s'"
		   (notmuch-alert-description alert)))))))

;;;###autoload
(defun notmuch-alert-uninstall ()
  "Uninstall an alert for the current buffer."
  (interactive)
  (notmuch-alert-with-current-buffer bm alert
    (notmuch-alert-remove-alert bm)
    (message "Alert removed")))

;;;###autoload
(defun notmuch-alert-check ()
  "Check current buffer's alert."
  (interactive)
  (notmuch-alert-with-current-buffer bm alert
    (if (notmuch-alert-update bm)
	(notmuch-alert-display-info bm alert)
      (message "Alert inactive"))))

;;;###autoload
(defun notmuch-alert-tare (&optional just-display)
  "Set the tare for the current buffer.
With prefix JUST-DISPLAY set to '(4), do not set the value, just
display it. With prefix JUSt-DISPLAY set to '(16), remove the
tare."
  (interactive "P")
  (notmuch-alert-with-current-buffer bm alert
    (let* ((tara       (notmuch-alert-get-tare bm)))
      (cl-case (car just-display)
	(16   (progn
		(notmuch-alert-set-tare bm 0)
		(message "Tare reset to 0.")))
	(4    (message (if tara
			   (format "Current tare is %d" tara)
			 "No tare set")))
	(t (progn
	     (notmuch-alert-set-tare bm (notmuch-alert-notmuch-count (notmuch-bookmarks-query bm)))
	     (message "Tare set to %d" (notmuch-alert-get-tare bm))))))))

;;; Convenience functions

(defun notmuch-alert-remove-all-alerts ()
  "Remove all notmuch alert bookmarks from the bookmark list."
  (setq bookmark-alist
	(seq-filter
	 (lambda (bm) (not (notmuch-alert-bm-has-alert-p bm)))
	 bookmark-alist)))


;; Hook into notmuch ecosystem

(defun notmuch-alert-mode-install (&optional uninstall)
  "Install alerting feature on top of notmuch bookmarks.
If UNINSTALL is non-nil, uninstall the feature.

This function should not be called directly. Use
`notmuch-alert-mode' instead."
  ;; install better buffer names:
  (let* ((hook-fn (if uninstall 'remove-hook 'add-hook)))
    (funcall hook-fn 'notmuch-show-hook
	     #'notmuch-alert-set-sensible-buffer-name))
  ;; set up for ivy completions:
  (unless uninstall
    (with-eval-after-load 'ivy
      (add-to-list 'ivy-sort-functions-alist '(notmuch-alert-visit))))
  ;; edit bmenu keymap:
  (when notmuch-alert-bmenu-filter-key
    (if uninstall
	(when notmuch-alert-bmenu-original-keymap
	  (setq bookmark-bmenu-mode-map
		notmuch-alert-bmenu-original-keymap))
      (setq notmuch-alert-bmenu-original-keymap
	    (copy-keymap bookmark-bmenu-mode-map))
      (define-key bookmark-bmenu-mode-map
	notmuch-alert-bmenu-filter-key
	'notmuch-alert-bmenu))))

(defun notmuch-alert-bmenu ()
  "Display bookmark menu only with notmuch alerts."
  (interactive)
  (let* ((bookmark-alist
	  (seq-filter #'notmuch-alert-bm-has-alert-p bookmark-alist)))
    (if (called-interactively-p 'interactive)
	(call-interactively #'bookmark-bmenu-list)
      (bookmark-bmenu-list))))

;;;###autoload
(define-minor-mode notmuch-alert-mode
  "Allow using notmuch bookmarks as alerts."
  :global t
  (notmuch-alert-mode-install (not notmuch-alert-mode)))

(provide 'notmuch-alert)
;;; notmuch-alert.el ends here
