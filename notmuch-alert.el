;;; notmuch-alert.el --- use notmuch bookmarks as alerts  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: mail

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

;; This package defines functionality to check bookmarked queries for
;; 'new' or 'unread' mails. It provides an API to create, check and
;; remove alerts, which hook into an existing bookmark.
;;
;; An alert simply is a function receiving one argument, a bookmark It
;; is the function's task to build the query (using the bookmark) and
;; to check for matching mails, etc. The return value of the alert
;; function must be either nil, indicating that nothing happened
;; ("inactive alert"), or the number of relevant mails matched
;; ("active alert").
;;
;; This package provides some functions which help to construct
;; alerts, e.g. to count mails.

;; This file is NOT part of notmuch or the notmuch emacs suite.

;;; Code:

(require 'notmuch)
(require 'notmuch-bookmarks)
(require 'acomplete)
;;; * Global Variables

(defvar notmuch-alert-bm-prettyprint-scheme
  nil
  "Scheme to pretty print bookmark items in interactive selections.
The value has to be a list with max. two elements. The first,
mandatory element is the name of a function. This function
recieves two arguments, the bookmark object and its associated
alert, if any; and returns a string. The second, optional element
is used to specify some further modifications of the string. This
second element is a property list. 

   ((functionname (:prop val :prop2 val2 ...)
    (functionname)
    (functionname (:prop val))))

The property list (second element) currently accepts two
properties. If you want to apply several modifications, use one
list with all properties and values combined.

  (:width <n>) - The total maximum size of the string, to which
  it is trimmed, if necessary.

  (:face (facespecs...)) - Face specifications which will be
  applied to the string using `propertize'.

See also `notmuch-alert-pp-column'.")

;;; * Alert Object

;; This structure defines an alert. An alert can be hooked into a
;; bookmark by associating it with the property `alert'. The alert can
;; then be "checked" via its bookmark. In checking, the bookmark
;; query, in conjunction with the alert's filter query, will be used
;; to count any "relevant" mails. This result is then passed to the
;; alert operator, which determines wether the alert is `active' or
;; not. The operator function (with the number of counted mails as its
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
  type
  description
  format-string)

;; Alert operators:

(defun notmuch-alert-count>0-p (count)
  "Check for COUNT being > 0."
  (when count (> count 0)))

;; Predefined alerts:

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

(defvar notmuch-alerts
  '(notmuch-alert-unread
    notmuch-alert-any
    notmuch-alert-today)
  "List of available alerts (for interactive selection).
Each element is a function name. The function has to return an
alert object.")

;;; * Stuffing Alerts into Bookmarks:

(defun notmuch-alert-maybe-save-bookmark-alist ()
  "Increase bookmark modification counter and possibly save the bookmark alist."
  (incf bookmark-alist-modification-count)
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
  (let* ((name   (first bookmark))
	 (record (second bookmark)))
    (cons name
	  (list
	   (seq-filter (lambda (keyval) (not (seq-contains keys (car keyval)))) record)))))

(defun notmuch-alert-remove-alert (bookmark)
  "Remove BOOKMARK's alert object."
  (notmuch-alert-set-tare bookmark nil)
  (notmuch-alert-set bookmark nil))

(defun notmuch-alert-remove-via-buffer (&optional buffer)
  "Remove BUFFER's bookmark, if defined."
  (when-let* ((bm (notmuch-bookmarks-get-buffer-bookmark buffer)))
    (notmuch-alert-remove-alert bm)))

(defun notmuch-alert-set-tare (bookmark tare)
  "Set the tare for BOOKMARK."
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
  "Query the notmuch database and return the result as an integer."
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
  "Face for the status string 'BOOKMARK'.")

(defface notmuch-alert-state-active-alert
  '((t . (:foreground "green" :weight bold)))
  "Face for the status string 'ACTIVE'.")

(defface notmuch-alert-state-inactive-alert
  '((t . (:foreground "dimgrey" :weight bold)))
  "Face for the status string 'INACTIVE'.")

(defun notmuch-alert-pprinter:state (bookmark alert)
  "Return colorized status string for BOOKMARK and ALERT."
  (if (not alert)
      (propertize "BOOKMARK" 'face 'notmuch-alert-state-bookmark)
    (if (notmuch-alert-status alert)
	(propertize "ACTIVE" 'face 'notmuch-alert-state-active-alert)
      (propertize "INACTIVE" 'face 'notmuch-alert-state-inactive-alert))))

(defun notmuch-alert-pprinter:count (bookmark alert)
  "Return the total count matching BOOKMARK and ALERT.
If an alert exists, just display its current count, else query
the notmuch database directly."
  (let* ((count (if alert
		    (notmuch-alert-count alert)
		  (notmuch-alert-notmuch-count (notmuch-bookmarks-query bookmark)))))
    (concat "(" (notmuch-hello-nice-number count) ")")))

(defun notmuch-alert-pprinter:alert-info (bookmark alert)
  "Return information on ALERT."
  (when alert
    ;; TODO Durch eigene Berechnung der Anzeige ersetzen,
    ;; dann auch mit propertized string (...-active-state)
    (notmuch-alert-status-string alert
				 (notmuch-alert-description alert))))

(defun notmuch-alert-pprinter:bm-name (bookmark alert)
  "Return the BOOKMARK's name."
  (bookmark-name-from-full-record bookmark))

(defun notmuch-alert-pprinter:bm-tare (bookmark alert)
  "Return information on the BOOKMARK's tare setting."
  (when-let* ((tare (notmuch-alert-get-tare bookmark)))
    (unless (<= tare 0)
      (format "Tare set to %d." tare))))

(setq notmuch-alert-bm-prettyprint-scheme
      '((notmuch-alert-pprinter:state       (:width 8))
	(" ")
	(notmuch-alert-pprinter:count       (:width 8))
	(" ")
	(notmuch-alert-pprinter:bm-name     (:width 40))
	(notmuch-alert-pprinter:alert-info  (:concat-if "."))
	(" ")
	(notmuch-alert-pprinter:bm-tare)))

;; The pretty printing infrastructure:

(defun notmuch-alert-pp-column (bookmark pprinter mods)
  "Pretty print BOOKMARK by passing it to PPRINTER and applying MODS.
PPRINTER is a function which accepts two arguments, the bookmark
and an alert object (which might be nil). 

As a special case, PPRINTER can also be a string, which is then
used directly.

MODS is a property list and should be either NIL or one of the
following:

 (:width <n>)
 (:face (facespec))
 (:concat-if \"string\")
"
  (let* ((alert (notmuch-alert-get bookmark))
	 (s     (if (stringp pprinter) pprinter
		  (funcall pprinter bookmark alert))))
    (if (null mods)
	s
      (when-let* ((concat-if (plist-get mods :concat-if)))
	(when s
	  (setq s (concat s concat-if))))
      (when-let* ((width (plist-get mods :width)))
	(let* ((pad (- width (string-width s))))
	  (if (<= pad 0)
	      (setq s (substring s 0 width))
	    (setq s (concat s (make-string pad ?\s))))))
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
	      (notmuch-alert-pp-column bookmark (first scheme-spec) (second scheme-spec)))
	    scheme)))

;; TODO Unused, delete me.
(defun notmuch-alert-bm-pp-all (scheme collection)
  "Convert a COLLECTION of bookmarks to a pretty printed list of strings, using SCHEME.
For the structure of scheme, see the documentation of
`notmuch-alert-bm-prettyprint-scheme'."
  (seq-map (apply-partially #'notmuch-alert-pp-line scheme) collection))


;;; * Information on alerts

(defun notmuch-alert-status-string (alert &optional inactive-string prefix)
  "Return the status of ALERT as an informative string.
If alert is not active, return INACTIVE-STRING."
  (if (notmuch-alert-status alert)
      (format (concat prefix (notmuch-alert-format-string alert))
	      (notmuch-alert-count alert))
    inactive-string))

(defun notmuch-alert-display-info (alert)
  "Display information about ALERT in the echo area."
  (let* ((status (notmuch-alert-status alert))
	 (tara   (or (notmuch-alert-tara alert) 0)))
    (message 
     (concat "Alert"
	     " '" (or (notmuch-alert-description alert) "no description") "' "
	     "is " (if status "ACTIVE" "INACTIVE")
	     ". "
	     (notmuch-alert-status-string alert nil "Counted ")
	     (when (> tara 0)
	       (format " (ignoring %s mails as tare)" tara))
	     "."
	     ))))

(defun notmuch-alert-set-sensible-buffer-name ()
  "Set a better buffer name for the currently visited notmuch show buffer."
  (when (eq major-mode 'notmuch-show-mode)
    (let* ((from (notmuch-show-get-from))
	   (subject (notmuch-show-get-subject))
	   (new-name (format "%s: %s" from subject))
	   (unique-p (null (get-buffer new-name))))
      (when unique-p
	(setf (buffer-name) new-name)))))

;;; * Interactive Functions

(defun notmuch-alert-display-info-via-buffer (&optional buf)
  "Display information about BUF's alert in the echo area."
  (let* ((alert (notmuch-alert-get-via-buffer buf)))
    (if alert
	(notmuch-alert-display-info alert)
      (user-error "No alert defined"))))

;;;###autoload 
(defun notmuch-alert-display ()
  "Display information about the current buffer's alert."
  (interactive)
  (notmuch-alert-display-info-via-buffer))

(defun notmuch-alert-select-one (prompt alerts)
  "Choose between one of the ALERTS."
  (acomplete prompt alerts
	     :string-fn #'notmuch-alert-description))

;;;###autoload
(defun notmuch-alert-visit-alert-bookmark ()
  "Jump to one of the bookmarks with an alert."
  (interactive)
  (notmuch-alert-update-all)
  (let* ((collection (append
		      (seq-filter #'notmuch-alert-bm-has-active-alert-p bookmark-alist)
		      (seq-filter #'notmuch-alert-bm-has-inactive-alert-p bookmark-alist)
		      (seq-filter (lambda (bm) (and (notmuch-bookmarks-record-p bm)
						    (not (notmuch-alert-bm-has-alert-p bm))))
				  bookmark-alist))))
    (bookmark-jump (acomplete "Select bookmark: " collection
			      :string-fn (apply-partially #'notmuch-alert-pp-line notmuch-alert-bm-prettyprint-scheme)))))

;; Useful macro for dealing with indirect access to alerts via the current buffer:
(defmacro notmuch-alert-with-current-buffer (bookmark-symbol alert-symbol &rest body)
  "Execute BODY with current buffer's bookmark and alert bound to BOOKMARK-SYMBOL and ALERT-SYMBOL."
  (declare (indent 2)
	   (debug (symbolp symbolp &rest form)))
  (let* ((_bm     (intern (symbol-name bookmark-symbol)))
	 (_alert  (intern (symbol-name alert-symbol))))
    `(let* ((,_bm (notmuch-bookmarks-get-buffer-bookmark)))
       (if (null ,_bm)
	   (user-error "Current buffer is not bookmarked")
	 (let* ((,_alert (notmuch-alert-get ,_bm)))
	   (if (null ,_alert)
	       (user-error "Current buffer's bookmark has no alert")
	     ,@body))))))

;;;###autoload
(defun notmuch-alert-install ()
  "Install an alert for the current buffer.
The available alerts are listed in `notmuch-alerts'."
  (interactive)
  (let* ((bm (notmuch-bookmarks-get-buffer-bookmark)))
    (if (null bm)
	(user-error "Current buffer is not bookmarked")
      (let* ((alert (notmuch-alert-select-one
		     "Set alert for current buffer:"
		     ;; always create new objects:
		     (seq-map #'funcall notmuch-alerts))))
	(if (null alert)
	    (user-error "Canceled")
	  (notmuch-alert-set bm alert)
	  (message "Set alert '%s'"
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
	(notmuch-alert-display-info alert)
      (message "Alert inactive"))))

;;;###autoload
(defun notmuch-alert-tare (&optional just-display)
  "Set the tare for the current buffer.
With prefix, just display the current value.
With double prefix, remove the tare."
  (interactive "P")
  (notmuch-alert-with-current-buffer bm alert
    (let* ((count      (or (notmuch-alert-count alert)
		 	    (notmuch-alert-update bm)))
	   (tara       (notmuch-alert-get-tare bm)))
      (cl-case (car just-display)
	(16   (progn
		(notmuch-alert-set-tare bm 0)
		(message "Tare reset to 0.")))
	(4    (message (if tara
			   (format "Current tare is %d" tara)
			 "No tare set")))
	(t (progn
	     (notmuch-alert-set-tare bm (notmuch-alert-notmuch-count (notmuch-bookmarks-query bm)))
	     (message "Tare set to %d" count)))))))

;;; Convenience functions

(defun notmuch-alert-copy-iff-bookmark (bookmark)
  "Copy bookmark if it is a notmuch bookmark, else pass it unchanged."
  (if (notmuch-bookmarks-record-p bookmark)
      (notmuch-bookmarks-copy-bookmark bookmark)
    bookmark))

(defun notmuch-alert-remove-all-alerts ()
  "Remove all ALERTS from the bookmark list, keeping the bookmarks."
  (setq bookmark-alist
	(seq-map #'notmuch-alert-copy-iff-bookmark bookmark-alist)))

;; Hook into notmuch ecosystem

(add-hook 'notmuch-show-hook #'notmuch-alert-set-sensible-buffer-name)
(add-to-list 'ivy-sort-functions-alist '(notmuch-alert-visit-alert-bookmark))

(provide 'notmuch-alert)
;;; notmuch-alert.el ends here
