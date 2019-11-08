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


;; * Adding, Removing, Checking Alerts:

(defun notmuch-alert-set (bookmark alert)
  "Set fn ALERT in BOOKMARK as an alert."
  (bookmark-prop-set bookmark 'alert alert))

(defun notmuch-alert-set-via-buffer (alert &optional buffer)
  "Install ALERT in the bookmark associated with BUFFER.
Returns the installed alert or nil if someting went wrong."
  (when-let* ((bm (notmuch-bookmarks-get-buffer-bookmark buffer)))
    (notmuch-alert-set bm alert)
    alert))

(defun notmuch-alert-get (bookmark)
  "Get the alert of BOOKMARK."
  (bookmark-prop-get bookmark 'alert))

(defun notmuch-alert-p (bookmark)
  "Check if BOOKMARK is an alert."
  (and (notmuch-bookmarks-record-p bookmark)
       (not (null (notmuch-alert-get bookmark)))))

(defun notmuch-not-alert-p (bookmark)
  "Check that BOOKMARK is NOT an alert."
  (not (notmuch-alert-p bookmark)))

(defun notmuch-alert-to-bookmark (bookmark)
  "Change BOOKMARK so that it is no more an alert.
This function actually leaves BOOKMARK mostly unchanged, it just
removes the alert function."
  (bookmark-prop-set bookmark 'alert nil))

(defun notmuch-alert-query (bookmark)
  "Return the query associated with BOOKMARK."
  (cl-assert (notmuch-alert-p bookmark))
  (bookmark-prop-get bookmark 'filename))

(defun notmuch-alert-call (bookmark)
  "Call the alert of BOOKMARK and return its value."
    (when-let* ((alert-fn (notmuch-alert-get bookmark)))
      (funcall alert-fn bookmark)))

;;; * Alerts:

;; Helpful functions for alerts:

(defun notmuch-alert-count (bookmark &optional filter)
  "Count the mails matched by QUERY and an additional extra FILTER.

Correct count according to the tara value of bookmark, if defined."
  (let* ((last-count (bookmark-prop-get bookmark 'tara))
	 (query (concat
		 (when filter "(")
		 (notmuch-alert-query bookmark)
		 (when filter ")")
		 (when filter
		   (concat " AND " filter))))
	 (count (string-to-number
		 (notmuch-command-to-string "count" query))))
    (- count (or last-count 0))))

;; Some predefined alerts:

;;; TODO Ändern. Der Fehler hier ist: Es geht nicht um "types",
;;; sondern um spezifische Ergebnisse je nach Funktionsnamen. Also so
;;; vorgehen: Überlegen, was für fn-spezifische infos wir brauchen;
;;; dann eine Alist mit den entsprechenden Daten erstellen. Dann ein
;;; System, dass neue Alerts explizit "registriert" und sie so der
;;; Datenbank hinzufügt. Dann schließlich eine Funktion, die alle
;;; vorhandenen Bookmarks auf "legitime types" prüft und ggf. löscht.
;;; Außerdem wollen wir eine Auswahl aus den Alerts geben.
;;
;; ODER: Oder wir packen das alles in die Alert-Funktion. Aber das
;; wäre dann wie ein Objekt.
;; HMMMMM.
;;

(defun notmuch-alert-type-string (bookmark)
  "Return a string representing the alert type of BOOKMARK."
  (cl-case (notmuch-alert-get bookmark)
    ('notmuch-alert-when-unread "unread")
    ('notmuch-alert-any-new-alert "new")
    (t "(unknown alert type)")))

(defun notmuch-alert-when-unread (bookmark)
  "Return the number of unread mails of BOOKMARK."
  (notmuch-alert-count bookmark "tag:unread"))

(defun notmuch-alert-any-new (bookmark)
  "Return the number of any mails displayed in BOOKMARK."
  (notmuch-alert-count bookmark))

;;; * Interactive Functions to work with Alerts:

(defun notmuch-alert-install-unread-alert (&optional buffer interactive)
  "Install an unread alert in current buffer."
  (interactive (list (current-buffer) t))
  (if (notmuch-alert-set-via-buffer #'notmuch-alert-when-unread buffer)
      (message "Installed unread mails alert.")
    (when interactive
      (user-error "Current buffer is not bookmarked."))))

(defun notmuch-alert-check-alert (buffer &optional display)
  "Check the alert for BUFFER and optionally display its status."
  (interactive (list (current-buffer) t))
  (if-let* ((bm (notmuch-bookmarks-get-buffer-bookmark buffer)))
      (let* ((status (notmuch-alert-call bm)))
	(when (and status display)
	  (message (concat "%s " (notmuch-alert-type-string bm)) status))
	status)
    (when display
      (user-error "No bookmarked buffer."))))


(provide 'notmuch-alert)
;;; notmuch-alert.el ends here
