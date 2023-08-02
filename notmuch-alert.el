;;; notmuch-alert.el --- Use notmuch bookmarks as alerts  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021; rewrite 2023

;; Author:  <joerg@joergvolbers.de>
;; Package-Requires: ((seq "2.20") (emacs "26.1") (notmuch "0.1") (notmuch-bookmarks "0.2"))
;; Version: 0.2
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

;; TODO Mute-lösch Funktion an sinnvolle Orte einbauen:
;; TODO - Notmuch refresh (über advise)
;; TODO - Irgendwie "notmuch poll" von extern abfangen

;;; Code:

(require 'notmuch-bookmarks)
(require 'cl-lib)
(require 'map)

;;; Poor Man's Memoizing

(defvar notmuch-alert-counts nil
  "Alist storing all counts using bookmark name as key.
Set this variable to nil to enforce fresh counts when updating
the alerts.")

(defvar notmuch-alert-mute-counts nil
  "Alist storing all mute counts using the bookmark name as key.")

;;; Hook alerts into notmuch bookmarks

(defun notmuch-alert-p (bookmark-or-name)
  "Return non-nil if BOOKMARK-OR-NAME is a notmuch alert bookmark."
  (and (notmuch-bookmarks-record-p bookmark-or-name)
       (bookmark-prop-get bookmark-or-name 'alert)
       (functionp (bookmark-prop-get bookmark-or-name 'alert))))

(defun notmuch-alert-select-bookmark ()
  "Let the user select a bookmark."
  (completing-read " Select notmuch bookmark: "
                   (seq-filter #'notmuch-bookmarks-record-p bookmark-alist)))

(defun notmuch-alert-bookmark-name (bookmark-or-name)
  "Return the name associated with BOOKMARK-OR-NAME."
  (pcase bookmark-or-name
    ((pred stringp) bookmark-or-name)
    (`(,name . ,_)  name)
    (_   (error (format "Could not identify name for bookmark-or-name %S" bookmark-or-name)))))

(defun notmuch-alert-get-or-init-prop (bookmark-or-name prop val)
  "If PROP in BOOKMARK-OR-NAME is not defined, initialize it with VAL.
Return the value of PROP."
  (unless (assq prop (bookmark-get-bookmark-record bookmark-or-name))
    (bookmark-prop-set bookmark-or-name prop val))
  (bookmark-prop-get bookmark-or-name prop))

(defun notmuch-alert-remove-prop (bookmark-or-name prop)
  "Remove PROP from BOOKMARK-OR-NAME."
  (let* ((name (notmuch-alert-bookmark-name bookmark-or-name))
         (record (bookmark-get-bookmark-record name)))
    (bookmark-store name (assoc-delete-all prop record) nil)))

(defun notmuch-alert-get-alert (bookmark-or-name)
  "Return the alert function for BOOKMARK-OR-NAME or nil.
Also assert that the alert has the right format."
  (when-let ((alert (bookmark-prop-get bookmark-or-name 'alert)))
    ;; ignore alert objects installed by notmuch-alert v0.1
    (when (functionp alert)
      alert)))

(defun notmuch-alert-set-alert (bookmark-or-name fn)
  "Set FN as the alert function for BOOKMARK-OR-NAME."
  (bookmark-prop-set bookmark-or-name 'alert fn))

(defun notmuch-alert-get-filter (bookmark-or-name)
    "Return the filter string for BOOKMARK-OR-NAME.
If there is no filter string, return nil."
    (bookmark-prop-get bookmark-or-name 'filter))

(defun notmuch-alert-set-filter (bookmark-or-name filter)
  "Set the filter for BOOKMARK-OR-NAME to FILTER."
  (bookmark-prop-set bookmark-or-name 'filter filter))

(defun notmuch-alert-remove-filter (bookmark-or-name)
  "Remove any filter property for BOOKMARK-OR-NAME."
  (notmuch-alert-remove-prop bookmark-or-name 'filter))

(defun notmuch-alert-get-description (bookmark-or-name)
  "Return the description string for the alert stored in BOOKMARK-OR-NAME.
If there is no description, return nil."
  (bookmark-prop-get bookmark-or-name 'description))

(defun notmuch-alert-set-description (bookmark-or-name description)
  "Set DESCRIPTION for the alert stored in BOOKMARK-OR-NAME."
  (bookmark-prop-set bookmark-or-name 'description description))

(defun notmuch-alert-remove-description (bookmark-or-name)
  "Remove the description property of BOOKMARK-OR-NAME."
  (notmuch-alert-remove-prop bookmark-or-name 'description))

;;; Predicates for counting

(defun notmuch-alert--build-query (bookmark-or-name)
  "Build a query from the alert BOOKMARK-OR-NAME."
  (let ((filter (notmuch-alert-get-filter bookmark-or-name))
        (query  (bookmark-prop-get bookmark-or-name 'filename)))
    (when query
      (concat
       (when filter "(")
       query
       (when filter ")")
       (when filter
         (concat " AND " filter))))))

;; REVIEW Argument force is never used - is it useful at all?
(defun notmuch-alert-get-count (bookmark-or-name &optional force)
  "Count the number of mails for the alert in BOOKMARK-OR-NAME.
Note that this count also includes a filter restriction, if implemented.

Return the value stored in `notmuch-alert-counts'; if there is
none, call `notmuch query' and store the result. Set FORCE to a
non-nil value to force a fresh query.

Throw an error if BOOKMARK-OR-NAME has no query."
  (let* ((name (notmuch-alert-bookmark-name bookmark-or-name))
         (stored-count (map-elt notmuch-alert-counts name)))
    (if (and (not force) stored-count)
        stored-count
      (let ((query (notmuch-alert--build-query bookmark-or-name)))
        (unless query
          (user-error "Alert %s has no query" name))
        (let ((new-count (string-to-number (notmuch-command-to-string "count" query))))
          (setq notmuch-alert-counts
                (map-insert notmuch-alert-counts name new-count))
          new-count)))))

(defun notmuch-alert-get-mute-count (bookmark-or-name)
  "Return mute count for BOOKMARK-OR-NAME."
  (map-elt notmuch-alert-mute-counts
           (notmuch-alert-bookmark-name bookmark-or-name)))

(defun notmuch-alert-set-mute-count (bookmark-or-name val)
  "Set mute count for BOOKMARK-OR-NAME to the number VAL.
Called interactively, use the current buffer's bookmark and ask
for VAL."
  (interactive (list (or bookmark-current-bookmark (notmuch-alert-select-bookmark))
                     (read-number "Set mute count for this bookmark: "
                                  (notmuch-alert-get-count bookmark-current-bookmark))))
  (setq notmuch-alert-mute-counts
        (map-insert
         notmuch-alert-mute-counts
         (notmuch-alert-bookmark-name bookmark-or-name)
         val)))

(defun notmuch-alert-remove-mute-count (bookmark-or-name)
  "Remove mute count for BOOKMARK-OR-NAME."
  (setq notmuch-alert-mute-counts
        (map-delete notmuch-alert-mute-counts
                    (notmuch-alert-bookmark-name bookmark-or-name))))

(defun notmuch-alert-remove-all-mute-counts ()
  "Remove all mute counts."
  (setq notmuch-alert-mute-counts nil))

(defun notmuch-alert-count>0 (bookmark-or-name)
  "Check if BOOKMARK-OR-NAME counts more than zero mails."
  (let ((count (notmuch-alert-get-count bookmark-or-name))
        (mute-count (or (notmuch-alert-get-mute-count bookmark-or-name) 0)))
    (> (- count mute-count) 0)))

(defun notmuch-alert-install (bookmark-or-name &optional filter description interactively-p)
  "Install an alert for  notmuch bookmark BOOKMARK-OR-NAME.
The alert is active when the combined query associated with the
bookmark returns more than zero mails. Per default, use the
bookmarked query. Alternatively, add some additional search
criteria via FILTER, e.g. \"is: unread\". These additional
conditions only apply to the filter and leave the bookmarked
query untouched.

DESCRIPTION should qualify the mails the alert reacts on; e.g.
\"unread mails\" or \"mails from today\". It will be used to give
the user feedback on the alert in sentences likes \"Bookmark has
%s\". Setting no value defaults to \"mails\".

Set INTERACTIVELY-P to get feedback on what is being done.

Example usages:
  ;; bookmark name refers to the query \"is:unread\"
  ;; alert when there are more than 0 unread messages:
  (notmuch-alert-install \"notmuch: UNREAD\")
  ;; bookmark name refers to the query \"date:today\"
  ;; alert when there are anread messages for today:
  (notmuch-alert-install \"notmuch: today\" \"is:unread\" \"unread mails\")"
  (interactive (list (or bookmark-current-bookmark
                         (notmuch-alert-select-bookmark))
                     (notmuch-read-query " [Optional] Additional alert filter: ")
                     (read-string " [Optional] Description of what the alert counts: ")
                     t))
  ;;
  (when (string-blank-p filter) (setq filter nil))
  (when (string-blank-p description) (setq description nil))
  ;;
  (let ((name (notmuch-alert-bookmark-name bookmark-or-name)))
    (bookmark-prop-set name 'alert #'notmuch-alert-count>0)
    (when filter
      (notmuch-alert-set-filter name filter))
    (when description
      (notmuch-alert-set-description name description))
    (when interactively-p
      (message "Alert set (%s, %s)"
               (or filter "no filter")
               (or description "no description")))))

(defun notmuch-alert-uninstall (bookmark-or-name)
  "Uninstall any alert associated with BOOKMARK-OR-NAME."
  (interactive (list (or bookmark-current-bookmark
                         (notmuch-alert-select-bookmark))))
  (let ((name (notmuch-alert-bookmark-name bookmark-or-name)))
    (cl-dolist (prop '(alert filter description))
      (notmuch-alert-remove-prop name prop))
    (setq notmuch-alert-counts
          (map-delete notmuch-alert-counts name))
    (setq notmuch-alert-mute-counts
          (map-delete notmuch-alert-mute-counts name))))

(defun notmuch-alert-active-p (bookmark-or-name)
  "Return non-nil if BOOKMARK-OR-NAME's alert is active.
An alert is active when the combined query from bookmark and
search filter result in a mail count greater than zero.

If BOOKMARK-OR-NAME has no valid alert, return nil."
  (when-let ((pred (notmuch-alert-get-alert bookmark-or-name)))
    (funcall pred bookmark-or-name)))

(defun notmuch-alert-active-alerts ()
  "Return all active alerts."
  (thread-last
    (seq-filter #'notmuch-alert-p bookmark-alist)
    (seq-filter #'notmuch-alert-active-p)))

(defun notmuch-alert-update-all ()
  "Update all counts for all alerts."
  (setq notmuch-alert-counts nil)
  ;; NOTE This could be sped up using `notmuch --batch'
  (thread-last
    (seq-filter #'notmuch-alert-p bookmark-alist)
    (seq-filter #'notmuch-alert-get-count)))

;; User interaction

(defun notmuch-alert-remove-all ()
  "Destructively remove all alerts in the bookmark list."
  (interactive)
  (when (y-or-n-p " Remove all alerts from the current bookmark list? ")
    (let ((count 0))
      (cl-dolist (bookmark (mapcar #'car bookmark-alist))
        (when (bookmark-prop-get bookmark 'alert)
          (cl-dolist (prop '(alert filter description))
            (notmuch-alert-remove-prop bookmark prop))
          (cl-incf count)))
      (message "Removed %d alerts" count))))

(defun notmuch-alert-reset-notmuch-bookmarks ()
  "Aggresively reset all notmuch bookmarks to its bare functionality.
This function deletes all alerts and additionally also removes
any properties which are not used by `notmuch-bookmarks' > v0.2."
  (interactive)
  (when (y-or-n-p " Reset all notmuch bookmarks? ")
    (let ((count 0))
      (cl-dolist (bookmark (seq-filter #'notmuch-bookmarks-record-p bookmark-alist))
        (bookmark-store (car bookmark)
                        `((handler    . ,(bookmark-prop-get bookmark 'handler))
                          (filename   . ,(bookmark-prop-get bookmark 'filename))
                          (annotation . ,(bookmark-prop-get bookmark 'annotation))
                          (major-mode . ,(bookmark-prop-get bookmark 'major-mode)))
                        nil)
        (cl-incf count))
      (message "Reset %d bookmarks" count))))

(defun notmuch-alert-visit ()
  "Update alerts and let the user select one to visit."
  (interactive)
  (notmuch-alert-update-all)
  (let ((alerts (notmuch-alert-active-alerts)))
    (if (not alerts)
        (user-error "No alert active")
      (let ((alert (completing-read " Visit alerted bookmark: "
                                    alerts nil t)))
        (bookmark-jump alert)))))

(defun notmuch-alert-status-string (bookmark)
  "Return a string about BOOKMARK's alert state."
  (pcase bookmark
    ('nil  "No bookmark")
    ((pred (not notmuch-bookmarks-record-p)) "Not bookmarked as a notmuch bookmark")
    ((pred (not notmuch-alert-p)) "No alert associated")
    ((pred (not notmuch-alert-active-p)) "Alert dormant")
    (_ (format "Bookmark has %d %s"
               (notmuch-alert-get-count bookmark)
               (or (notmuch-alert-get-description bookmark) "mails")))))

(defun notmuch-alert-info-string (bookmark)
  "Return a string on BOOKMARK's alert."
  (pcase bookmark
    ('nil "No bookmark")
    ((pred (not notmuch-bookmarks-record-p)) "Not bookmarked as a notmuch bookmark")
    ((pred (not notmuch-alert-p)) "No alert associated")
    (_  (format "Alert %s: counting %d mails on query '%s'."
                (if (notmuch-alert-active-p bookmark)
                    (propertize "active" 'face 'warning)
                  (propertize "dormant" 'face 'success))
                (notmuch-alert--build-query bookmark)
                (notmuch-alert-get-count bookmark)))))

;;;###autoload
(defun notmuch-alert-display-info (bookmark)
  "Display information on current BOOKMARK's alert."
  (interactive (list (or bookmark-current-bookmark
                         (notmuch-alert-select-bookmark))))
  (message (notmuch-alert-info-string bookmark)))

;;;###autoload
(defun notmuch-alert-display-status (bookmark)
  "Display status information about current BOOKMARK's alert."
  (interactive (list (or bookmark-current-bookmark
                         (notmuch-alert-select-bookmark))))
  (message (notmuch-alert-status-string bookmark)))


;; Minor Mode

(defun notmuch-alert-mode-install (&optional uninstall)
  "Install alerting feature on top of notmuch bookmarks.
If UNINSTALL is non-nil, uninstall the feature.

This function should not be called directly. Use
`notmuch-alert-mode' instead."
  (ignore uninstall))

;;;###autoload
(define-minor-mode notmuch-alert-mode
  "Allow using notmuch bookmarks as alerts."
  :global t
  :group 'notmuch-alert
  (notmuch-alert-mode-install (not notmuch-alert-mode)))

(provide 'notmuch-alert)
;;; notmuch-alert.el ends here
