;;; notmuch-alert.el --- Use notmuch bookmarks as alerts  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021; rewrite 2023

;; Author:  <joerg@joergvolbers.de>
;; Package-Requires: ((seq "2.20") (emacs "26.1") (notmuch "0.1") (notmuch-bookmarks "0.2"))
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

;; TODO Idee: bookmark-after-jump-hook einbauen. Wenn
;; bookmark-current-bookmark ein notmuch bookmark ist, dann z.B.
;; anzeige machen über "tare". und allgemein Infos zum "alert". Diese
;; Anzeige kann dann auch z.B. bei der Aktualisierung via "g"
;; angezeigt werden. Statt eine Anzeige im Minibuffer wäre auch eine
;; header line denkbar.

;;; Code:

(require 'notmuch-bookmarks)
(require 'cl-lib)

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
