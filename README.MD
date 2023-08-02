# notmuch alerts

Add and manage alerts 'on top' of notmuch bookmarks. This is package
version 0.2, which breaks v0.1 See below for how to transition.

This package allows to check bookmarked notmuch queries for the
existence of 'new' or 'unread' mails. In the jargon of this package,
such a check is called an *alert*. The package provides an API to
install, visit, and remove alerts.

Alerts are always created 'on top' of existing notmuch bookmarks,
possibly extending it by adding an additional filter query. Thus, if
you have a query listing all mails from today, an *unread alert*
checks whether there are any mails which have today's date *and* are
also marked as unread.

# Features

 - Interactively insall or uninstall an alert in a bookmarked notmuch
   buffer.
 - Set a session-local "mute" value for counting mails which is
   automatically subtracted before deciding whether there are any new
   or unread mails. Thus, you can 'silence' an alert without changing
   the status of the existing mails -- they are just not counted in.
 - Provide an interactive selection of all alerts and bookmarks. You
   can see at one glance which new mails have arrived.

# What is an alert?

An alert tells you whether a bookmark has 'new' mails (if this is the
case, the alert is *active*).

From the point of view of the user, an alert is active if the
bookmarked query returns more than zero mails. Additionally, it is
possible to further refine the bookmarked query by adding an
alert-specific filter, such as "tag:unread". Such a filter does not
change the bookmarked query. With a filter, however, the alert only
responds to the combination ("AND") of the bookmarked query and the
filter search. The alert is *active* if the filter matches any number
of mails; else it remains *inactive* or *dormant*.
 
# Installation

There is currently no "mode" to be activated; the package is just a
set of interactive functions.

I have the following snippet in my `init.el`:

```
;; First, install notmuch-bookmarks:
(use-package notmuch-bookmarks
  :after notmuch
  :config
  (notmuch-bookmarks-mode)
  ;; requires marginalia to work
  (notmuch-bookmarks-annotation-mode))

;; Second, install the alert package:
(use-package notmuch-alert
  :after notmuch-bookmarks
  :bind*
  (:map global-map
	("<f3>" . notmuch-alert-visit)))
```

It is recommended to activate automatic saving of bookmarks. This
way, your bookmarks with alerts become constant entry points for
accessing your mails. The result comes very close to what you are used
to in a normal mail client, yet preserves the "emacs" way of doing
stuff:

```
(use-package bookmark
 :config
 (setq bookmark-save-flag 1))
```

# Interactive Functions

## notmuch-alert-install
Install an alert in the bookmark associated with the current buffer.

Let the user select between a set of predefined alerts and 'hooks' it
into the bookmark. If `bookmark-save-flag`is `t`, automatically save
the changed bookmark.

## notmuch-alert-uninstall

Uninstall current buffer's bookmark alert.

## notmuch-alert-visit

Present the user a selection of all active alerts.

## notmuch-alert-set-mute-count

Set the mute count for the current bookmark. A mute count will be
automatically substracted from the alert count, thus silencing an
alert even if the query returns a non-zero result.

Mute counts do not persist across sessions. 

## notmuch-alert-remove-mute-count

Remove the mute count for the current bookmark. 

## notmuch-alert-remove-all 

Remove all alerts.

## notmuch-alert-mute-counts

Remove all mute counts from all alerts.


## Changelog

 - v0.2 Complete rewrite
 
