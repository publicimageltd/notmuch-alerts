# notmuch alerts
Add and manage alerts on top of notmuch bookmarks

This package defines functionality to check bookmarked queries for
either 'new' or 'unread' mails. It further provides an API to create,
check and remove alerts, which hook into an existing bookmark.

This file is NOT part of notmuch or the notmuch emacs suite.

# What is an alert?

An alert simply is a function receiving one argument, a bookmark. It
is the function's task to build a query (using the bookmark) and to
check for matching mails, etc. The return value of the alert function
must be either nil, indicating that nothing happened ("inactive
alert"), or the number of relevant mails matched ("active alert").

The package provides some functions which help to construct
alerts, e.g. to count mails.

# Features

 - Interactively add or remove alert in a bookmarked notmuch buffer
 - Set a "tare" value for counting mails which is automatically
   subtracted before deciding whether there are any new or unread
   mails.
 - Provide an interactive selection of all alerts and bookmarks,
   sorted by relevancy.

# Requires

 - notmuch (of course)
 - notmuch-bookmarks.el
 - acomplete (a package of my own, also available in this repository)
 
# Installation

I have the following snippet in my `init.el`:

```
(use-package notmuch-bookmarks
  :after notmuch
  :config
  (notmuch-bookmarks))

(use-package notmuch-alert
  :bind*
  (:map global-map
	("<f3>" . notmuch-alert-visit)))
```

It is recommended to activate automatic saving of bookmarks. This way,
your bookmarks with alerts become convenient constant entry points for
accessing your mails, comparable to what you see in a normal mail
client:

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

The available alerts are predefined in the global variable
`notmuch-alerts'. See there for how to add new alerts.

Currently, there are three alerts defined:

 - notmuch-alert-unread: Check for any unread mails.
 - notmuch-alert-any: Check for any matching mails at all.
 - notmuch-alert-today: Check for any mails from today.
 
## notmuch-alert-uninstall

Uninstall current buffer's bookmark alert.

## notmuch-alert-visit

Present the user a selection consisting of, in this order:

 - all active alerts
 - all inactive alerts
 - all further notmuch bookmarks
 
!()[screenshot.png]

## notmuch-alert-tare

Set the tare for the current buffer. With prefix, just display the
current value. With double prefix, remove the tare.


