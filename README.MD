# notmuch alerts
Add and manage alerts 'on top' of notmuch bookmarks.

This package allows check bookmarked notmuch queries for either 'new'
or 'unread' mails. It further provides an API to visit, create, check
and remove alerts. Alerts are always created 'on top' of existing
notmuch bookmarks, that is, they extend a bookmared query by adding an
additional filter query.

This file is NOT part of notmuch or the notmuch emacs suite.

# What is an alert?

An alert is a function receiving one argument, a bookmark. It is up to
this function to build and run the query which checks for matching
mails. The package provides some helpful functions for that. The
return value of the alert function must be either `nil`, indicating
that nothing happened ("inactive alert"), or the number of relevant
mails matched ("active alert"), as an integer.

# Features

 - Interactively add or remove alert in a bookmarked notmuch buffer.
 - Set a "tare" value for counting mails which is automatically
   subtracted before deciding whether there are any new or unread
   mails. Thus, you can 'silence' an alert without changing the status
   of the existing mails -- they are just not counted in.
 - Provide an interactive selection of all alerts and bookmarks,
   sorted by relevancy.

# Dependencies

This package depends on the following packages:

 - Emacs > 26.1
 - notmuch
 - notmuch-bookmarks

 
# Installation

I have the following snippet in my `init.el`:

```
(use-package notmuch-bookmarks
  :after notmuch
  :config
  (notmuch-bookmarks-mode))

(use-package notmuch-alert
  :config
  (notmuch-alert-mode)
  :bind*
  (:map global-map
	("<f3>" . notmuch-alert-visit)))
```

It is recommended to activate the automatic saving of bookmarks. This
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

The available alerts are predefined in the global variable
`notmuch-alerts`. See the documentation there for how to add new
alerts.

Currently, there are three alerts predefined:

<dl>
 <dt>notmuch-alert-unread</dt>
 <dd>Check for any unread mails.</dd>
 
 <dt> notmuch-alert-any</dt>
 <dd>Check for any mails matching the original bookmark query; no further filtering.<dd>

<dt>notmuch-alert-today:</dt>
<dd>Check for any mails from today.</dd>

 </dl>
## notmuch-alert-uninstall

Uninstall current buffer's bookmark alert.

## notmuch-alert-visit

Present the user a selection consisting of, in this order:

 - a list of all active alerts,
 - a list of all inactive alerts, and ultimately
 - a list of all other notmuch bookmarks, which have no alert
   associated with it.
 
![Screenshot](screenshot.png)

As a special bonus, calling `notmuch-alert-visit` modifies the keymap
so that pressing the calling key twice will cancel the action. This is
meant for offering a quick way to 'check mails': Since all active
alerts appear on the top of the list, pressing the key once gives you
an immediate feedback about newly arrived mails. By pressing the key
again, you return to work.

If you do not like that behaviour, set
`notmuch-alert-visit-quit-when-pressed-twice` to `nil`.

## notmuch-alert-tare

Set the tare for the current buffer. With prefix, just display the
current value. With double prefix, remove the tare.

 Setting a tare of 0 effectively means to remove the tare.
 
 


