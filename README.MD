# notmuch alerts

## Summary

Add and manage alerts 'on top' of notmuch bookmarks. 

**IMPORTANT: Current version 0.2, breaks older v0.1!** See
below for how to transition.

This package allows to check bookmarked notmuch queries for the
existence of 'new' or 'unread' mails. In the jargon of this package,
such a check is called an *alert*. The package provides an API to
install, visit, and remove alerts.

Alerts are always created 'on top' of existing notmuch bookmarks,
possibly extending it by adding an additional filter query. Thus, if
you have a query listing all mails from today, an *unread alert*
checks whether there are any mails which have today's date *and* are
also marked as unread.

## Example usage

### Use case: Create an alert for unread mails.

Imagine you want to know if you have new mails from github. The query
would be something like `from:github`. You can create an alert in this
way:

 - Create thequery which shows the relevant mails with `M-x
   notmuch-search`.
 - Bookmark the buffer displaying the mails using `M-x bookmark-set`
   (usually bound to `C-x r m`)
 - Store an alert using `M-x notmuch-alert-install`. 
   - When prompted for an additional filter, enter `is: unread`. 
   - When asked for a description, enter `unread mails`.
   
Now you have created an alert for the associated query. Assuming that
the query is something like `from:github`, you can now see if you
have new mails from github by interactively calling `M-x
notmuch-alert-visit`. You will be prompted a list of bookmarks where
the alerts are 'active', that is in our case: where there are any
unread mails. If there are any active bookmarks, you can visit them by
selecting them.

If you bind `notmuch-alert-visit` to a key, you can thus check for new
mails by simplying using that key.

Alerts, being stored in the bookmark, persist across sessions if you
set `bookmark-save-flag` to `1`.

### Use case: Mute an active alert

Say you use your github inbox as a todo list. Thus, it is not useful
to always see an active alert for unread mails. You better want to
know that a new mail has arrived.

You can fine-tune your alert by calling `M-x
notmuch-alert-set-mute-count`. Visit the buffer containing the
bookmark with the alert, call this command, and then set the mute
count; per default, that is the current number of mails counted. 

With the mute count set, the alert be activated only if your mail
count diverges from the mute count. 

For most cases, this amounts to "you have new mails." But actually,
the alert can be triggered in quite different ways:

 - You have new mails (expected use case).
 - You have no new mails, but changed some stored mails by reading
   them.
 - You have new mails *and* you have changed some mails by reading
   them, or vice versa.
 
 Note that the last condition implies that an alert can be inactive
 even if new mails have arrived -- namely, in the case that you have
 read the same amount of locally stored mails before. Read 3, 3 new
 arrive, nothing changes for that system. Mute count is thus a quite
 primitive measure to define "new", but hey, it works.
 
 Mute counts are not persistent across sessions. To remove mute
 counts, you can use `M-x notmuch-alert-remove-mute-count` or even
 `M-x notmuch-alert-remove-all-mute-counts`.

# Features

 - Interactively insall or uninstall an alert in a bookmarked notmuch
   buffer.
 - Set a session-local "mute" value, effectively 'silencing' an alert
   until new mails arrive.
 - Interactive command showing all active alerts, effectively telling
   you whether new mails have arrived.
 
# What is an alert?

An alert tells you whether a bookmark has 'new' mails (if this is the
case, the alert is *active*). More formally, the alert is *active* if
the query (or the filtered query) matches any number of mails > 0;
else it remains *inactive* or *dormant*.

The bookmarked query can be refined by adding an alert-specific
filter, such as `tag:unread`. When checking the query, the filter is
combined with the bookmark query using `AND`. The alert never changes
the bookmarked query itself.
 
# Installation

There is currently no minor mode to be activated; the package is just
a set of interactive functions.

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

It is recommended to activate automatic saving of bookmarks. This way,
your bookmarks with alerts become constant entry points for accessing
your mails. The result comes very close to what you are used to in a
normal mail client, yet preserves the "emacs" way of doing stuff:

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

## notmuch-alert-mute-all

Mute all alerts at once.

## notmuch-alert-remove-mute-count

Remove the mute count for the current bookmark. 

## notmuch-alert-remove-all 

Remove all alerts.

## notmuch-alert-remove-all-mute-counts  or notmuch-alert-unmute-all

Remove all mute counts from all alerts.

# Transition to v.2

The new version uses a different format to store the alerts. So if you
used `notmuch-alerts` before, you will have to remove the old data and
re-install the alerts by hand. To remove the old data, use `M-x
notmuch-alert-reset-notmuch-bookmarks`. It strips down all notmuch
bookmarks to the core functionality.

# Changelog

 - Add new command `notmuch-alert-mute-all`
 - v0.2 Complete rewrite
 
