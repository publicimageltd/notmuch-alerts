;;; notmuch-alert-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck_notmuch-alert" "flycheck_notmuch-alert.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flycheck_notmuch-alert.el

(autoload 'notmuch-alert-display-info "flycheck_notmuch-alert" "\
Display information on current BOOKMARK's alert.

\(fn BOOKMARK)" t nil)

(defvar notmuch-alert-mode nil "\
Non-nil if Notmuch-Alert mode is enabled.
See the `notmuch-alert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `notmuch-alert-mode'.")

(custom-autoload 'notmuch-alert-mode "flycheck_notmuch-alert" nil)

(autoload 'notmuch-alert-mode "flycheck_notmuch-alert" "\
Allow using notmuch bookmarks as alerts.

This is a minor mode.  If called interactively, toggle the
`Notmuch-Alert mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='notmuch-alert-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "flycheck_notmuch-alert" '("notmuch-alert-"))

;;;***

;;;### (autoloads nil "notmuch-alert" "notmuch-alert.el" (0 0 0 0))
;;; Generated autoloads from notmuch-alert.el

(autoload 'notmuch-alert-display-info "notmuch-alert" "\
Display information on current BOOKMARK's alert.

\(fn BOOKMARK)" t nil)

(defvar notmuch-alert-mode nil "\
Non-nil if Notmuch-Alert mode is enabled.
See the `notmuch-alert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `notmuch-alert-mode'.")

(custom-autoload 'notmuch-alert-mode "notmuch-alert" nil)

(autoload 'notmuch-alert-mode "notmuch-alert" "\
Allow using notmuch bookmarks as alerts.

This is a minor mode.  If called interactively, toggle the
`Notmuch-Alert mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='notmuch-alert-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "notmuch-alert" '("notmuch-alert-"))

;;;***

;;;### (autoloads nil "notmuch-alert-old" "notmuch-alert-old.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from notmuch-alert-old.el

(autoload 'notmuch-alert-display "notmuch-alert-old" "\
Display information about the current buffer's alert." t nil)

(autoload 'notmuch-alert-visit "notmuch-alert-old" "\
Jump to one of the bookmarks with an alert." t nil)

(autoload 'notmuch-alert-install "notmuch-alert-old" "\
Install an alert for the current buffer.
The available alerts are defined by `notmuch-alerts'." t nil)

(autoload 'notmuch-alert-uninstall "notmuch-alert-old" "\
Uninstall an alert for the current buffer." t nil)

(autoload 'notmuch-alert-check "notmuch-alert-old" "\
Check current buffer's alert." t nil)

(autoload 'notmuch-alert-tare "notmuch-alert-old" "\
Set the tare for the current buffer.
With prefix JUST-DISPLAY set to '(4), do not set the value, just
display it. With prefix JUSt-DISPLAY set to '(16), remove the
tare.

\(fn &optional JUST-DISPLAY)" t nil)

(defvar notmuch-alert-mode nil "\
Non-nil if Notmuch-Alert mode is enabled.
See the `notmuch-alert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `notmuch-alert-mode'.")

(custom-autoload 'notmuch-alert-mode "notmuch-alert-old" nil)

(autoload 'notmuch-alert-mode "notmuch-alert-old" "\
Allow using notmuch bookmarks as alerts.

This is a minor mode.  If called interactively, toggle the
`Notmuch-Alert mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='notmuch-alert-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "notmuch-alert-old" '("notmuch-alert"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; notmuch-alert-autoloads.el ends here
