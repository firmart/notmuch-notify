;;; notmuch-notify.el --- Notification for notmuch new emails arrival -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.1
;; Keywords: convenience, notification
;; URL: https://www.github.com/firmart/notmuch-notify
;; Package-Requires: ((emacs "25.1") (alert "1.2") notmuch)


;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'notmuch)
(require 'alert)

;;; Commentary:
;;

;;; Code:
(defgroup notmuch-notify nil
  "Notmuch notification"
  :group 'notmuch
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-alert-severity 'normal
  "The severity level of the alert emitted upon new email notification.

Accepted value: trivial, low, normal, moderate, high, urgent."
  :type 'symbol
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-alert-title "Notmuch: new message"
  "Title of the alert emitted upon new email notification."
  :type 'string
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-alert-icon
  (expand-file-name "notmuch-logo.png"
		    (file-name-directory (find-library-name "notmuch-notify")))
  "Path of the icon associated to the alert emitted upon new email notification.

The path must be absolute."
  :type 'file
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-alert-audio-file
  (expand-file-name "emailreceived.wav"
		    (file-name-directory (find-library-name "notmuch-notify")))
  "Path of the audio associated to the alert emitted upon new email notification.

The path must be absolute."
  :type 'file
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-alert-audio-program "mpv"
  "Program to play `notmuch-notify-alert-audio-file'.

E.g. mpv, cvlc, etc."
  :type 'string
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-excluded-tags nil
  "List of tags that doesn't worth to trigger a notification.

Useful to not be disturbed by active mailing list."
  :type '(repeat string)
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-refresh-interval 600
  "The interval in seconds to check if there are new emails.

Warning: the value should not be lower than the interval between
two runs of the command \"notmuch new\". Otherwise, alert can miss
some new emails."
  :type 'number
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defvar notmuch-notify-timer nil)
(defvar notmuch-notify-refresh-count 0)
(defvar notmuch-notify-refresh-timestamp nil)

(defun notmuch-notify-hello-refresh-status-message ()
  "Show the number of new mails after refresh."
  (let* ((new-count (notmuch-notify--count))
	 (diff-count (- new-count notmuch-notify-refresh-count)))
    (cond
     ((= notmuch-notify-refresh-count 0)
      (message "You have %s messages."
	       (notmuch-hello-nice-number new-count)))
     ((> diff-count 0)
      (message "You have %s more messages since last refresh."
	       (notmuch-hello-nice-number diff-count)))
     ((< diff-count 0)
      (message "You have %s fewer messages since last refresh."
	       (notmuch-hello-nice-number (- diff-count)))))
    (notmuch-notify--update new-count)))

(defun notmuch-notify--count (&optional since-timestamp excluded-tags)
  "Count emails SINCE-TIMESTAMP with EXCLUDED-TAGS.

- Called without argument, runs \"notmuch count\"
- Called with SINCE-TIMESTAMP, runs \"notmuch count
  date:@SINCE-TIMESTAMP..@ts-now\"
- Called with both arguments, runs \"notmuch count
  date:@SINCE-TIMESTAMP..@ts-now and not (tag:tag1 or ... or tag:tagN)\""
  (let* ((date (when since-timestamp
		 (format "date:@%s..@%s"
			 since-timestamp
			 (format-time-string "%s" (current-time)))))
	 (tags (when excluded-tags
		 (concat "not ("
			 (string-join
			  (mapcar
			   (lambda (s) (concat "tag:" s)) excluded-tags)
			  " or ")
			 ")")))
	 (args (list "count"
		     (string-join (remove nil (list date tags)) " and "))))
    (string-to-number
     (car (apply #'process-lines notmuch-command args)))))

(defun notmuch-notify--update (new-count)
  "Update refresh counting by NEW-COUNT and timestamp."
  (setq notmuch-notify-refresh-count new-count)
  (setq notmuch-notify-refresh-timestamp (format-time-string "%s" (current-time))))

(defun notmuch-notify-send-alert ()
  "Notify notmuch new mails arrival with the system notification feature."
  (let* ((new-count (notmuch-notify--count))
	 (diff-count (- new-count notmuch-notify-refresh-count))
	 (info (format "%s new messages since last refresh"
		       (notmuch-hello-nice-number diff-count)))
	 (ready (and (not (= new-count 0)) ;; already initialized
		     (> diff-count 0) ;; there are new email
		     ;; whether or not we exclude some tags
		     (or (not notmuch-notify-excluded-tags)
			 (and notmuch-notify-excluded-tags
			      (> (notmuch-notify--count
				  notmuch-notify-refresh-timestamp
				  notmuch-notify-excluded-tags)
				 0))))))
    (when ready
      (alert info
	     :severity notmuch-notify-alert-severity
	     :title notmuch-notify-alert-title
	     :icon notmuch-notify-alert-icon
	     :id 'notmuch-notify)
      (when (and (executable-find notmuch-notify-alert-audio-program)
		 (file-exists-p notmuch-notify-alert-audio-file))
	(start-process "notmuch-notify" nil notmuch-notify-alert-audio-program
		       notmuch-notify-alert-audio-file)))
    (notmuch-notify--update new-count)))

;; FIXME notmuch-notify-timer may be not nil while no timer is running
(defun notmuch-notify-set-refresh-timer ()
  "Set notmuch notification timer."
  (interactive)
  ;; kill the timer whenever the interval is updated.
  (when (and notmuch-notify-timer
	     (not (= notmuch-notify-refresh-interval
		     (aref notmuch-notify-timer 4))))
    (notmuch-notify-cancel-refresh-timer))
  (unless notmuch-notify-timer
    (setq notmuch-notify-timer
	  (run-at-time nil
		       notmuch-notify-refresh-interval
		       #'notmuch-notify-send-alert))
    (message "notmuch-notify set timer %s." notmuch-notify-timer)))

(defun notmuch-notify-cancel-refresh-timer ()
  "Cancel notmuch notification timer."
  (interactive)
  (cond ((not notmuch-notify-timer)
	 (message "notmuch-notify is not running any timer!"))
	(notmuch-notify-timer
	 (message "notmuch-notify canceled timer %s." notmuch-notify-timer)
	 (cancel-timer notmuch-notify-timer)
	 (setq notmuch-notify-timer nil))))

(provide 'notmuch-notify)
;;; notmuch-notify ends here
