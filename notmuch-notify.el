;;; notmuch-notify.el --- Notification for notmuch new emails arrival -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.1
;; Keywords: convenience, notification
;; URL: https://www.github.com/firmart/notmuch-notify
;; Package-Requires: ((emacs "25.1") notmuch)


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


;;; Commentary:
;; 

;;; Code:
(defgroup notmuch-notify nil
  "Notmuch notification"
  :group 'notmuch
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-notification-urgency "normal"
  "The urgency of new email notification.  Accepted value: low, normal, critical."
  :type '(radio (const :tag "low" :value "low")
		(const :tag "normal" :value "normal")
		(const :tag "critical" :value "critical"))
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-excluded-tags nil
  "List of tags that will not trigger system-wise notification.

Useful to not be disturbed by active mailing list."
  :type '(repeat string)
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-refresh-interval 60
  "Send a system-wise notification every given seconds."
  :type 'number
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-title "Notmuch: new message"
  "Title for system-wise notification."
  :type 'string
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-icon (expand-file-name "notmuch-logo.png" ".")
  "Path of the icon associated to system-wise notification.

The path must be absolute."
  :type 'string
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

(defun notmuch-notify-send-notification ()
  "Notify notmuch new mails arrival with the system notification feature."
  (let* ((new-count (notmuch-notify--count))
	 (diff-count (- new-count notmuch-notify-refresh-count))
	 (info (format "%s new messages since last refresh"
		       (notmuch-hello-nice-number diff-count)))
	 (program "notify-send")
         (args (list "-u" "normal" "-i" notmuch-notify-icon notmuch-notify-title info)))
    (cond ((= new-count 0) ;; init counting
	   (notmuch-notify--update new-count))
	  ((and (> diff-count 0)
		(executable-find program))
	   (if notmuch-notify-excluded-tags
	       (when (> (notmuch-notify--count notmuch-notify-refresh-timestamp notmuch-notify-excluded-tags) 0)
		 (apply 'start-process (append (list notmuch-notify-title nil program) args)))
	     (apply 'start-process (append (list notmuch-notify-title nil program) args)))
	   (notmuch-notify--update new-count)))))

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
		       #'notmuch-notify-send-notification))
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
