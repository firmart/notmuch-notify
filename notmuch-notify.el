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

;;; Code:
(defgroup notmuch-notify nil
  "Notmuch notification"
  :group 'notmuch
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-notification-urgency "normal"
  "The urgency of new email notification. Accepted value: low, normal, critical"
  :type '(radio (const :tag "low" :value "low")
		(const :tag "normal" :value "normal")
		(const :tag "critical" :value "critical"))
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-refresh-interval 600
  "Send a notification every given seconds."
  :type 'number
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-title "Notmuch: new message"
  "Notification title for new email arrival."
  :type 'string
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defvar notmuch-notify-timer nil)
(defvar notmuch-notify-refresh-count 0)

(defun notmuch-notify-hello-refresh-status-message ()
  "Show the number of new mails after refresh."
  (let* ((new-count (string-to-number
		     (car (process-lines notmuch-command "count"))))
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
    (setq notmuch-notify-refresh-count new-count)))

(defun notmuch-notify-send-notification ()
  "Notify notmuch new mails arrival with the system notification feature."
  (let* ((new-count (string-to-number
		     (car (process-lines notmuch-command "count"))))
	 (diff-count (- new-count notmuch-notify-refresh-count))
	 (title notmuch-notify-title)
	 (info (format "%s more messages since last refresh" (notmuch-hello-nice-number diff-count)))
	 (program "notify-send")
         (args (list "-u" "normal" title info)))
    (cond ((= new-count 0) (setq notmuch-notify-refresh-count new-count))
	  ((and (> diff-count 0) 
		(executable-find program))
	   (apply 'start-process (append (list title nil program) args))
	   (setq notmuch-notify-refresh-count new-count)))))

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

(notmuch-notify-set-refresh-timer)
(add-hook 'notmuch-hello-refresh-hook #'notmuch-notify-hello-refresh-status-message)

(provide 'notmuch-notify)
;;; notmuch-notify ends here
