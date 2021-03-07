;;; notmuch-notify.el --- Notification for notmuch new emails arrival -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.2
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
(require 'seq)
(require 'cl-loop)

;;; Commentary:

;;; Code:

(defgroup notmuch-notify nil
  "Notmuch notification"
  :group 'notmuch
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-alert-default-severity 'normal
  "The severity level of the `alert' emitted upon new email arrival.

Accepted value: trivial, low, normal, moderate, high, urgent.
See `alert' for more information."
  :type 'symbol
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.2"))

(defcustom notmuch-notify-alert-default-title "Notmuch: new message"
  "Title of the `alert' emitted upon new email arrival."
  :type 'string
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.2"))

(defcustom notmuch-notify-alert-default-icon
  (expand-file-name "notmuch-logo.png"
		    (file-name-directory (find-library-name "notmuch-notify")))
  "Path of the icon associated to the alert emitted upon new email arrival.

The path must be absolute."
  :type 'file
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.2"))

(defcustom notmuch-notify-alert-default-audio
  (expand-file-name "emailreceived.wav"
		    (file-name-directory (find-library-name "notmuch-notify")))
  "Path of the audio associated to the alert emitted upon new email arrival.

The path must be absolute."
  :type 'file
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.2"))

(define-widget 'notmuch-notify-alert-profile 'list
  "A single alert profile."
  :args
  '(list (group (const :name) symbol)
	 (group (const :severity) symbol)
	 (group (const :search-term) (repeat string))
	 (group (const :title) string)
	 (group (const :icon) (file :must-match t))
	 (group (const :audio) (file :must-match t))))

(defcustom notmuch-notify-alert-profiles
  `(,(list :name "Default"
	   :search-term nil
	   :severity notmuch-notify-alert-default-severity
	   :title notmuch-notify-alert-default-title
	   :icon notmuch-notify-alert-default-icon
	   :audio notmuch-notify-alert-default-audio))
  "A list of alert profiles.

An alert profile is a property list made with the optional
properties below.  It allows to set different alert for different
messages.  If any property is missed, `notmuch-notify' will
fallback to the default value `notmuch-notify-alert-default-*'.

`:search-term'

  A notmuch search-term to select messages belonging this alert
  profile.  Message having tag in `notmuch-notify-excluded-tags'
  will be excluded unconditionally.  `notmuch-notify' will take
  care to not count messages belonging to two different profiles.
  Especially, if one profile set it to nil, the profile will
  either take into account all new messages (if it's the only
  profile) or the remaining messages unselected by other
  profiles.  Note that at most one profile could have this
  property nil (other such profiles will be ignored).

`:severity'

  `alert' severity: should be one of `urgent', `high',
  `moderate',`normal',`low' or `trivial'.

`:title': The `alert' title.
`:icon': The `alert' icon.
`:audio': The audio played along with the `alert'.
`:name': The name of the alert profile (for debug purpose)."

  :type '(repeat notmuch-notify-alert-profile)
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.2"))

(defcustom notmuch-notify-alert-audio-program "mpv"
  "Program to play alert audio.

E.g. mpv, cvlc, etc."
  :type 'string
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-excluded-tags nil
  "List of tags that doesn't worth to trigger a notification.

Useful to prevent being disturbed by active mailing list."
  :type '(repeat string)
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defcustom notmuch-notify-refresh-interval 60
  "The interval in seconds to check if there are new emails."
  :type 'number
  :group 'notmuch-notify
  :package-version '(notmuch-notify . "0.1"))

(defvar notmuch-notify--timer nil "Notmuch notify alert timer.")
;; TODO add last visit to notify new mail when emacs is not open
;; (defvar notmuch-notify-refresh-timestamp nil)
(defvar notmuch-notify--hash-table nil
  "Notmuch notify hash table.
- Key is notmuch search term.
- Value is the last total messages returned by 'notmuch count <search-term>'.")

(defun notmuch-notify-hello-refresh-status-message ()
  "Show the number of new mails after refreshing `notmuch-hello' buffer."
  (let* ((old-count (or (gethash nil notmuch-notify--hash-table) 0))
	 (new-count (notmuch-notify--count))
	 (diff-count (- new-count old-count)))
    (cond
     ((= old-count 0)
      (message "You have %s messages." (notmuch-hello-nice-number new-count)))
     ((> diff-count 0) (message "You have %s more messages since last refresh."
				(notmuch-hello-nice-number diff-count)))
     ((< diff-count 0) (message "You have %s fewer messages since last refresh."
				(notmuch-hello-nice-number (- diff-count)))))
    (notmuch-notify--update nil new-count)))

(defun notmuch-notify--build-search-term (&optional main-st excluded-tags excluded-sts ts-since &rest extra-sts)
  "Build a notmuch search term composed by:
- MAIN-ST: the main notmuch search term.
- EXCLUDED-TAGS: a list of excluded tags.
- EXCLUDED-STS: a list of excluded search terms.
- TS-SINCE: a timestamp to make the search term date:@TS-SINCE..<ts-current>.
- EXTRA-STS: extra search terms."
  (let ((main-st (if (string-empty-p main-st) nil main-st))
	(excl-tags-str (when excluded-tags
			 (concat "not ("
				 (string-join
				  (mapcar
				   (lambda (s) (concat "tag:" s)) excluded-tags)
				  " or ")
				 ")")))
	(excl-st-str  (when excluded-sts
			(concat "not ("
				(string-join excluded-sts " or ")
				")")))
	(date (when ts-since
		(format "date:@%s..@%s"
			ts-since
			(format-time-string "%s" (current-time))))))

    (string-join (remove nil (append (list date main-st excl-tags-str excl-st-str) extra-sts)) " and ")))

(defun notmuch-notify--query-headers (search-term)
  "Return the header of emails matching the query SEARCH-TERM."
  (let ((default-directory (temporary-file-directory)))
    (notmuch-query-map-threads
     (lambda (p) (plist-get p :headers))
     (notmuch-query-get-threads (list search-term)))))

(defun notmuch-notify--count (&optional search-term)
  "Count emails matching the query SEARCH-TERM."
  (let ((default-directory (temporary-file-directory)))
    (string-to-number
     (car (apply #'process-lines notmuch-command (remove nil (list "count" search-term)))))))

(defun notmuch-notify--init-hash-table ()
  "Initialize `notmuch-notify--hash-table' if it's not the case."
  (unless notmuch-notify--hash-table
    (setq notmuch-notify--hash-table
	  (make-hash-table :size (length notmuch-notify-alert-profiles)
			   :test 'equal))))

(defun notmuch-notify--update (key new-count)
  "Update `notmuch-notify--hash-table' KEY value by NEW-COUNT."
  (notmuch-notify--init-hash-table)
  (puthash key new-count notmuch-notify--hash-table))

(defun notmuch-notify--build-sender-name (from)
  "Build the sender name from the FROM property of notmuch header."
  (replace-regexp-in-string "\\(.*?\\)\s *<.*>.*" "\\1" from))

(defun notmuch-notify--convert-header-date-to-ts (header)
  "Convert notmuch HEADER date (RFC 822) into Unix timestamp."
  (time-convert (encode-time (parse-time-string (plist-get header :Date))) 'integer))

(defun notmuch-notify--build-message (new-email-count search-term)
  "Build an alert message based on NEW-EMAIL-COUNT and a notmuch SEARCH-TERM."
  (let* ((headers (seq-take (cl-sort
			     (notmuch-notify--query-headers search-term) #'>
			     :key #'notmuch-notify--convert-header-date-to-ts)
			    new-email-count))
	 (subjects (mapcar (lambda (header)
			     (format "(%s) %s" (notmuch-notify--build-sender-name (plist-get header :From))
				     (plist-get header :Subject)))
			   headers)))
    (concat
     (format "%s new messages since last refresh\n\n" new-email-count)
     (string-join subjects "\n"))))

(defun notmuch-notify-send-alert-profile (profile &optional excluded-search-terms)
  "Send notification for the notmuch alert PROFILE.

Also exclude accumulated search terms EXCLUDED-SEARCH-TERMS
coming from other profiles so that all alert profiles' messages
are mutually exclusive, i.e. you won't received two notifications
for an email matching two different notmuch search terms."
  (let* ((default-directory (temporary-file-directory))
	 (search-term (notmuch-notify--build-search-term
		       (plist-get profile :search-term)
		       notmuch-notify-excluded-tags excluded-search-terms))
	 (old-count (or (gethash search-term notmuch-notify--hash-table) 0))
	 (new-count (notmuch-notify--count search-term))
	 (diff-count (- new-count old-count))
	 (msg (notmuch-notify--build-message
	       diff-count
	       (notmuch-notify--build-search-term
		search-term nil nil
		;; Lower bound search term, as notmuch show required,
		;; date:@<ts-current - refresh-interval - 10m>..<ts-current>
		;; to fetch enough messages but not too much.
		(- (string-to-number (format-time-string "%s" (current-time)))
		   (+ notmuch-notify-refresh-interval 600)))))
	 (ready (and (not (= old-count 0)) ;; already initialized
		     (> diff-count 0))))

    (when ready
      (alert msg
	     :severity (or (plist-get profile :severity) notmuch-notify-alert-default-severity)
	     :title (or (plist-get profile :title) notmuch-notify-alert-default-title)
	     :icon (or (plist-get profile :icon) notmuch-notify-alert-default-icon)
	     :id 'notmuch-notify)
      (when (and (executable-find notmuch-notify-alert-audio-program)
		 (file-exists-p (or (plist-get profile :audio) notmuch-notify-alert-default-audio)))
	(start-process "notmuch-notify" nil notmuch-notify-alert-audio-program
		       notmuch-notify-alert-default-audio)))
    (notmuch-notify--update search-term new-count)))

(defun notmuch-notify-send-alert ()
  "Send notification for all profiles in `notmuch-notify-alert-profiles'."
  (notmuch-notify--init-hash-table)
  (let ((nil-st-profiles) (other-profiles) (acc-exclude-sts))

    ;; Separate profile(s) having nil `:search-term'.
    (dolist (profile notmuch-notify-alert-profiles)
      (if (null (plist-get profile :search-term))
	  (push profile nil-st-profiles)
	(push profile other-profiles)))

    ;; If there are multiple nil-search-term profile, keep only one of them
    (when (< 1 (length notmuch-notify-alert-profiles))
      (setq nil-st-profiles (list (car nil-st-profiles))))

    ;; Move nil-search-term profile to the end
    (dolist (profile (append other-profiles nil-st-profiles))
      (notmuch-notify-send-alert-profile profile acc-exclude-sts)
      ;; Accumulate search terms and exclude them to have mutually exclusive messages sets
      (push (plist-get profile :search-term) acc-exclude-sts))))

;; FIXME notmuch-notify--timer may be not nil while no timer is running
(defun notmuch-notify-set-refresh-timer ()
  "Set notmuch notification timer."
  (interactive)
  ;; kill the timer whenever the interval is updated.
  (when (and notmuch-notify--timer
	     (not (= notmuch-notify-refresh-interval
		     (aref notmuch-notify--timer 4))))
    (notmuch-notify-cancel-refresh-timer))
  (unless notmuch-notify--timer
    (setq notmuch-notify--timer
	  (run-at-time nil
		       notmuch-notify-refresh-interval
		       #'notmuch-notify-send-alert))
    (message "notmuch-notify set timer %s." notmuch-notify--timer)))

(defun notmuch-notify-cancel-refresh-timer ()
  "Cancel notmuch notification timer."
  (interactive)
  (cond ((not notmuch-notify--timer)
	 (message "notmuch-notify is not running any timer!"))
	(notmuch-notify--timer
	 (message "notmuch-notify canceled timer %s." notmuch-notify--timer)
	 (cancel-timer notmuch-notify--timer)
	 (setq notmuch-notify--timer nil))))

(provide 'notmuch-notify)
;;; notmuch-notify ends here
