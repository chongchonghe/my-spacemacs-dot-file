;; example configuration for mu4e

;; make sure mu4e is in your load-path
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; Only needed if your maildir is _not_ ~/Maildir
;; Must be a real dir, not a symlink
(setq mu4e-maildir "~/mbsync2/terpmail")

(setq mu4e-get-mail-command "mbsync terpmail")

;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.
;; (setq mu4e-sent-folder   "/sent")
;; (setq mu4e-drafts-folder "/drafts")
;; (setq mu4e-trash-folder  "/trash")
(setq mu4e-trash-folder "/trash" )
(setq mu4e-sent-folder "/[Gmail].All Mail" )
(setq mu4e-refile-folder "/[Gmail].All Mail" )
(setq mu4e-drafts-folder "/[Gmail].Drafts" )

;; smtp mail setting; these are the same that `gnus' uses.
(setq
 message-send-mail-function   'smtpmail-send-it
 user-mail-address "che1234@terpmail.umd.edu"
 user-full-name  "ChongChong He"
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server         "smtp.gmail.com"
 ;; starttls-use-gnutls t
 ;; smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 ;; smtpmail-auth-credentials '(("smtp.gmail.com" 587 "che1234@terpmail.umd.edu" nil))
 smtpmail-stream-type 'starttls
 smtpmail-smtp-service 587
 )
