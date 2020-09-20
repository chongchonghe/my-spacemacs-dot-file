(setq-default dotspacemacs-configuration-layers
              '((mu4e :variables
                      mu4e-installation-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp")))

;;(defun dotspacemacs/user-config ()
;; Ref: https://www.emacswiki.org/emacs/LoadingLispFiles
(require 'mu4e)

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; for mbsync, when move a message to All Mail as archiving
(setq mu4e-change-filenames-when-moving t)

;; Path to emails
(setq mu4e-maildir "~/mbsync2")

(setq mu4e-get-mail-command "mbsync -a")

(setq mu4e-attachment-dir "~/Documents/MailAttachments")

(setq
 starttls-use-gnutls t
 smtpmail-stream-type 'starttls
 )

;; send messages
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)

;; TODO: try this out
;; (setq mu4e-context-policy 'pick-first)
;; (setq mu4e-compose-context-policy 'always-ask)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Outlook and gmail contexts
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "umdastro"
          :enter-func (lambda () (mu4e-message "Entering the umdastro context"))
          :leave-func (lambda () (mu4e-message "Leaving umdastro context"))
          ;; we match based on the maildir of the message
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/umdastro" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address . "chongchong@astro.umd.edu" )
                   ( user-full-name . "ChongChong He" )
                   ( smtpmail-smtp-user . "chongchong" )
                   ( smtpmail-default-smtp-server . "gaia.astro.umd.edu" )
                   ( smtpmail-smtp-server . "gaia.astro.umd.edu" )
                   ( mu4e-trash-folder . "/umdastro/Trash" )
                   ( mu4e-refile-folder . "/umdastro/Archive" )
                   ( mu4e-drafts-folder . "/umdastro/Drafts" )
                   ( mu4e-sent-folder . "/umdastro/Sent Messages" )
                   ;; Save message to Sent Messages, astro IMAP server fails to take care of this
                   ( mu4e-sent-messages-behavior . sent)
                   ( mu4e-maildir-shortcuts . (("/umdastro/INBOX"         . ?i)
                                               ("/umdastro/Sent Messages" . ?s)
                                               ("/umdastro/Trash"         . ?t)
                                               ("/umdastro/Archive"       . ?a)
                                               ("/umdastro/Drafts"        . ?d)
                                               ;;          ("/Reference"           . ?r)
                                               ;;          ;; ("/Waiting"             . ?w)
                                               ;;          ;; ("/Action Items"        . ?c)
                                               ))
                   ))
        ,(make-mu4e-context
           :name "terpmail"
           :enter-func (lambda () (mu4e-message "Entering terpmail context"))
           :leave-func (lambda () (mu4e-message "Leaving terpmail context"))
           ;; we match based on the maildir of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches
                            msg '(:from :to :cc :bcc) "che1234@terpmail.umd.edu")
                           (mu4e-message-contact-field-matches
                            msg '(:from :to :cc :bcc) "che1234@umd.edu")
                           ;; (string-match-p "^/terpmail" (mu4e-message-field msg :maildir))
                           ))
           :vars '( ( user-mail-address . "che1234@terpmail.umd.edu" )
                    ( smtpmail-smtp-user . "che1234@terpmail.umd.edu")
                    ( smtpmail-smtp-server . "smtp.gmail.com" )
                    (smtpmail-smtp-service . 587)
                    ( smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                    ( smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                    ( user-full-name . "ChongChong He" )
                    ;; testing: removing all the folder def since I don't need
                    ;; it. Later will set folder shortcuts
                    ;; ???? TODO, where is trash?
                    ( mu4e-trash-folder . "/terpmail/trash" )
                    ( mu4e-sent-folder . "/terpmail/[Gmail].All Mail" )
                    ( mu4e-refile-folder . "/terpmail/[Gmail].All Mail" )
                    ( mu4e-drafts-folder . "/terpmail/[Gmail].Drafts" )
                    ;; don't save message to Sent Messages, IMAP takes care of this
                    ( mu4e-sent-messages-behavior . delete)
                    ( mu4e-maildir-shortcuts . (("/terpmail/INBOX"               . ?i)
                                                ("/terpmail/[Gmail].Sent Mail"   . ?s)
                                                ;; only trash is named differently
                                                ("/terpmail/trash"               . ?t)
                                                ("/terpmail/[Gmail].Starred"     . ?r)
                                                ("/terpmail/[Gmail].All Mail"    . ?a)))
                    ))
         ))


;; ;; TODO: try this out. 
;; ;;; Bookmarks
;; (setq mu4e-bookmarks
;;       `(
;;         ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
;;         ("flag:unread" "new messages" ?n)
;;         ("date:today..now" "Today's messages" ?t)
;;         ("date:7d..now" "Last 7 days" ?w)
;;         ("mime:image/*" "Messages with images" ?p)
;;         ))

(setq mu4e-update-interval 60)
(setq mu4e-search-result-limit 500)
(setq mu4e-headers-results-limit 200)
;; (define-key mu4e-headers-mode-map (kbd "d") nil)

; tell mu4e to use w3m for html rendering.
;; (setq mu4e-html2text-command "w3m -T text/html")
(setq mu4e-html2text-command "w3m -dump -T text/html -o display_link_number=true")

;; Alert
;; (setq mu4e-alert-interesting-mail-query "flag:unread AND maildir:/INBOX")

;; view message in browser by typing 'aV'
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq mu4e-hide-index-messages t) ;; Silence index messages

;; show images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; TODO: try this out
;; Load the org-mu4e package 
;; (load "org-mu4e") 
;; (setq org-mu4e-convert-to-html t)
;; (use-package org-mime
;;   :ensure t)

;; https://emacs.stackexchange.com/questions/52487/mu4e-how-to-stop-the-unarchiving-of-entire-threads-when-new-message-arrives
;; TODO: uncomment if necessary
;; (setq mu4e-headers-include-related nil)

;; ;; sending emails
;; (setq user-mail-address "chongchong@astro.umd.edu"
;;       user-full-name  "ChongChong He"
;;       smtpmail-default-smtp-server "gaia.astro.umd.edu"
;;       ;; smtpmail-local-domain "account1.example.com"
;;       smtpmail-smtp-server "gaia.astro.umd.edu"
;;       ;; smtpmail-stream-type 'starttls
;;       ;; smtpmail-smtp-service 587
;;       smtpmail-debug-info t)
;; from: http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/#getting-set-up-with-mu-and-offlineimap
;; (setq sendmail-program "/usr/local/bin/msmtp"
;;       send-mail-function 'smtpmail-send-it
;;       message-sendmail-f-is-evil t
;;       message-sendmail-extra-arguments '("--read-envelope-from")
;;       message-send-mail-function 'message-send-mail-with-sendmail)

;; from: https://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
;; 1) messages to me@foo.example.com should be replied with From:me@foo.example.com
;; 2) messages to me@bar.example.com should be replied with From:me@bar.example.com
;; 3) all other mail should use From:me@cuux.example.com
(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (when msg
  (setq user-mail-address
    (cond
      ((mu4e-message-contact-field-matches msg :to "chongchong@astro.umd.edu")
        "chongchong@astro.umd.edu")
      (t "chongchong@astro.umd.edu")))))))

;; (setq mu4e-compose-signature
;;    "ChongChong He\n")
;; from: https://emacs.stackexchange.com/questions/21723/how-can-i-delete-mu4e-drafts-on-successfully-sending-the-mail
(defun draft-auto-save-buffer-name-handler (operation &rest args)
  "for `make-auto-save-file-name' set '.' in front of the file name; do nothing for other operations"
(if
    (and buffer-file-name (eq operation 'make-auto-save-file-name))
    (concat (file-name-directory buffer-file-name)
            "."
            (file-name-nondirectory buffer-file-name))
  (let ((inhibit-file-name-handlers
         (cons 'draft-auto-save-buffer-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args))))

;; automatically cc myself
(setq mu4e-compose-keep-self-cc t)
(setq mu4e-compose-dont-reply-to-self nil) ;; trying because the above doesn't work

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; TODO: Uncomment if necessary
;; ;; (setq mu4e-compose-dont-reply-to-self t) ; ?
;; ;; from: https://emacs.stackexchange.com/questions/52608/how-to-add-a-value-for-cc-or-reply-to-in-each-new-message/52609
;; (add-hook 'mu4e-compose-mode-hook
;;           (defun my-add-bcc ()
;;             "Add a cc: header."
;;             (save-excursion (message-add-header "cc: chongchong@astro.umd.edu\n"))))


;; (add-to-list 'file-name-handler-alist '("Drafts/cur/" . draft-auto-save-buffer-name-handler))

;; TODO: Uncomment if necessary. Don't know how the default looks in spacemacs
;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:human-date    . 12)    ;; alternatively, use :human-date
       (:flags         . 6)
       (:from-or-to    . 25)
       ;; (:cc            . 25)
       (:subject       . nil))) ;; alternatively, use :thread-subject

(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")
;; Desktop notifications for unread emails
;; Choose the style you prefer for desktop notifications
;; If you are on Linux you can use
;; 1. notifications - Emacs lisp implementation of the Desktop Notifications API
;; 2. libnotify     - Notifications using the `notify-send' program, requires `notify-send' to be in PATH
;;
;; On Mac OSX you can set style to
;; 1. notifier      - Notifications using the `terminal-notifier' program, requires `terminal-notifier' to be in PATH
;; 1. growl         - Notifications using the `growl' program, requires `growlnotify' to be in PATH
;;(mu4e-alert-set-default-style 'notifier) ; TODO, fix this
;;(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
;; Mode Line display of unread emails
;; Display of the unread email count in the mode-line
;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
;;(add-hook 'after-init-hook #'mu4e-alert-disable-mode-line-display)
;; adding the following snippet to your init file, will instruct
;; mu4e-alert to only display the number of unread emails.
;; (setq mu4e-alert-email-notification-types '(subjects))
;; (setq mu4e-alert-email-notification-types '(count))

;; Configs from https://www.reddit.com/r/spacemacs/comments/c8omik/spacemacs_mu4e_emacsw3m_awesome/ that I don't understand
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-display-update-status-in-modeline t)
(setq mu4e-compose-format-flowed t)
(setq mu4e-compose-complete-only-personal t)
;;
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
;;
(electric-pair-mode)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)
             ))

;; (setq mu4e-compose-mode-hook
;;           (lambda ()
;;             (use-hard-newlines -1)
;;             (turn-off-auto-fill)
;;             (visual-line-mode)))
;; (require 'smtpmail)
;; (setq message-send-mail-function 'smtpmail-send-it
;;       starttls-use-gnutls t
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "XXX@XXX.COM" "XXX"))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)
;;)
