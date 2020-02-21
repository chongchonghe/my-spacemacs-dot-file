(setq-default dotspacemacs-configuration-layers
              '((mu4e :variables
                      mu4e-installation-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp")))

;;(defun dotspacemacs/user-config ()
;; Ref: https://www.emacswiki.org/emacs/LoadingLispFiles
(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-maildir "~/mbsync2/umdastro")
(setq mu4e-get-mail-command "mbsync -a")
;; mu4e requires to specify drafts, sent, and trash dirs a smarter
;; configuration allows to select directories according to the account
;; (see mu4e page)
(setq mu4e-sent-folder    "/Sent Messages")
(setq mu4e-drafts-folder  "/Drafts")
(setq mu4e-trash-folder   "/Trash")
(setq mu4e-refile-folder  "/Archive")
;; don't save message to Sent Messages, IMAP takes care of this
;; (setq mu4e-sent-messages-behavior 'delete)
;; Save message to Sent Messages, astro IMAP server fails to take care of this
(setq mu4e-sent-messages-behavior 'sent)
(setq mu4e-update-interval 60)
(setq mu4e-search-result-limit 500)
(setq mu4e-headers-results-limit 200)
(setq mu4e-change-filenames-when-moving t)
;; (define-key mu4e-headers-mode-map (kbd "d") nil)
; tell mu4e to use w3m for html rendering.
;; (setq mu4e-html2text-command "w3m -T text/html")
(setq mu4e-html2text-command "w3m -dump -T text/html -o display_link_number=true")
(setq mu4e-alert-interesting-mail-query "flag:unread AND maildir:/INBOX")
;; https://emacs.stackexchange.com/questions/52487/mu4e-how-to-stop-the-unarchiving-of-entire-threads-when-new-message-arrives
(setq mu4e-headers-include-related nil)
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/Sent Messages"       . ?s)
         ("/Trash"               . ?t)
         ("/Drafts"              . ?d)
         ("/Archive"             . ?a)
         ("/Reference"           . ?r)
         ;; ("/Waiting"             . ?w)
         ;; ("/Action Items"        . ?c)
         ))
;; sending emails
(setq user-mail-address "chongchong@astro.umd.edu"
      user-full-name  "ChongChong He"
      smtpmail-default-smtp-server "gaia.astro.umd.edu"
      ;; smtpmail-local-domain "account1.example.com"
      smtpmail-smtp-server "gaia.astro.umd.edu"
      ;; smtpmail-stream-type 'starttls
      ;; smtpmail-smtp-service 587
      smtpmail-debug-info t)
;; show images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
;; from: http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/#getting-set-up-with-mu-and-offlineimap
(setq sendmail-program "/usr/local/bin/msmtp"
      send-mail-function 'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)
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
;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
          "My settings for message composition."
          (set-fill-column 72)
          (flyspell-mode)))
;; (setq mu4e-compose-signature
;;    "ChongChong He\n")
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
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
;; (setq mu4e-compose-dont-reply-to-self t) ; ?
;; from: https://emacs.stackexchange.com/questions/52608/how-to-add-a-value-for-cc-or-reply-to-in-each-new-message/52609
(add-hook 'mu4e-compose-mode-hook
          (defun my-add-bcc ()
            "Add a cc: header."
            (save-excursion (message-add-header "cc: chongchong@astro.umd.edu\n"))))
;;
(add-to-list 'file-name-handler-alist '("Drafts/cur/" . draft-auto-save-buffer-name-handler))
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
;;
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
(setq mu4e-alert-email-notification-types '(count))

;; Configs from https://www.reddit.com/r/spacemacs/comments/c8omik/spacemacs_mu4e_emacsw3m_awesome/ that I don't understand
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-display-update-status-in-modeline t)
(setq mu4e-hide-index-messages t)
;; (setq mu4e-get-mail-command "offlineimap")
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
