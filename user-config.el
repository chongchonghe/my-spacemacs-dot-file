;;(load-directory "~/.spacemacs.d/my-lisp")
(global-linum-mode t)
(global-set-key (kbd "M-v") 'yank)

(defun spacemacs//linum-update-window-scale-fix (win)
  "Fix linum for scaled text in the window WIN."
  (set-window-margins win 6))

(evil-ex-define-cmd "q" 'kill-this-buffer)
;; Need to type out :quit to close emacs
(evil-ex-define-cmd "quit" 'evil-quit)

(unbind-key (kbd "C-u") evil-normal-state-map)

;; org mode
;; https://stackoverflow.com/questions/11670654/how-to-resize-images-in-org-mode
;; (setq-default dotspacemacs-configuration-layers
;;               '(
;;                 (org :variables org-projectile-file "~/Dropbox/orgfiles/tasks.org")
;;                 ))
;; (setq-default dotspacemacs-configuration-layers
;;               '((org :variables org-enable-github-support t)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;The following code causes the error:
;;Error (use-package): org-projectile/:config: Symbol’s function definition is void: org-projectile:per-repo
;; (with-eval-after-load 'org-agenda
;;   (require 'org-projectile)
;;   (push (org-projectile:todo-files) org-agenda-files))

(load-file "~/.my-elips/org.el")

;; (setq org-descriptive-links nil)

(require 'ess-site)
(setq  inferior-julia-program-name "/Applications/Julia-1.4.app/Contents/Resources/julia/bin/julia")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (julia . t)
   (shell . t)
   (python . t)
   ))

(setq
 org-export-babel-evaluate nil
 org-confirm-python-evaluate nil
 org-confirm-babel-evaluate nil
 org-confirm-C++-evaluate nil
 )

(setq org-present-text-scale 2)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 ;;(toggle-frame-fullscreen)
                 ))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)
                 ;;(toggle-frame-fullscreen)
                 ))))

(require 'yasnippet)
(yas/load-directory "~/.emacs.d.bk/snippets")
;; (setq yas-triggers-in-field t)
;; (yas/initialize)
;; (yas-minor-mode 1)

;; (setq-default dotspacemacs-configuration-layers
;; 	'((mu4e :variables
;; 		mu4e-installation-path "/usr/local/Cellar/mu/1.2.0_1/share/emacs/site-lisp")))
;; (load-file "~/.spacemacs.d/my-lisp/mail.el")
;; (load-file "~/.spacemacs.d/my-lisp/mininum.el")
;; (load-file "~/.my-elips/mail2.el")

(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent ;; use mu4e for e-mail in emacs
      ;; for mbsync, when move a message to All Mail as archiving
      mu4e-change-filenames-when-moving t
      mu4e-view-show-addresses t
      mu4e-maildir "~/Maildir"
      mu4e-get-mail-command "mbsync umd"
      mu4e-attachment-dir "~/Documents/MailAttachments"
      mu4e-update-interval 60
      )

(setq mu4e-html2text-command
      "textutil -stdin -format html -convert txt -stdout")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq
 sendmail-program "/usr/local/bin/msmtp"
 message-sendmail-f-is-evil t
 message-sendmail-extra-arguments '("--read-envelope-from")
 ;; send-mail-function 'smtpmail-send-it
 ;; message-send-mail-function 'message-send-mail-with-sendmail
 message-send-mail-function 'smtpmail-send-it
 user-full-name "ChongChong He"
 user-mail-address "che1234@umd.edu"
 smtpmail-smtp-user "che1234@umd.edu"
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 ;; smtpmail-stream-type 'starttls
 starttls-use-gnutls t
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 ;; smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
 smtpmail-debug-info t
 mu4e-compose-keep-self-cc t
 mu4e-compose-dont-reply-to-self nil
 ;; smtpmail-auth-credentials '(("smtp.gmail.com" 587 "che1234@umd.edu" nil))
 ;; testing: removing all the folder def since I don't need
 ;; it. Later will set folder shortcuts
 mu4e-trash-folder "/umd/trash"
 mu4e-sent-folder "/umd/[Gmail].All Mail"
 mu4e-refile-folder "/umd/[Gmail].All Mail"
 mu4e-drafts-folder "/umd/[Gmail].Drafts"
 ;; don't save message to Sent Messages, IMAP takes care of this
 mu4e-maildir-shortcuts '( ("/umd/INBOX"               . ?i)
                           ("/umd/[Gmail].Sent Mail"   . ?s)
                           ;; ("/umd/trash"               . ?t)
                           ("/umd/[Gmail].Drafts"      . ?d)
                           ("/umd/[Gmail].Starred"     . ?*)
                           ("/umd/[Gmail].All Mail"    . ?a)
                           ("/umd/f.Reference"         . ?r)
                           ("/umd/f.Followup"          . ?f)
                           ("/umd/f.Todo"              . ?t)
                         )
 )

(add-hook 'mu4e-compose-mode-hook
          (defun my-add-bcc ()
            "Add a cc: header."
            (save-excursion (message-add-header "Cc: che1234@umd.edu\n"))))

;; ;; from https://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
;; (add-hook 'mu4e-compose-mode-hook
;;           (lambda()
;;             (let* ((ctx (mu4e-context-current))
;;                    (name (if ctx (mu4e-context-name ctx))))
;;               (when name
;;                 (cond
;;                  ((string= name "astro")
;;                   (save-excursion (message-add-header "Cc: chongchong@astro.umd.edu\n")))
;;                  ((string= name "terpmail")
;;                   (save-excursion (message-add-header "Cc: che1234@terpmail.umd.edu\n")))
;;                  ((string= name "umd")
;;                   (save-excursion (message-add-header "Cc: che1234@umd.edu\n")))
;; 		 )))))

(defun htmlize-and-send ()
  "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  (interactive)
  (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
    (org-mime-htmlize)
    (org-mu4e-compose-org-mode)
    (message-send-and-exit)))

(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y at %R, %f wrote:\n")

(setq mu4e-enable-notifications t)
(with-eval-after-load 'mu4e-alert
  ;; Enable Desktop notifications
  ;; (mu4e-alert-set-default-style 'notifications)) ; For Linux.
  ;; (mu4e-alert-set-default-style 'libnotify))  ; Alternative for Linux
  (mu4e-alert-set-default-style 'notifier))   ; For macOS (through the
                                              ; terminal notifier app).
  ;; (mu4e-alert-set-default-style 'growl))      ; Alternative for macOS.

(setq TeX-auto-save t)
(add-hook 'LaTeX-mode-hook 'hs-minor-mode)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(setq TeX-PDF-mode t)	      ;; Compile documents to PDF by default
;; The following two lines make forward syncing possible. Tested on 2020-02-17
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; CDLaTeX: bug, won't recognize multiple files if turned on
;; (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; Make emacs aware of multi-file projects: do not need on spacemacs. not working when cdlatex is on
;; (setq-default TeX-master nil)
(electric-pair-mode)
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)
             ))

;; (setq TeX-parse-self t)
;; ;; (add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; ;; (setq reftex-default-bibliography '("/Users/chongchonghe/Documents/bib_tmp.bib"))
;; ;; (setq helm-bibtex-bibliography '("/Users/chongchonghe/Documents/bib_tmp.bib"))

;; (setq TeX-source-correlate-start-server t)
;; ;;
;; (setq TeX-source-correlate-mode t) ; ?
;; (setq TeX-source-correlate-method 'synctex) ; ?

(require 'elfeed)

(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))

;; face for starred articles
(defface elfeed-search-starred-title-face
  '((t :foreground "#f77"))
  "Marks a starred Elfeed entry.")

(push '(star elfeed-search-starred-title-face) elfeed-search-face-alist)

(defun concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
  (mapconcat
   (lambda (author) (plist-get author :name))
   authors-list ", "))

(defun my-search-print-fn (entry)
  "Print ENTRY to the buffer."

  (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
         (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                         elfeed-goodies/tag-column-width 4))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               title-width)
                        :left))
         (tag-column (elfeed-format-column
                      tags-str 13
                      :left))
         (feed-column (elfeed-format-column
                       feed-title 11
                       :left))
         (entry-authors (concatenate-authors
                         (elfeed-meta entry :authors)))
         (authors-column (elfeed-format-column
                          entry-authors 40
                          :left))
         )

    (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
        (progn
          (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
          (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
          (insert (propertize authors-column
                              'face 'elfeed-search-date-face
                              'kbd-help entry-authors) " ")
          (insert (propertize title 'face title-faces 'kbd-help title))
          )
      (insert (propertize title 'face title-faces 'kbd-help title)))))

(setq elfeed-search-print-entry-function #'my-search-print-fn)

;; ;; Use cmd key for meta
;; ;; https://superuser.com/questions/297259/set-emacs-meta-key-to-be-the-mac-key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)
;; ;; make swithing windows easier
(global-set-key (kbd "M-j") 'evil-window-down)
(global-set-key (kbd "M-k") 'evil-window-up)
(global-set-key (kbd "M-h") 'evil-window-left)
(global-set-key (kbd "M-l") 'evil-window-right)
;; trying bind-key.el: https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings

;; ;; not working
;; (use-package bind-key
;;   :config
;;   (bind-keys*
;;    ("M-h" . 'evil-window-left)
;;    ("M-l" . 'evil-window-right)
;;    ("M-j" . 'evil-window-up)
;;    ("M-k" . 'evil-window-down)
;;    )
;; )
;; ;; not working
;; (define-key org-mode-map (kbd "M-j") nil)
;; (global-set-key (kbd "C-j") 'windmove-down)
;; (global-set-key (kbd "M-<up>") 'windmove-up)
;; (global-set-key (kbd "M-<right>") 'windmove-right)
;; (global-set-key (kbd "M-<left>") 'windmove-left)
;; Abandoned. Not working in org mode under spacemacs
;; (global-set-key (kbd "M-j") 'windmove-down)
;; (global-set-key (kbd "M-k") 'windmove-up)
;; (global-set-key (kbd "M-h") 'windmove-left)
;; (global-set-key (kbd "M-l") 'windmove-right)
;; (global-set-key (kbd "s-x") nil) ; my habits causes mistake
;; ;; https://orgmode.org/manual/Conflicts.html
;; ;; Make windmove work in Org mode:
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'evil-windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)
;; (define-key org-mode-map (kbd "M-h") 'windmove-left) ;; org conflicts
;; ;; Keybindings
;; (define-key evil-normal-state-map (kbd "M-h") #'evil-window-left)
;; (define-key evil-normal-state-map (kbd "M-j") #'evil-window-down)
;; (define-key evil-normal-state-map (kbd "M-k") #'evil-window-up)
;; (define-key evil-normal-state-map (kbd "M-l") #'evil-window-right)

(require 'atomic-chrome)
(atomic-chrome-start-server)
