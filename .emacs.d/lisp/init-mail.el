;;; Commentary:
;;
;; mu4e configurations.
;;

;;; Code:

(use-package mu4e
  :straight t
  :commands (mu4e mu4e-compose-new)
  :config
  (setq mu4e-maildir "~/Mail"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t)

  ;;; Mail directory shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/work/INBOX" . ?w)
          ("/personal/Inbox" . ?p)))

;;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))
  ;; multiple accounts
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Work"
             :enter-func (lambda () (mu4e-message "Switch to the Work context"))
             :leave-func (lambda () (mu4e-message "Leaving Work context"))
             ;; leave-fun not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "w.dai@imperial.ac.uk")))
             :vars '( (user-mail-address . "w.dai@imperial.ac.uk")
                      (user-full-name . "Wang-Zhou Dai")
                      (mu4e-compose-signature . (concat
                                                 "Kind regards,\n"
                                                 "\tWang-Zhou\n"))
                      (mu4e-sent-folder . "/work/&XfJT0ZABkK5O9g-")
                      (mu4e-drafts-folder . "/work/&g0l6Pw-")
                      (mu4e-trash-folder . "/work/&XfJSIJZkkK5O9g-")
                      (mu4e-attachment-dir "/work/Files")))
           ,(make-mu4e-context
             :name "Private"
             :enter-func (lambda () (mu4e-message "Switch to the Private context"))
             :leave-func (lambda () (mu4e-message "Leaving Work context"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to "dai.wzero@hotmail.com")))
             :vars '( (user-mail-address . "dai.wzero@hotmail.com")
                      (user-full-name . "Wang-Zhou Dai")
                      (mu4e-compose-signature . (concat
                                                 "Kind regards,\n"
                                                 "\tWang-Zhou.\n"))
                      (mu4e-sent-folder . "/personal/Sent")
                      (mu4e-drafts-folder . "/personal/Drafts")
                      (mu4e-trash-folder . "/personal/Trash")
                      (mu4e-attachment-dir "/personal/Files")))))

  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.
  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)
  ;; compose with the current context if no context matches;
  ;; default is to ask
  ;; '(setq mu4e-compose-context-policy nil)
  )

(provide 'init-mail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mail.el ends here
