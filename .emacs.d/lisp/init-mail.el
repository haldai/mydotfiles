;;; Commentary:
;;
;; mu4e configurations.
;;

;;; Code:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(load-file "~/.config/mu4e/mu4e-config.el")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


;; attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800)


;; (setq mu4e-html2text-command "html2text -utf8 -width 72") ;; nil "Shel command that converts HTML
;; ref: http://emacs.stackexchange.com/questions/3051/how-can-i-use-eww-as-a-renderer-for-mu4e
(defun my-render-html-message ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))
(setq mu4e-html2text-command 'my-render-html-message)

;; yt
(setq mu4e-view-prefer-html t) ;; try to render
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t) ;; read in browser
;; mu4e as default email agent in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; convert org content in mu4e to html and send
(require 'org-mime)
(require 'mu4e-org)

(defun htmlize-and-send ()
  "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  (interactive)
  (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
    (org-mime-htmlize)
    (message-send-and-exit)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)
(add-hook 'mu4e-compose-mode-hook
          (lambda () (local-set-key (kbd "C-c C-o") #'org-mu4e-compose-org-mode)))

;; give me ISO(ish) format date-time stamps in the header list
(setq  mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; customize the reply-quote-string
;; M-x find-function RET message-citation-line-format for docs
(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
      '( (:date          .  25)
         (:flags         .   6)
         (:from          .  22)
         (:subject       .  nil)))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; attachments go here
(setq mu4e-attachment-dir  "~/Downloads")

;; send attachment directly from dired
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; use C-1 c for add cc if already in composition mode
(define-key mu4e-compose-mode-map (kbd "C-1 c") 'message-goto-cc)

;; signature
(setq mu4e-compose-signature-auto-include nil)
(defun my-mu4e-choose-signature ()
  "Insert one of a number of sigs"
  (interactive)
  (let ((message-signature
         (mu4e-read-option "Signature:"
                           '(("formal" .
                              (concat
                               "Wang-Zhou DAI, PhD\n"
                               "Associate Professor\n"
                               "Nanjing University, Suzhou Campus.\n"
                               "https://daiwz.net\n"))
                             ("informal" .
                              "Wang-Zhou.\n")))))
    (message-insert-signature)))

(add-hook 'mu4e-compose-mode-hook
          (lambda () (local-set-key (kbd "C-c C-w") #'my-mu4e-choose-signature)))


(provide 'init-mail)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mail.el ends here
