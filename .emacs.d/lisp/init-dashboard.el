;;; Commentary:
;;
;; Dashboard configurations.
;;

;;; Code:

(use-package dashboard
  :straight t
  :after all-the-icons projectile
  :diminish (dashboard-mode page-break-lines-mode)
  :defines (persp-save-dir persp-special-last-buffer)
  :functions (all-the-icons-faicon
              all-the-icons-material
              open-custom-file
              persp-get-buffer-or-null
              persp-load-state-from-file
              persp-switch-to-buffer
              winner-undo
              widget-forward)
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("R" . restore-session)
         ("L" . persp-load-state-from-file)
         ("S" . open-custom-file)
         ("q" . quit-dashboard))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :init (dashboard-setup-startup-hook)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (setq dashboard-banner-logo-title "HAL9001 - Enjoy Programming & Writing")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)))

  (defun my-banner-path (&rest _)
    "Return the full path to banner."
    (expand-file-name "banner.txt" user-emacs-directory))
  (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                    (eq (treemacs-current-visibility) 'visible))
               2
             1))
        (setq dashboard-recover-layout-p t))

    (delete-other-windows)

    ;; Refresh dashboard buffer
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)

    ;; Jump to the first section
    (goto-char (point-min))
    (dashboard-goto-recent-files))

  (defun restore-session ()
    "Restore last session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (condition-case-unless-debug err
          (persp-load-state-from-file)
        (error
         (message "Error: Unable to restore last session -- %s" err)))
      (when (persp-get-buffer-or-null persp-special-last-buffer)
        (persp-switch-to-buffer persp-special-last-buffer))))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
               (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil)))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (funcall (local-key-binding "r")))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (funcall (local-key-binding "p")))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (funcall (local-key-binding "m")))

  (defun dashboard-insert-buttons (_list-size)
    (insert "\n")
    (insert (make-string (max 0 (floor (/ (- dashboard-banner-length
                                             (if (display-graphic-p) 51 42)
                                             ) 2))) ?\ ))
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Restore previous session"
                   :action (lambda (&rest _) (restore-session))
                   :mouse-face 'highlight
                   :tag (concat
                         (if (display-graphic-p)
                             (concat
                              (all-the-icons-material "restore"
                                                      :height 1.35
                                                      :v-adjust -0.24
                                                      :face 'font-lock-keyword-face)
                              (propertize " " 'face 'variable-pitch)))
                         (propertize "Session" 'face 'font-lock-keyword-face)))
    (insert " ")
    (widget-create 'file-link
                   :tag (concat
                         (if (display-graphic-p)
                             (concat
                              (all-the-icons-faicon "cog"
                                                    :height 1.2
                                                    :v-adjust -0.1
                                                    :face 'font-lock-keyword-face)
                              (propertize " " 'face 'variable-pitch)))
                         (propertize "Settings" 'face 'font-lock-keyword-face))
                   :help-echo "Open custom file"
                   :mouse-face 'highlight
                   custom-file)
    (insert " ")
    (insert "\n")
    (insert "\n")
    (insert (make-string (max 0 (floor (/ (- dashboard-banner-length
                                             (if (display-graphic-p) 49 51))
                                          2))) ?\ ))
    (insert (concat
             (propertize (format "Emacs loaded in %s "
                                 (emacs-init-time))
                         'face 'font-lock-comment-face)
             (propertize "(h/? for help)"
                         'face 'font-lock-doc-face))))

  (add-to-list 'dashboard-item-generators '(buttons . dashboard-insert-buttons))
  (add-to-list 'dashboard-items '(buttons))

  (dashboard-insert-startupify-lists)

  (defhydra dashboard-hydra (:color red :columns 3)
    "Help"
    ("<tab>" widget-forward "Next Widget")
    ("C-i" widget-forward "Prompt")
    ("<backtab>" widget-backward "Previous Widget")
    ("RET" widget-button-press "Press Widget" :exit t)
    ("g" dashboard-refresh-buffer "Refresh" :exit t)
    ("}" dashboard-next-section "Next Section")
    ("{" dashboard-previous-section "Previous Section")
    ("r" dashboard-goto-recent-files "Recent Files")
    ("p" dashboard-goto-projects "Projects")
    ("m" dashboard-goto-bookmarks "Bookmarks")
    ("H" browse-homepage "Browse Homepage" :exit t)
    ("R" restore-session "Restore Previous Session" :exit t)
    ("L" persp-load-state-from-file "List Saved Sessions" :exit t)
    ("S" open-custom-file "Settings" :exit t)
    ("<f2>" open-dashboard "Open Dashboard" :exit t)
    ("q" quit-dashboard "Quit Dashboard" :exit t)
    ("C-g" nil "quit"))
  (bind-keys :map dashboard-mode-map
             ("h" . dashboard-hydra/body)
             ("?" . dashboard-hydra/body)))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
