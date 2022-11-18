;;; Commentary:
;;
;; Dashboard configurations.
;;

;;; Code:

(use-package dashboard
  :straight t
  :after all-the-icons projectile page-break-lines
  :diminish (dashboard-mode page-break-lines-mode)
  :functions (all-the-icons-faicon
              all-the-icons-material
              open-custom-file
              winner-undo
              widget-forward)
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("R" . restore-session)
         ("S" . open-custom-file)
         ("q" . quit-dashboard))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :init (dashboard-setup-startup-hook)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (setq dashboard-banner-logo-title "HAL9001 - 知行合一")
  (setq dashboard-startup-banner "/home/daiwz/Org/img/LAMDA_light.svg")
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))

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
