;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:

;; A tree layout file explorer
(use-package treemacs-all-the-icons
  :straight t
  :defines winum-keymap
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :init
  (with-eval-after-load 'winum
    (bind-key (kbd "M-9") #'treemacs-select-window winum-keymap))
  ;; use all-the-icons theme
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons")
  :config
  (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (if (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'treemacs--fringe-indicator-bitmap
        (vector #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111
                #b00000111111))))

;; Projectile integration for treemacs
(use-package treemacs-projectile
  :straight t
  :after treemacs projectile
  :bind (([M-f8] . treemacs-projectile)
         :map projectile-command-map
         ("h" . treemacs-projectile)))

(use-package treemacs-magit
  :straight t
  :after treemacs magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
