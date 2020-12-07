;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(use-package org
  :straight org-plus-contrib
  :functions hydra-org-template/body
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :hook ((org-indent-mode . (lambda() (diminish 'org-indent-mode)))
         (org-src-mode . display-line-numbers-mode)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode))
  :config

  ;; latex preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

  ;; indent in source code block
  (setq org-src-tab-acts-natively t)

  (setq org-agenda-files '("~/org")
        org-todo-keywords '((sequence "TODO(T)" "DOING(I)" "HANGUP(H)" "|" "DONE(D)" "CANCEL(C)")
                            (sequence "⚑(t)" "🏴(i)" "❓(h)" "|" "✔(d)" "✘(c)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-log-done 'time
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-image-actual-width 500)

  (add-to-list 'org-export-backends 'md)

  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar") ;; ditaa

  ;; jupyter mode
  (use-package jupyter
    :straight t
    :bind (("C-c C-x r" . jupyter-repl-restart-kernel)
           ("C-c C-x h" . jupyter-org-restart-and-execute-to-point)))

  ;; use xelatex for latex export
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

  ;; mime support
  (use-package org-mime :straight t)

  ;; fonts
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Vollkorn" :height 1.2))))
   '(fixed-pitch ((t (:family "SauceCodePro Nerd Font Mono" :slant normal :weight normal :height 0.9))))
   '(org-level-1 ((t (:family "EB Garamond SC" :height 1.2))))
   '(org-level-2 ((t (:family "EB Garamond SC" :height 1.1))))
   '(org-level-3 ((t (:family "EB Garamond SC" :height 1.0))))
   '(org-level-4 ((t (:family "EB Garamond SC" :height 1.0))))
   '(org-level-5 ((t (:family "EB Garamond SC" :height 1.0))))
   '(org-level-6 ((t (:family "EB Garamond SC" :height 1.0))))
   '(org-level-7 ((t (:family "EB Garamond SC" :height 1.0))))
   '(org-level-8 ((t (:family "EB Garamond SC" :height 1.0))))
   '(org-document-title ((t (:family "EB Garamond" :height 2.0 :weight bold))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-todo ((t (:inherit fixed-pitch))))
   '(org-done ((t (:inherit fixed-pitch))))
   '(org-keywords ((t (:inherit fixed-pitch))))
   '(org-formula ((t (:inherit fixed-pitch :height 0.8))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:inherit variable-pitch :slant italic :height 0.9))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:inherid fixed-pitch :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  ;; More fancy UI
  (use-package org-bullets
    :straight t
    :hook
    (org-mode . org-bullets-mode))

  (use-package org-ref
    :straight t
    :init (setq org-ref-completion-library 'org-ref-ivy-cite)
    :config
    ;; see org-ref for use of these variables
    (setq org-ref-bibliography-notes "~/Documents/bibliography/notes.org"
          org-ref-default-bibliography '("~/Documents/bibliography/references.bib")
          org-ref-pdf-directory "~/Documents/bibliography/bibtex-pdfs/"))

  (use-package org-fancy-priorities
    :straight t
    :diminish
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (unless (char-displayable-p ?❗)
      (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (latex . t)
                               (perl . t)
                               (julia . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (ditaa . t)
                               (dot . t)
                               (gnuplot . t)
                               (plantuml . t)
                               (jupyter . t)))

  (setq inferior-julia-program-name "julia")

  ;; ob-sh renamed to ob-shell since 26.1.
  (cl-pushnew '(shell . t) load-language-list)

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  (org-babel-jupyter-override-src-block "python")

  (add-to-list 'org-structure-template-alist '("jj" . "src jupyter-julia"))
  (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python"))
  (add-to-list 'org-structure-template-alist '("jl" . "src julia"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("pl" . "src prolog"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  ;; juypter-julia settings
  (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                      (:session . "jl")
                                                      (:kernel . "julia-1.5")
                                                      (:exports . "both")))

  ;; juypter-python settings
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3")
                                                       (:exports . "both")))
  ;; do not evaluate code blocks while exporting
  (setq org-export-babel-evaluate t)

  (use-package htmlize :straight t)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :straight t
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :straight t
    :hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
    :straight t
    :diminish org-preview-html-mode)

  ;; Presentation
  (use-package org-tree-slide
    :straight t
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("C-<f7>" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
                ("<left>" . org-tree-slide-move-previous-tree)
                ("<right>" . org-tree-slide-move-next-tree)
                ("S-SPC" . org-tree-slide-move-previous-tree)
                ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  ;; Visually summarize progress
  (use-package org-dashboard
    :straight t)

  (require 'org-tempo)

  ;; reveal
  (use-package ox-reveal
    :straight (ox-reveal :type git :host github :repo "yjwen/org-reveal")))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
