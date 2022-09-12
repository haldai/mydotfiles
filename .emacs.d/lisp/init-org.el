;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(use-package org
  :straight t
  :mode ("\\.org" . org-mode)
  :functions hydra-org-template/body
  :bind (:map org-mode-map
              ("C-c a" . org-agenda)
              ("C-c s" . org-schedule)
              ("C-c b" . org-switchb)
              ("C-c l" . org-store-link)
              ("C-c SPC" . insert-zero-width-space)
              ("ESC SPC" . insert-thin-space))
  :hook ((org-indent-mode . (lambda () (diminish 'org-indent-mode)))
         (org-src-mode . display-line-numbers-mode)
         (org-mode . variable-pitch-mode))
  :init
  (use-package org-contrib :straight t)
  (use-package simple-httpd :straight t)
  :config
  (setq org-agenda-files '("~/Org/agenda/")
        org-directory "~/Org"                       ; let's put files here
        org-use-property-inheritance t              ; it's convenient to have properties inherited
        org-log-done 'time                          ; having the time a item is done sounds convenient
        org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
        org-export-in-background t                  ; run export processes in external emacs process
        org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{})       ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}

  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cs" 'org-schedule)

  (add-hook 'text-mode-hook #'visual-line-mode)
  (remove-hook 'text-mode-hook #'auto-fill-mode)

  ;; latex preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; indent in source code block
  (setq org-src-tab-acts-natively t)

  (defun my/org-mode/load-prettify-symbols ()
    (interactive)
    (setq prettify-symbols-alist
          '(("#+begin_src" . ?)
            ("#+BEGIN_SRC" . ?)
            ("#+end_src" . ?)
            ("#+END_SRC" . ?)
            ("#+begin_example" . ?)
            ("#+BEGIN_EXAMPLE" . ?)
            ("#+begin_quote" . ?)
            ("#+BEGIN_QUOTE" . ?)
            ("#+end_example" . ?)
            ("#+END_EXAMPLE" . ?)
            ("#+end_quote" . ?)
            ("#+END_QUOTE" . ?)
            ("#+begin_center" . ?)
            ("#+BEGIN_CENTER" . ?)
            ("#+end_center" . ?)
            ("#+END_CENTER" . ?)
            ("#+caption" . ?)
            ("#+CAPTION" . ?)
            ("#+header:" . ?)
            ("#+HEADER:" . ?)
            ("#+name:" . ?)
            ("#+NAME:" . ?)
            ("#+begin_notes" . ?)
            ("#+BEGIN_NOTES" . ?)
            ("#+end_notes" . ?)
            ("#+END_NOTES" . ?)
            ("#+results:" . ?)
            ("#+RESULTS:" . ?)
            ("#+call:" . ?)
            ("#+CALL:" . ?)
            (":PROPERTIES:" . ?)
            (":properties:" . ?)
            (":END:" . ?)
            (":end:" . ?)
            (":LOGBOOK:" . ?)
            (":logbook:" . ?)))
    (prettify-symbols-mode 1))
  (if (char-displayable-p ?)
      (add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)
    nil)

  (add-to-list 'org-export-backends 'md)

  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar") ;; ditaa

  (use-package websocket :straight t)
  (use-package zmq :straight t)

  ;; jupyter mode
  (use-package jupyter
    :straight (emacs-jupyter :type git :host github :repo "nnicandro/emacs-jupyter")
    :bind (("C-c C-x r" . jupyter-repl-restart-kernel)
           ("C-c C-x h" . jupyter-org-restart-and-execute-to-point))
    :config
    (setq jupyter--debug t))

  ;; ansi colors
  (defun display-ansi-colors ()
    (ansi-color-apply-on-region (point-min) (point-max)))

  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors)


  ;; use xelatex for latex export
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("xelatex" "lualatex" "pdflatex")))
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
  (setq org-latex-pdf-process '("latexmk -f -pdf -xelatex -interaction=nonstopmode -output-directory=%o %f"))

  ;; custom babel latex source code preambles
  (setq org-babel-latex-preamble '(lambda (_)
                                    "\\documentclass[preview]{standalone}
\\usepackage{tikz}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{xeCJK}
\\def\\pgfsysdriver{pgfsys-dvisvgm4ht.def}"))

  ;; mime support
  (use-package org-mime :straight t)

  ;; fonts
  (setq org-tags-column -77)
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Vollkorn" :height 1.1))))
   '(fixed-pitch ((t (:family "SauceCodePro Nerd Font Mono" :slant normal :weight normal :height .95))))
   '(org-level-1 ((t (:inherit variable-pitch :height 1.5 :weight bold))))
   '(org-level-2 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-level-3 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-level-4 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-level-5 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-level-6 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-level-7 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-level-8 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-document-title ((t (:inherit variable-pitch :height 2.0 :weight bold))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-todo ((t (:inherit fixed-pitch))))
   '(org-done ((t (:inherit fixed-pitch))))
   '(org-keywords ((t (:inherit fixed-pitch))))
   '(org-formula ((t (:inherit fixed-pitch :height 0.8))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:inherit variable-pitch :slant italic :height 0.9))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch) :weight bold))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:inherid fixed-pitch :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch) :weight bold))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  ;; no export of zero-width spaces
  (defun +org-export-remove-zero-width-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string "\u200B" "" text)))

  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-zero-width-space t)

  ;; list bullet sequence
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

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
                               (prolog . t)
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

  (add-to-list 'org-structure-template-alist '("d" . "src dot :file TMP.svg"))
  (add-to-list 'org-structure-template-alist '("jj" . "src jupyter-julia"))
  (add-to-list 'org-structure-template-alist '("j" . "src julia :exports code :eval never"))
  (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python"))
  (add-to-list 'org-structure-template-alist '("ls" . "src latex :file TMP.svg :headers '(\"\\\\usepackage{tikz}\\\\n\\\\usepackage{xeCJK}\\\\n\") :results file raw"))
  (add-to-list 'org-structure-template-alist '("ln" . "src latex :file TMP.png :imagemagick yes :headers '(\"\\\\usepackage{tikz}\\\\n\\\\usepackage{xeCJK}\\\\n\") :iminoptions \"-density 600\" :results file raw"))
  (add-to-list 'org-structure-template-alist '("lp" . "src latex :file TMP.pdf :headers '(\"\\\\usepackage{tikz}\\\\n\\\\usepackage{xeCJK}\\\\n\") :results file raw"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("pl" . "src prolog"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("n"  . "notes"))

  ;; default headers
  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:comments . "link")))

  ;; juypter-julia settings
  (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                      (:session . "jl")
                                                      (:kernel . "julia-1.7")
                                                      (:exports . "both")))

  ;; juypter-python settings
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3")
                                                       (:exports . "both")))

  ;; latex
  (setq org-babel-default-header-args:latex '((:fit . "yes")
                                              (:exports . "results")
                                              ;; (:imagemagick . "yes")
                                              (:iminoptions . "-density 300")
                                              (:imoutoptions . "-flatten")
                                              (:eval . "never-export")))

  ;; do not evaluate code blocks while exporting
  (setq org-export-babel-evaluate t)

  (use-package htmlize :straight t)

  ;; Make invisible parts of Org elements appear visible.
  (use-package org-appear
    :straight
    (org-appear :type git :host github :repo "awth13/org-appear")
    :custom
    (org-appear-autolinks t)
    (org-appear-submarkers t)
    :hook (org-mode . org-appear-mode))

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

  ;; preview latex with svg
  (setq org-preview-latex-default-process 'dvisvgm)

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
    (org-tree-slide-presentation-profile)
    (setq org-tree-slide-skip-outline-level 5))

  ;; Visually summarize progress
  (use-package org-dashboard
    :straight t)

  (require 'org-tempo)

  (use-package ob-julia
    :straight (ob-julia :type git :host nil :repo "https://git.nixo.xyz/nixo/ob-julia"))

  ;; reveal
  (use-package ox-reveal
    :requires ob-julia
    :straight (ox-reveal :type git :host github :repo "yjwen/org-reveal")
    :bind (("C-c \" B" . org-reveal-export-to-html-and-browse)
           ("C-c \" R" . org-reveal-export-to-html)))
  ;; (use-package emacs-reveal
  ;;   :straight (emacs-reveal :type git :host gitlab :repo "oer/emacs-reveal")))

  ;; Translate capital keywords (old) to lower case (new)
  (defun org-syntax-convert-keyword-case-to-lower ()
    "Convert all #+KEYWORDS to #+keywords."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0)
            (case-fold-search nil))
        (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
          (unless (s-matches-p "RESULTS" (match-string 0))
            (replace-match (downcase (match-string 0)) t)
            (setq count (1+ count))))
        (message "Replaced %d occurances" count))))

  (add-hook 'org-mode-hook
            (lambda () (add-hook 'before-save-hook #'org-syntax-convert-keyword-case-to-lower nil 'local)))

  ;; Super agenda

  (setq org-todo-keywords '((sequence "TODO(T)" "DOING(I)" "HANGUP(H)" "|" "DONE(D)" "CANCEL(C)")
                            (sequence "⚑(t)" "🏴(i)" "❓(h)" "|" "✔(d)" "✘(c)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-log-done 'time
        org-startup-indented t
        org-startup-with-inline-images t
        org-ellipsis (if (char-displayable-p ?) " " nil)
        org-cycle-separator-lines -1
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-image-actual-width nil)

  (use-package org-super-agenda
    :straight t
    :after org-agenda
    :hook (org-agenda-mode . org-super-agenda-mode))

  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-tags-column 100 ;; from testing this seems to be a good value
        org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "important"
                                   :tag "important"
                                   :priority "A"
                                   :order 6)
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Due Soon"
                                   :deadline future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :face error
                                   :order 7)
                            (:name "meeting"
                                   :tag "meeting"
                                   :order 7)
                            (:name "Waiting"
                                   :todo "WAITING"
                                   :order 20)
                            (:name "Trivial"
                                   :priority<= "E"
                                   :tag ("trivial" "unimportant")
                                   :todo ("SOMEDAY" )
                                   :order 90)
                            (:discard (:tag ("Chore" "Routine" "Daily"))))))))))))

(use-package org-roam
  :straight t
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Org"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d" . org-id-get-create)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
