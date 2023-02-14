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
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

  ;; indent in source code block
  (setq org-src-tab-acts-natively t)

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

  ;; automatically remove latex files
  ;; https://answer-id.com/53623039
  (setq org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex" "tex~" "aux"
                "idx" "log" "out" "toc" "nav"
                "snm" "vrb" "dvi" "fdb_latexmk"
                "blg" "brf" "fls" "entoc" "ps"
                "spl" "bbl" "xdv" "pyg")))

  ;; latex export format
  (setq org-latex-classes
        '(("article"
           "
\\documentclass[10pt,a4paper]{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\RequirePackage{xeCJK}
\\RequirePackage{imakeidx}
\\RequirePackage{xstring}
\\RequirePackage{xcolor}
\\RequirePackage{tikz}
\\RequirePackage{amsmath}
\\RequirePackage{amssymb}
\\definecolor{zenfg+1}{HTML}{FFFFEF}
\\definecolor{zenfg}{HTML}{DCDCCC}
\\definecolor{zenfg-1}{HTML}{656555}
\\definecolor{zenbg-2}{HTML}{000000}
\\definecolor{zenbg-1}{HTML}{2B2B2B}
\\definecolor{zenbg-05}{HTML}{383838}
\\definecolor{zenbg}{HTML}{3F3F3F}
\\definecolor{zenbg+05}{HTML}{494949}
\\definecolor{zenbg+1}{HTML}{4F4F4F}
\\definecolor{zenbg+2}{HTML}{5F5F5F}
\\definecolor{zenbg+3}{HTML}{6F6F6F}
\\definecolor{zenred+2}{HTML}{ECB3B3}
\\definecolor{zenred+1}{HTML}{DCA3A3}
\\definecolor{zenred}{HTML}{CC9393}
\\definecolor{zenred-1}{HTML}{BC8383}
\\definecolor{zenred-2}{HTML}{AC7373}
\\definecolor{zenred-3}{HTML}{9C6363}
\\definecolor{zenred-4}{HTML}{8C5353}
\\definecolor{zenred-5}{HTML}{7C4343}
\\definecolor{zenred-6}{HTML}{6C3333}
\\definecolor{zenorange}{HTML}{DFAF8F}
\\definecolor{zenyellow}{HTML}{F0DFAF}
\\definecolor{zenyellow-1}{HTML}{E0CF9F}
\\definecolor{zenyellow-2}{HTML}{D0BF8F}
\\definecolor{zengreen-5}{HTML}{2F4F2F}
\\definecolor{zengreen-4}{HTML}{3F5F3F}
\\definecolor{zengreen-3}{HTML}{4F6F4F}
\\definecolor{zengreen-2}{HTML}{5F7F5F}
\\definecolor{zengreen-1}{HTML}{6F8F6F}
\\definecolor{zengreen}{HTML}{7F9F7F}
\\definecolor{zengreen+1}{HTML}{8FB28F}
\\definecolor{zengreen+2}{HTML}{9FC59F}
\\definecolor{zengreen+3}{HTML}{AFD8AF}
\\definecolor{zengreen+4}{HTML}{BFEBBF}
\\definecolor{zencyan}{HTML}{93E0E3}
\\definecolor{zenblue+3}{HTML}{BDE0F3}
\\definecolor{zenblue+2}{HTML}{ACE0E3}
\\definecolor{zenblue+1}{HTML}{94BFF3}
\\definecolor{zenblue}{HTML}{8CD0D3}
\\definecolor{zenblue-1}{HTML}{7CB8BB}
\\definecolor{zenblue-2}{HTML}{6CA0A3}
\\definecolor{zenblue-3}{HTML}{5C888B}
\\definecolor{zenblue-4}{HTML}{4C7073}
\\definecolor{zenblue-5}{HTML}{366060}
\\definecolor{zenmagenta}{HTML}{DC8CC3}
\\setmainfont{Vollkorn}
\\setCJKmainfont[BoldFont={FZCuHeiSong-B-JF}]{FZPingXianYaSong-R-GBK}
\\AtBeginEnvironment{quote}{\\quotefont\\small}
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.0}
\\hypersetup{
  colorlinks=true,
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}
[EXTRA]
"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  ;; [FIXME]
  ;; 原本是不要讓 org 插入 hypersetup（因為 org-mode 這部份設計成沒辦法自訂，或許可以去 report 一下？
  ;; 改成自行插入，但這樣 pdfcreator 沒辦法根據 Emacs 版本插入，pdfkeyword 也會無效...幹。
  (setq org-latex-with-hyperref t)

  ;; Export source code using minted
  (setq org-latex-listings 'minted)

  (setq org-latex-default-packages-alist
        '(("" "nopageno" t)
          ("" "hyperref" t)
          ("" "fontspec" t)
          ("" "etoolbox" t) ;; Quote 部份的字型設定
          ("margin=2cm" "geometry" nil)
          ;; ("AUTO" "inputenc" t)
          ;; ("" "fixltx2e" nil)
          ("dvipdfmx" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "textcomp" t)
          ("" "marvosym" t)
          ("" "wasysym" t)
          ("" "multicol" t)  ; 這是我另外加的，因為常需要多欄位文件版面。
          ("" "amssymb" t)
          ("" "fancyhdr" nil) ;; 页眉页脚
          ("cache=false" "minted" nil) ;; Code color
          "\\tolerance=1000"))

  ;; custom babel latex source code preambles
  (setq org-babel-latex-preamble '(lambda (_)
                                    "\\documentclass[preview]{standalone}
\\usepackage{tikz}
\\usepackage{xeCJK}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\setmainfont{Vollkorn}
\\setCJKmainfont[BoldFont={FZCuHeiSong-B-JF}]{FZPingXianYaSong-R-GBK}
\\def\\pgfsysdriver{pgfsys-dvisvgm4ht.def}"))

  ;; mime support
  (use-package org-mime :straight t)

  ;; fonts
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Vollkorn"))))
   ;; '(variable-pitch ((t (:family "Fira Code"))))
   '(fixed-pitch ((t (:family "Fira Code" :slant normal :weight normal))))
   '(org-level-1 ((t (:inherit variable-pitch :height 1.5 :weight bold))))
   '(org-level-2 ((t (:inherit variable-pitch :height 1.4 :weight bold))))
   '(org-level-3 ((t (:inherit variable-pitch :height 1.3 :weight bold))))
   '(org-level-4 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
   '(org-level-5 ((t (:inherit variable-pitch :height 1.1 :weight bold))))
   '(org-level-6 ((t (:inherit variable-pitch :height 1.0 :weight bold))))
   '(org-level-7 ((t (:inherit variable-pitch :height 1.0 :weight bold))))
   '(org-level-8 ((t (:inherit variable-pitch :height 1.0 :weight bold))))
   '(org-document-title ((t (:inherit variable-pitch :height 2.0 :weight bold))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-todo ((t (:inherit fixed-pitch :underline t))))
   '(org-done ((t (:inherit fixed-pitch :underline t))))
   '(org-keywords ((t (:inherit fixed-pitch))))
   '(org-formula ((t (:inherit fixed-pitch :height 0.8))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:inherit variable-pitch :slant italic))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch) :weight bold))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:inherid fixed-pitch :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch) :weight bold))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   ;; '(org-table ((t (:inherit fixed-pitch))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.7))))
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

  (use-package ob-prolog :straight t)

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
                                                      (:kernel . "julia-1.8")
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
  (add-to-list 'org-preview-latex-process-alist
               '(xdvisvgm :programs
                          ("xelatex" "dvisvgm")
                          :description "dvi > svg"
                          :message "you need to install the programs: xelatex and dvisvgm."
                          :image-input-type "xdv"
                          :image-output-type "svg"
                          :image-size-adjust (1.7 . 1.5)
                          :latex-compiler ("xelatex -interaction=nonstopmode -output-directory %o -no-pdf %f")
                          :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")))
  (setq org-preview-latex-default-process 'xdvisvgm)

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
                                    (org-display-inline-images)
                                    (read-only-mode -1)))
           (org-tree-slide-stop . (lambda ()
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-presentation-profile)
    (setq org-tree-slide-skip-outline-level 5)
    (with-eval-after-load "org-tree-slide"
      (defvar my-hide-org-meta-line-p nil)
      (defun my-hide-org-meta-line ()
        (interactive)
        (setq my-hide-org-meta-line-p t)
        (set-face-attribute 'org-meta-line nil
                            :foreground (face-attribute 'default :background)))
      (defun my-show-org-meta-line ()
        (interactive)
        (setq my-hide-org-meta-line-p nil)
        (set-face-attribute 'org-meta-line nil :foreground nil))

      (defun my-toggle-org-meta-line ()
        (interactive)
        (if my-hide-org-meta-line-p
            (my-show-org-meta-line) (my-hide-org-meta-line)))

      (add-hook 'org-tree-slide-play-hook #'my-hide-org-meta-line)))

  ;; Visually summarize progress
  (use-package org-dashboard
    :straight t)

  (require 'org-tempo)

  ;; reveal
  (use-package ox-reveal
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

  (setq org-todo-keywords '((sequence "TODO(T)" "DOING(I)" "HANGUP(H)" "|" "DONE(D)" "CANCELED(C)")
                            (sequence "(t)" "金(i)" "(h)" "|" "(d)" "(c)"))
        org-log-done 'time
        org-startup-indented t
        org-startup-with-inline-images t
        org-ellipsis (if (char-displayable-p ?…) " …" nil)
        org-cycle-separator-lines -1
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-image-actual-width nil)

  (setq org-todo-keyword-faces
        '(("DOING"    . (:inherit org-todo))
          ("HANGUP"   . (:inherit org-todo))
          ("CANCELED" . (:inherit org-done))))

  ;; Super agenda
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
