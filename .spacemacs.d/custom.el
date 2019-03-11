(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     (#("LaTeX" 0 1
        (idx 9))
      "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     (#("XeTeX" 0 1
        (idx 18))
      "xetex -synctex=1 %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run an xetex")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     (#("BibTeX" 0 1
        (idx 0))
      "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     (#("Biber" 0 1
        (idx 1))
      "biber %s" TeX-run-Biber nil t :help "Run Biber")
     (#("View" 0 1
        (idx 16))
      "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     (#("Print" 0 1
        (idx 12))
      "%p" TeX-run-command t t :help "Print the file")
     (#("Queue" 0 1
        (idx 14))
      "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     (#("File" 0 1
        (idx 7))
      "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     (#("Dvips" 0 1
        (idx 6))
      "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     (#("Ps2pdf" 0 1
        (idx 13))
      "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     (#("Index" 0 1
        (idx 8))
      "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     (#("Xindy" 0 1
        (idx 19))
      "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     (#("Check" 0 1
        (idx 2))
      "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     (#("ChkTeX" 0 1
        (idx 3))
      "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     (#("Spell" 0 1
        (idx 15))
      "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     (#("Clean" 0 1
        (idx 4))
      "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     (#("Clean All" 0 1
        (idx 5))
      "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     (#("Other" 0 1
        (idx 11))
      "" TeX-run-command t t :help "Run an arbitrary command")
     (#("XeLaTex" 0 1
        (idx 17))
      "xelatex%(mode) -synctex=1 %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "XeLaTex for CJK languages"))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server nil)
 '(TeX-view-program-list
   (quote
    (("Acrobat" "Acrobat.exe %o")
     ("Gsview" "gsview32.exe %o")
     ("gv" "gv %o")
     ("qpdfview" "qpdfview --unique %o")
     ("MasterPDF" "masterpdfeditor4 %o")
     ("Evince" "evince %o")
     ("Firefox" "firefox %o")
     ("Yap" "yap.exe %o"))))
 '(TeX-view-program-selection (quote ((output-pdf "PDF Tools") (output-dvi "gv"))))
 '(ediprolog-program "swipl")
 '(enable-recursive-minibuffers t)
 '(epa-pinentry-mode (quote loopback))
 '(global-auto-revert-non-file-buffers t)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-save-directory-list-file "/home/daiwz/.emacs.d/.cache/ido.last")
 '(ido-vertical-mode t)
 '(initial-buffer-choice (lambda nil (get-buffer spacemacs-buffer-name)))
 '(ivy-use-virtual-buffers t)
 '(markdown-coding-system (quote utf-8))
 '(markdown-command "pandoc -f markdown -t html" t)
 '(org-agenda-files (quote ("~/Documents/2018_fall/seminar/SAT_tree.org")))
 '(package-selected-packages
   (quote
    (doom-modeline transient ox-reveal insert-shebang fish-mode company-shell zen-and-art-theme yapfify ws-butler winum white-sand-theme which-key wgrep web-mode web-beautify volatile-highlights use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spaceline powerline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smex smeargle smartparens slim-mode seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme popwin planet-theme pip-requirements pinentry phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el paradox spinner orgit organic-green-theme org-ref pdf-tools key-chord helm-bibtex biblio parsebib biblio-core tablist org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-prolog noctilux-theme neotree naquadah-theme mwim mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme markdown-preview-mode uuidgen web-server markdown-mode majapahit-theme magit-gitflow magit magit-popup ghub treepy graphql madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme json-mode json-snatcher json-reformat js2-refactor multiple-cursors js-doc jbeans-theme jazz-theme ivy-hydra ir-black-theme inkpot-theme indent-guide hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme helm-make helm helm-core hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-commit with-editor geiser gandalf-theme fuzzy flyspell-correct-ivy flyspell-correct flycheck-pos-tip pos-tip flycheck flx-ido flx flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-search-highlight-persist evil-ediff evil goto-chg undo-tree eval-sexp-fu highlight ess julia-mode espresso-theme emmet-mode elisp-slime-nav ein skewer-mode request-deferred websocket request deferred js2-mode simple-httpd ediprolog dumb-jump dracula-theme django-theme disaster diminish define-word darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode counsel-projectile projectile pkg-info epl counsel swiper ivy company-web web-completion-data company-tern dash-functional tern company-statistics company-c-headers company-auctex company-anaconda company column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized coffee-mode cmake-mode clues-theme clean-aindent-mode clang-format cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme bind-map bind-key badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex-latexmk auctex async apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f dash s ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link avy ac-ispell auto-complete popup zenburn-theme)))
 '(pdf-view-incompatible-modes
   (quote
    (linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode)))
 '(persp-set-ido-hooks t)
 '(safe-local-variable-values (quote ((eval progn (pp-buffer) (indent-buffer)))))
 '(uniquify-ignore-buffers-re "^\\*")
 '(winum-auto-assign-0-to-minibuffer nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
