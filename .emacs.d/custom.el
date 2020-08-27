(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
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
      :help "XeLaTex for CJK languages")))
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server nil)
 '(TeX-view-program-list
   '(("Acrobat" "Acrobat.exe %o")
     ("Gsview" "gsview32.exe %o")
     ("gv" "gv %o")
     ("qpdfview" "qpdfview --unique %o")
     ("MasterPDF" "masterpdfeditor4 %o")
     ("Evince" "evince %o")
     ("Firefox" "firefox %o")
     ("Yap" "yap.exe %o")))
 '(TeX-view-program-selection '((output-pdf "PDF Tools") (output-dvi "gv")))
 '(ediprolog-program "swipl")
 '(ediprolog-system 'swi)
 '(enable-recursive-minibuffers t)
 '(epa-pinentry-mode 'loopback)
 '(global-auto-revert-non-file-buffers t)
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-save-directory-list-file "/home/daiwz/.emacs.d/.cache/ido.last")
 '(ido-vertical-mode t)
 '(ivy-use-virtual-buffers t)
 '(markdown-coding-system 'utf-8)
 '(markdown-command "pandoc -f markdown -t html")
 '(prolog-electric-if-then-else-flag t)
 '(prolog-paren-indent-p t)
 '(prolog-program-name
   '(((getenv "EPROLOG")
      (eval
       (getenv "EPROLOG")))
     (eclipse "eclipse")
     (mercury nil)
     (sicstus "sicstus")
     (swi "pl")
     (gnu "gprolog")
     (t "prolog")))
 '(prolog-program-switches '((swi ("-G128M" "-T128M" "-L128M" "-O")) (t nil)))
 '(pyim-dicts
   '((:name "bigdict" :file "/home/daiwz/.emacs.d/pyim-bigdict.pyim.gz"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-dir-face ((t (:foreground nil))))
 '(all-the-icons-dsilver ((((background dark)) :foreground "#838484") (((background light)) :foreground "#838484")))
 '(all-the-icons-lsilver ((((background dark)) :foreground "#B9B6AA") (((background light)) :foreground "#7F7869")))
 '(all-the-icons-silver ((((background dark)) :foreground "#716E68") (((background light)) :foreground "#716E68")))
 '(diff-hl-change ((t (:background "#46D9FF"))))
 '(diff-hl-delete ((t (:background "#ff6c6b"))))
 '(diff-hl-insert ((t (:background "#98be65"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(hl-todo ((t (:box t :inherit))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
