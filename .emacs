;---------------
;;benchmark init
;;---------------
;;(add-to-list 'load-path "/home/daiwz/.emacs.d/plugins/benchmark-init-el")
;;(require 'benchmark-init-loaddefs)
;;(benchmark-init/activate)

;;--------------------------------------
;; load path
;;--------------------------------------
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/elpa")
(set 'plugin-folder '"~/.emacs.d/plugins")

;;----------------------
;; use package
;;----------------------
(require 'package)
(package-initialize) ;; You might already have this line
(setq package-enable-at-startup nil)
(setq package-check-signature nil)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;------------------
;; language setting
;;------------------
;;(set-language-environment 'Chinese-GBK)
;;(set-keyboard-coding-system 'gbk)
;;(set-clipboard-coding-system 'gbk)
;;(set-terminal-coding-system 'gbk)
;;(set-buffer-file-coding-system 'gbk)
;;(set-selection-coding-system 'gbk)
;;(modify-coding-system-alist 'process "*" 'gbk)
;;(setq default-process-coding-system 
;;            '(gbk . gbk))
;;(setq-default pathname-coding-system 'gbk)
;; -----
;; ibus
;; -----
;;(add-to-list 'load-path "~/.emacs.d/plugins/ibus-el-0.3.2")
;;(require 'ibus)
;;(add-hook 'after-init-hook 'ibus-mode-on)
;; Use C-SPC for Set Mark command
;;(ibus-define-common-key ?\C-\s nil)
;; Use C-/ for Undo command
;;(ibus-define-common-key ?\C-/ nil)
;; Use S-SPC to toggle input status
;;(ibus-define-common-key ?\S-\s nil)
;;(global-set-key (kbd "S-SPC") 'ibus-toggle)
;; Change cursor color depending on IBus status
;;(setq ibus-cursor-color '("red" "blue" "limegreen"))

;;--------------------------------------------------
;; common config
;;--------------------------------------------------

;; unset ctrl-z
(global-unset-key (kbd "C-z"))

;; ansi color
(define-derived-mode fundamental-ansi-mode fundamental-mode "fundamental ansi"
  "Fundamental mode that understands ansi colors."
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq auto-mode-alist
      (cons '("\\.col\\'" . fundamental-ansi-mode) auto-mode-alist))

;; bash
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-ic")

;; goto line
(global-set-key (kbd "C-c SPC") 'goto-line)

;; emacs window-numbers
(autoload 'window-number-mode "window-number"
   "A global minor mode that enables selection of windows according to
 numbers with the C-x C-j prefix.  Another mode,
 `window-number-meta-mode' enables the use of the M- prefix."
   t)

 (autoload 'window-number-meta-mode "window-number"
   "A global minor mode that enables use of the M- prefix to select
 windows, use `window-number-mode' to display the window numbers in
 the mode-line."
   t)

(window-number-mode 1)
(window-number-meta-mode 1)

;; use tramp
(require 'tramp)
(auto-image-file-mode t)
(global-linum-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
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
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("XeLaTex" "xelatex%(mode) %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "XeLaTex for CJK languages"))))
 '(column-number-mode t)
 '(display-time-mode t)
 '(ediprolog-program "/home/daiwz/.local/bin/swipl")
 '(exec-path
   (quote
    ("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/games" "/usr/games" "/usr/local/libexec/emacs/25.1/x86_64-unknown-linux-gnu" "/home/daiwz/.local/bin")))
 '(font-use-system-font t)
 '(gdb-many-windows t)
 '(inhibit-startup-screen t)
 '(markdown-command "pandoc -f markdown -t html")
 '(matlab-shell-command "/home/daiwz/APPs/MATLAB/R2016b/bin/matlab")
 '(package-selected-packages
   (quote
    (realgud multiple-cursors mc-extras mc-jump ess julia-mode ac-js2 js2-mode ein elpy matlab-mode magit smex slime-clj slime scpaste pos-tip popwin paredit marmalade-demo marmalade json-mode idle-highlight-mode flyspell-correct find-file-in-project ediprolog company-auctex better-defaults auto-complete-auctex auctex-latexmk ac-python ac-octave ac-math ac-ispell ac-geiser)))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; display column number
(column-number-mode t)
;; display time
(display-time)

;; indent settings
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)
(setq-default default-tab-width 4)

;;--------------------
;; better-defaults
;;--------------------

(add-to-list 'load-path "~/.emacs.d/plugins/better-defaults")
(require 'better-defaults)

;;-------------------------
;; cedet config
;;-------------------------

;; load cedet
(setq byte-compile-warnings nil) ;; disable cedet warning
(load-file "~/.emacs.d/plugins/cedet/cedet-devel-load.el")

;; semantics
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode 0)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode 0)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;;(require 'semantic-gcc)
;;(require 'semantic-ia)
(require 'cedet)

(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
;;(semantic-load-enable-guady-code-helpers)
(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

;; set ede
(global-ede-mode t)
(semantic-mode 0)
  
;; bookmarks
;; (enable-visual-studio-bookmarks)

; code folding

;  (global-semantic-tag-folding-mode 1)
;  (global-set-key (kbd "C-?") 'global-semantic-tag-folding-mode)
;  (define-key semantic-tag-folding-mode-map (kbd "C-c -") 'semantic-tag-folding-fold-block)
;  (define-key semantic-tag-folding-mode-map (kbd "C-c =") 'semantic-tag-folding-show-block)
;  (define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-all)
;  (define-key semantic-tag-folding-mode-map (kbd "C-+") 'semantic-tag-folding-show-all)

; syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)	

; semanticdb file search
;; (setq semanticdb-project-roots (list (expand-file-name "/")))
(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public" "/home/daiwz/.local/lib/swipl/include" "/home/daiwz/.local/include"))
(defconst cedet-win32-include-dirs
  (list "C:/MinGW/include"
        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
(require 'semantic-c nil 'noerror)
(let ((include-dirs cedet-user-include-dirs))
  (when (eq system-type 'windows-nt)
    (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

; set semantic temp file path
(setq semanticdb-default-save-directory "~/.emacs.d/semantic")

(setq semanticdb-search-system-databases t)
(add-hook 'c-mode-common-hook
(lambda ()
  (setq semanticdb-project-system-databases
	(list (semanticdb-create-database
	       semanticdb-new-database-class
	       "/usr/include")))))

;;(enable-visual-studio-bookmarks)
(require 'eassist nil 'noerror)
(require 'semantic-tag-folding nil 'noerror)

; jumping
(global-set-key (kbd "C-<f12>") 'semantic-ia-fast-jump)

(global-set-key [s-f12]
		(lambda ()
                  (interactive)
                  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
                      (error "Semantic Bookmark ring is currently empty"))
                  (let* ((ring (oref semantic-mru-bookmark-ring ring))
                         (alist (semantic-mrub-ring-to-assoc-list ring))
                         (first (cdr (car alist))))
                    (if (semantic-equivalent-tag-p (oref first tag)
                                                   (semantic-current-tag))
                        (setq first (cdr (car (cdr alist)))))
                    (semantic-mrub-switch-tags first))))

;;---------------
;; ECB
;;---------------
;; activate ecb
;; (setq stack-trace-on-error t) ;; disable error message
;; (add-to-list 'load-path "~/.emacs.d/plugins/ecb/")
;; (require 'ecb)
;; (require 'ecb-autoloads)
;; 
;; ;; ecb layout style
;; (setq ecb-layout-name "left1")
;; (setq ecb-show-sources-in-directories-buffer 'always)
;; 
;; ;; compile window size
;; (setq ecb-compile-window-height 5)
;; 
;; ;; activate and deactivate ecb
;; (global-set-key (kbd "C-c ;") 'ecb-activate)
;; (global-set-key (kbd "C-c '") 'ecb-deactivate)
;; ;; show/hide ecb window
;; (global-set-key (kbd "C-c C-x ;") 'ecb-show-ecb-windows)
;; (global-set-key (kbd "C-c C-x '") 'ecb-hide-ecb-windows)
;; ;; quick navigation between ecb windows
;; (global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
;; (global-set-key (kbd "C-!") 'ecb-goto-window-directories)
;; (global-set-key (kbd "C-@") 'ecb-goto-window-sources)
;; (global-set-key (kbd "C-#") 'ecb-goto-window-methods)
;; (global-set-key (kbd "C-$") 'ecb-goto-window-compilation)

;;---------------
;; GDB
;;---------------

;; gud-buttons
(add-hook 'gdb-mode-hook '(lambda ()
                            (define-key c-mode-base-map [(f5)] 'gud-go)
                            (define-key c-mode-base-map [(f7)] 'gud-step)
                            (define-key c-mode-base-map [(f8)] 'gud-next)))
;; gdb gui
(defadvice gdb-setup-windows (after my-setup-gdb-windows activate)
  "my gdb UI"
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let ((win0 (selected-window))
        (win1 (split-window nil nil 'left))      ;code and output
        (win2 (split-window-below (/ (* (window-height) 2) 3)))     ;stack
        )
    (select-window win2)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    (select-window win1)
    (set-window-buffer
     win1
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    (let ((win3 (split-window nil (/ (* (window-height) 3) 4)))) ;io
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3))
    (select-window win0)
    ))

;;-------------------
;; cc-mode config
;;-------------------

(global-set-key [(f5)] 'speedbar)
(add-to-list 'load-path "~/.emacs.d/plugins/cc-mode")
(require 'cc-mode)
;;(c-set-offset 'inline-open 0)
;;(c-set-offset 'friend '-)
;;(c-set-offset 'substatement-open 0)

;; default indent
(setq-default c-basic-offset 4)
(setq-default c-indent-tabs-mode t
			  c-indent-level 4
			  c-argdecl-indent 0)

;; newline and indent
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; no indent in namespace
(c-add-style "my-cc-style"
  '("linux"
	(c-offsets-alist
	 (innamespace . -)
     (substatement . 4)     
     (topmost-intro . -)
	 (inline-open . 0)
	 (block-open . +)
     (statement-block-intro . 4)
	 (defun-block-intro . 4)
     (brace-list-intro . +)
	 (substatement-open . 0)
	 (brace-list-open . +)
	 (arglist-intro . +))))

(setq c-default-style "my-cc-style")

;; other indents
;;(defun my-c-mode-common-hook ()  
;;  (setq default-tab-width 4)
;;  (setq tab-width 4)
;;  (setq c-basic-offset 4)
;;  (hs-minor-mode t))


;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)  
(load-library "hideshow")  
(add-hook 'c-mode-hook 'hs-minor-mode)  
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)  
(add-hook 'perl-mode-hook 'hs-minor-mode)  
(add-hook 'php-mode-hook 'hs-minor-mode)  
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)  
;
;; M-x hs-minor-mode  
;; C-c @ ESC C-s show all  
;; C-c @ ESC C-h hide all  
;; C-c @ C-s show block  
;; C-c @ C-h hide block  
;; C-c @ C-c toggle hide/show  

;; C langrage setting
(defun my-c-mode-hook ()  
  (c-set-style "my-cc-style")
;;  (c-comment-only-line-offset . 4)
  (setq hs-minor-mode t))
(add-hook 'c-mode-hook 'my-c-mode-hook)  

;; C++ language setting
(defun my-c++-mode-hook ()
;;  (c-tab-always-indent . t)
;;  (c-comment-only-line-offset . 4)
  (setq hs-minor-mode t))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook) 

;;------------------
;; xcscope
;;-----------------
(require 'xcscope)

;;----------------------
;; Python config
;;----------------------
(require 'elpy)
(elpy-enable)
(require 'ein)

;;-----------------------
;; Latex config
;;-----------------------

;; AucTex
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
 	
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(if (string-equal system-type "windows-nt")
    (require 'tex-mik))
;; Setting AucTex
(mapc (lambda (mode)
      (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'linum-mode
            'flyspell-mode))

; hook mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'default       ; use xlatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq-default TeX-master nil)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
	    ))

; text viewer
(setq TeX-view-program-list
      '(("Acrobat" "Acrobat.exe %o")
        ("Gsview" "gsview32.exe %o")
        ("Okular" "okular --unique %o")
        ("Evince" "evince %o")
        ("Firefox" "firefox %o")
	("Yap" "yap %o")))
(cond
 ((eq system-type 'windows-nt)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection '((output-pdf "Acrobat")
                                                 (output-dvi "Yap"))))))
 
 ((eq system-type 'gnu/linux)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection '((output-pdf "Okular")
                                                 (output-dvi "Okular")))))))

;; mpview
(defun mpost-compile ()
  "Compile current file with MetaPost."
  (interactive)
  (save-buffer)
  (compile (concat "mpost " (file-name-nondirectory buffer-file-name))))

(add-hook 'metapost-mode-hook
          '(lambda nil
             (define-key meta-mode-map "\C-c\C-c" 'mpost-compile))) 

;; Aspell
;; (setq-default ispell-program-name "aspell")
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--dont-tex-check-comments"))

;; easy spell check
(global-set-key (kbd "<f8>") 'ispell-word)
;;(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-c <f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-next-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)



;; Reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;;------------
;; yasnippet
;;------------
(require 'yasnippet)
(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
(defalias 'yas/table-hash 'yas--table-hash)
(yas/global-mode 1)

;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

;; (setq yas/trigger-key "C-c TAB")

;;-----------
;; popup
;;-----------
;;(add-to-list 'load-path "~/.emacs.d/elpa/popup-0.5/")
(require 'popup)

;;----------
;; pos-tip
;;----------
;;(add-to-list 'load-path "~/.emacs.d/elpa/pos-tip-0.4.5")
(require 'pos-tip)

;;----------------------------
;; auto complete
;;----------------------------
;;(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-1.4")

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20160710.1544")
(setq ac-fuzzy-enable nil) ;; fuzzy matching

;; default configuration
(ac-config-default)
(setq ac-auto-start nil) ;; use trigger key to start ac
(setq ac-quick-help-delay 0.5)
(define-key ac-mode-map  [(control return)] 'auto-complete)
(ac-set-trigger-key "<tab>")
(ac-set-trigger-key "TAB")
(global-auto-complete-mode t)

;; clang completion
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete-clang/")
(require 'auto-complete-clang)

;; (setq ac-auto-start t)

;; clang look-up places
(setq ac-clang-flags
      (mapcar (lambda (item)(concat "-I" item))
              (split-string
               "
 /usr/include/c++/5
 /usr/include/x86_64-linux-gnu/c++/5
 /usr/include/c++/5/backward
 /usr/lib/gcc/x86_64-linux-gnu/5/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/5/include-fixed
 /usr/include/c++/6
 /usr/include/x86_64-linux-gnu/c++/6
 /usr/include/c++/6/backward
 /usr/lib/gcc/x86_64-linux-gnu/6/include
 /usr/lib/gcc/x86_64-linux-gnu/6/include-fixed
 /usr/include
 /home/daiwz/.local/include
 /home/daiwz/.local/lib/swipl/include
"
               )))

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'c++-mode-hook 'ac-cc-mode-setup) 
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

;;-------------
;; ac-clang
;;-------------
;;(add-to-list 'load-path "~/.emacs.d/elpa/ac-clang-20150906.1008/")
;;(require 'ac-clang)

;;(ac-clang-initialize)

;;(setq ac-clang-flags
;;      (mapcar (lambda (item)(concat "-I" item))
;;              (split-string
;;               "
;; /usr/include/c++/5
;; /usr/include/x86_64-linux-gnu/c++/5
;; /usr/include/c++/5/backward
;; /usr/lib/gcc/x86_64-linux-gnu/5/include
;; /usr/local/include
;; /usr/lib/gcc/x86_64-linux-gnu/5/include-fixed
;; /usr/include/x86_64-linux-gnu
;; /usr/include
;; /home
;;"
;;               )))

;;(defun my-ac-config ()
;;  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;  (global-auto-complete-mode t)
;;  (ac-clang-activate))
;;
;;(my-ac-config)

;;-------------
;; color-theme
;;-------------
(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)

;;--------------
;; prolog Mode.
;;--------------
(require 'ediprolog)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))
(add-hook 'prolog-mode-hook '(lambda ()
                               (local-set-key (kbd "RET") 'newline-and-indent)
                               (auto-complete-mode t)
                               (local-set-key [f10] 'ediprolog-dwim)
                               (local-set-key (kbd "C-c <f10>")
                                              'ediprolog-consult)
                               (local-set-key (kbd "C-c q") 'insert-query-mark)
                               ))

(defun insert-query-mark ()
  "Insert an epiprolog query mark in prolog mode."
  (interactive)
  (insert "%?- "))

;;--------------
;; markdown Mode.
;;---------------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq markdown-enable-math t)

;;-------------
;; scala mode.
;;-------------
;;(add-to-list 'load-path "~/.emacs.d/plugins/scala-mode2/")
;;(require 'scala-mode2)
;;(add-hook 'scala-mode-hook '(lambda ()
  ;; Bind the 'newline-and-indent' command to RET (aka 'enter'). This
  ;; is normally also available as C-j. The 'newline-and-indent'
  ;; command has the following functionality: 1) it removes trailing
  ;; whitespace from the current line, 2) it create a new line, and 3)
  ;; indents it.  An alternative is the
  ;; 'reindent-then-newline-and-indent' command.
;;  (local-set-key (kbd "RET") 'newline-and-indent)
      
  ;; Alternatively, bind the 'newline-and-indent' command and
  ;; 'scala-indent:insert-asterisk-on-multiline-comment' to RET in
  ;; order to get indentation and asterisk-insertion within multi-line
  ;; comments.
  ;; (local-set-key (kbd "RET") '(lambda ()
  ;;   (interactive)
  ;;   (newline-and-indent)
  ;;   (scala-indent:insert-asterisk-on-multiline-comment)))
	  
  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a
  ;; code line as a new statement.
;;  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  ;; and other bindings here
  ;; (require 'whitespace)
  
  ;; clean-up whitespace at save
  ;; (make-local-variable 'before-save-hook)
  ;; (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  ;; (whitespace-mode)
;; ))

;;------------
;; sdcv-mode
;;------------
(require 'sdcv-mode) 
(global-set-key (kbd "C-c d") 'sdcv-search)

;;-----------
;; multiple-cursors
;;-----------
;;(add-to-list 'load-path "~/.emacs.d/elpa/multiple-cursors-1.3.0/")
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;--------------
;; gnugo
;;--------------
;; (require 'gnugo)

;;-----------------------
;; some useful libraries
;;-----------------------
(require 'magit)
(require 'smex)
(require 'paredit)
(require 'idle-highlight-mode)
(require 'find-file-in-project)
(require 'scpaste)

;;--------
;; json
;;--------
(require 'json-mode)
(setq auto-mode-alist (cons '("\\.json$" . json-mode) auto-mode-alist))

;;---------
;; scheme
;;---------
(require 'geiser)

;;-----------
;; matlab
;;-----------
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)

;;--------
;; ccrypt
;;--------
(require 'ps-ccrypt)

;;-------------
;; nxml-mode
;;-------------
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode))
;;(add-to-list 'auto-mode-alist '("\\.svg$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss$" . nxml-mode))

(unify-8859-on-decoding-mode)

(setq magic-mode-alist
	  (cons '("<＼＼?xml " . nxml-mode)
            magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)

;;------------
;; julia mode
;;------------
(require 'julia-mode)
(require 'ess-site)
