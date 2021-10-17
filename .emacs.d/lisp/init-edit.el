;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
(setq auto-window-vscroll nil)
;; (setq-default kill-whole-line t)           ; Kill line including '\n'
(setq global-visual-line-mode t)

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Delete selection if you insert
(use-package delsel
  :straight t
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :straight t
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :straight t
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :straight t
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :straight t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :straight t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Quickly follow links
(use-package ace-link
  :straight t
  :bind (("M-o" . ace-link-addr))
  :hook (after-init . ace-link-setup-default))

;; Jump to Chinese characters
(use-package ace-pinyin
  :straight t
  :diminish
  :hook (after-init . ace-pinyin-global-mode))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :straight t
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode go-mode
                           c-mode c++-mode csharp-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Show number of matches in mode-line while searching
(use-package anzu
  :straight t
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :straight t
  :bind ([remap comment-dwim] . comment-dwim-2)) ;

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :straight t
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :straight t
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :straight t
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

;; Multiple cursors
(use-package multiple-cursors
  :straight t
  :init (setq mc/always-run-for-all t)
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :straight t
  :hook (after-init . smart-region-on))

;; On-the-fly spell checker
(use-package flyspell
  :straight t
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB" " --dont-tex-check-comments" "--run-together")))

;; Hungry deletion
(use-package hungry-delete
  :straight t
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Make bindings that stick around
(straight-use-package 'hydra)

;; Framework for mode-specific buffer indexes
(use-package imenu
  :straight t
  :bind (("C-." . imenu)))

;; Move to the beginning/end of line or code
(use-package mwim
  :straight t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; Treat undo history as a tree
(use-package undo-tree
  :straight t
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil
              undo-tree-history-directory-alist
              `(("." . ,(concat user-emacs-directory "undo-tree-hist/"))))
  :config
  ;; FIXME:  `undo-tree-visualizer-diff' is a local variable in *undo-tree* buffer.
  (defun undo-tree-visualizer-show-diff (&optional node)
    ;; show visualizer diff display
    (setq-local undo-tree-visualizer-diff t)
    (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
                  (undo-tree-diff node)))
          (display-buffer-mark-dedicated 'soft)
          win)
      (setq win (split-window))
      (set-window-buffer win buff)
      (shrink-window-if-larger-than-buffer win)))

  (defun undo-tree-visualizer-hide-diff ()
    ;; hide visualizer diff display
    (setq-local undo-tree-visualizer-diff nil)
    (let ((win (get-buffer-window undo-tree-diff-buffer-name)))
      (when win (with-selected-window win (kill-buffer-and-window))))))

;; Goto last change
(use-package goto-chg
  :straight t
  :bind ("C-," . goto-last-change))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :straight t
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Hideshow
(use-package hideshow
  :straight t
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding)))

;; Flexible text folding
(use-package origami
  :straight t
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :config
  (defhydra origami-hydra (:color blue :hint none)
    "
      _:_: recursively toggle node       _a_: toggle all nodes    _t_: toggle node
      _o_: show only current node        _u_: undo                _r_: redo
      _R_: reset
      "
    (":" origami-recursively-toggle-node)
    ("a" origami-toggle-all-nodes)
    ("t" origami-toggle-node)
    ("o" origami-show-only-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("R" origami-reset))

  :bind (:map origami-mode-map
              ("C-<tab>" . origami-hydra/body))
  :config
  (face-spec-reset-face 'origami-fold-header-face))

;; Narrow/Widen
(use-package fancy-narrow
  :straight t
  :diminish
  :hook (after-init . fancy-narrow-mode))

;; Huge files
(use-package vlf
  :straight t)
(require 'vlf)
(require 'tramp)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

;; Input Method
(use-package posframe
  :straight t)

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :bind (:map rime-active-mode-map
              ("<tab>" . 'rime-inline-ascii)
              :map rime-mode-map
              ("C-`"  . 'rime-send-keybinding)
              ("M-j" . 'rime-force-enable))
  :custom
  (default-input-method "rime")
  (mode-line-mule-info '((:eval (rime-lighter))))
  ;; display in modeline
  (rime-show-candidate 'posframe)
  (rime-posframe-style 'vertical)
  (liberime-select-schema "luna_pinyin_simp")
  (rime-disable-predicates
   '(rime-predicate-after-ascii-char-p
     rime-predicate-prog-in-code-p
     rime-predicate-in-code-string-p
     my/rime-predicate-zero-length-space-after-cc-p
     rime-predicate-space-after-cc-p
     rime-predicate-current-uppercase-letter-p
     my/rime-predicate-punctuation-next-char-is-paired-p
     rime-predicate-tex-math-or-command-p))
  :init
  (defun my/rime-predicate-punctuation-next-char-is-paired-p ()
    (if (not (eq (point) (point-max)))
        (and (rime-predicate-current-input-punctuation-p)
             (not (string-match-p
                 (rx (any "\"\(\[\{"))
                 (buffer-substring (point) (1- (point)))))
             (string-match-p
              (rx (any "\}\]\)\""))
              (buffer-substring (point) (1+ (point)))))
      nil))
  (defun my/rime-predicate-zero-length-space-after-cc-p ()
    "If cursor is after a whitespace which follow a non-ascii character."
    (and (> (point) (save-excursion (back-to-indentation) (point)))
         (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
           (string-match-p "\\cc\u200B+$" string))))
  :config
  ;; Any single character that not trigger auto commit
  (setq rime-inline-ascii-holder ?x)
  ;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l))


(defun insert-zero-width-space ()
  (interactive (insert "\u200B")))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
