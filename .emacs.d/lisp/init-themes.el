;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; pixel-scroll-precision-mode
(setq pixel-scroll-precision-mode t)

;; color theme
(straight-use-package 'zenburn-theme)
(load-theme 'zenburn t)
;; (use-package vscode-dark-plus-theme
;;   :straight t
;;   :config
;;   (load-theme 'vscode-dark-plus t))

(use-package cnfonts
  :straight (cnfonts :type git :host github :repo "tumashu/cnfonts")
  ;; :after all-the-icons
  ;; :hook (cnfonts-set-font-finish
  ;;        . (lambda (fontsizes-list)
  ;;            (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  ;;            (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  ;;            (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  ;;            (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  ;;            (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  ;;            (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)))
  :custom
  (cnfonts-personal-fontnames '(("Iosevka Nerd Font" "等距更纱黑体 SC" "Noto Sans Mono CJK SC")
                                ("方正屏显雅宋_GBK" "方正宋刻本秀楷" "Yahei Mono" "SauceCodePro Nerd Font Mono" "Noto Sans Mono CJK SC")
                                ("HanaMinB" "Noto Sans CJK SC" "Noto Sans Mono CJK SC")
                                ("Symbols Nerd Font Mono" "Noto Sans Symbols" "SauceCodePro Nerd Font Mono")
                                ("Symbols Nerd Font Mono" "Noto Sans Symbols" "SauceCodePro Nerd Font Mono")))
  :config
  ;; cnfonts 1.1.3 prepends its Ext-B font to the whole default fontset.
  ;; On Emacs 31 this can select HanaMinB for spaces and ordinary glyphs,
  ;; producing hexadecimal missing-glyph boxes.  Restrict that one call to
  ;; the supplementary CJK ideograph range it is intended to cover.
  (when (>= emacs-major-version 31)
    (defvar my-cnfonts--setting-fonts nil)
    (defun my-cnfonts--limit-extb-to-cjk (args)
      "Restrict cnfonts Ext-B fallback in set-fontset-font ARGS."
      (if (and my-cnfonts--setting-fonts
               (null (nth 1 args))
               (eq (nth 4 args) 'prepend))
          (let ((safe-args (copy-sequence args)))
            (setf (nth 1 safe-args) '(#x20000 . #x323AF))
            safe-args)
        args))
    (defun my-cnfonts--set-font-1-with-cjk-extb-range (original fontsizes)
      "Call ORIGINAL with FONTSIZES while limiting the Ext-B fallback."
      (let ((my-cnfonts--setting-fonts t))
        (funcall original fontsizes)))
    (unless (advice-member-p #'my-cnfonts--limit-extb-to-cjk 'set-fontset-font)
      (advice-add 'set-fontset-font :filter-args
                  #'my-cnfonts--limit-extb-to-cjk))
    (unless (advice-member-p #'my-cnfonts--set-font-1-with-cjk-extb-range
                             'cnfonts--set-font-1)
      (advice-add 'cnfonts--set-font-1 :around
                  #'my-cnfonts--set-font-1-with-cjk-extb-range)))
  (cnfonts-mode 1)
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize))

(use-package mixed-pitch
  :straight t
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode)
  :config
  ;; Emacs 31 leaves the foundry unspecified when mixed-pitch copies only
  ;; the variable-pitch family.  With Vollkorn that can leave the remapped
  ;; default face without any usable glyphs.  Select the current TrueType
  ;; build explicitly; keep mixed-pitch's fixed-face cookies intact.
  (defun my-mixed-pitch-use-vollkorn-ttf ()
    "Make mixed-pitch use the current TrueType Vollkorn build."
    (when (and mixed-pitch-mode
               (equal (face-attribute mixed-pitch-face :family nil t)
                      "Vollkorn"))
      (when mixed-pitch-variable-cookie
        (face-remap-remove-relative mixed-pitch-variable-cookie))
      (setq mixed-pitch-variable-cookie
            (face-remap-add-relative
             'default
             :family "Vollkorn"
             :foundry "NONE"
             :weight 'normal))))
  (add-hook 'mixed-pitch-mode-hook #'my-mixed-pitch-use-vollkorn-ttf))

(defun mode-line-height ()
  "Get current height of mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

(use-package hide-mode-line
  :straight t
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))

;; Icons
;; NOTE: Must run `M-x nerd-icons-install-fonts' manually on Windows
(use-package nerd-icons
  :straight t
  :if (display-graphic-p))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :straight t
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :straight t
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :config
    (setq linum-format "%4d ")

    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :hook (global-linum-mode . hlinum-activate)
      :custom-face (linum-highlight-face
                    ((t `(
                          :inherit default
                          :background unspecified
                          :foreground unspecified
                          ))))
      :init
      (setq linum-highlight-in-all-buffersp t))))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Suppress GUI features
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Misc
(setq use-short-answers t)
(setq visible-bell t)
(size-indication-mode 1)
;; (blink-cursor-mode -1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; transparent
(set-frame-parameter (selected-frame) 'alpha '(100 . 95))
(add-to-list 'default-frame-alist '(alpha . (100 . 95)))

;; keycast
(use-package keycast
  :straight t
  :config
  (keycast-mode-line-mode 1))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

(use-package page-break-lines
  :straight t
  :hook (after-init . global-page-break-lines-mode))

(use-package ligature
  :straight t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable Fira Code ligatures in text modes
  (ligature-set-ligatures 'text-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "->" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://" "_|_" "/\\" "\\/" "\\n"
                                       "ff" "fi" "ffi" "Fl" "Tl" "Il" "fj"))
  (ligature-set-ligatures 'prog-mode '("\\*" "*/" "!=" "==" "%%" ":=" ";;" "&&" "||" ">=" "<="
                                       "</>" ">>" "<<" ">>>" "<<<" "0xF" "//" "\\\\" "**" "#!"
                                       "#=" "##" "###" "####" "!!" "..." "++"))
  ;; Emacs 31.1 crashes in the regexp matcher during redisplay when cnfonts
  ;; changes the active fontset while global ligature composition is enabled.
  ;; Prefer the configured fonts; retain ligatures on older Emacs versions.
  (if (>= emacs-major-version 31)
      (progn
        (global-ligature-mode -1)
        (message "global-ligature-mode disabled: incompatible with cnfonts on Emacs %s"
                 emacs-version))
    (global-ligature-mode 1)))

(provide 'init-themes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-themes.el ends here
