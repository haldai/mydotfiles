;;---------------------
;; Customize AucTex
;;---------------------
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-correlate-mode t)

;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)

(if (string-equal system-type "windows-nt")
    (require 'tex-mik))

;; hook mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'default)       ; use xlatex default
                  ;TeX-show-compilation t) ; display compilation windows
            (TeX-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
	          (setq TeX-auto-save t)
	          (setq TeX-parse-self t)
	          (setq-default TeX-master nil)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))

(defadvice TeX-LaTeX-sentinel
    (around mg-TeX-LaTeX-sentinel-open-output activate)
  "Open output when there are errors."
  ;; Run `TeX-LaTeX-sentinel' as usual.
  ad-do-it
  ;; Check for the presence of errors.
  (when
      (with-current-buffer TeX-command-buffer
        (plist-get TeX-error-report-switches (intern (TeX-master-file))))
    ;; If there are errors, open the output buffer.
    (TeX-recenter-output-buffer nil)))

;; do not check comments
(setq ispell-list-command "--list")
(setq ispell-extra-args '("--dont-tex-check-comments"))

;; Reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; text viewer
(setq TeX-view-program-list
      '(("Acrobat" "Acrobat.exe %o")
        ("Gsview" "gsview32.exe %o")
        ("gv" "gv %o")
        ("qpdfview" "qpdfview --unique %o")
        ("MasterPDF" "masterpdfeditor4 %o")
        ("Evince" "evince %o")
        ("Firefox" "firefox %o")
        ("Yap" "yap.exe %o")))
(cond
 ((eq system-type 'windows-nt)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection '((output-pdf "Acrobat")
                                                 (output-dvi "Yap"))))))
 ((eq system-type 'gnu/linux)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection '((output-pdf "PDF Tools")
                                                 (output-dvi "gv")))))))
;;---------------------------
;; Markdownmode with mathjax
;;---------------------------
(setq markdown-command
      (concat
       "/usr/local/bin/pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments"))
;;------------------------------
;; Chinese fonts
;;------------------------------
(defvar emacs-english-font "Source Code Pro"
  "The font name of English.")

(defvar emacs-cjk-font "方正宋刻本秀楷"
  "The font name for CJK.")

(defvar emacs-font-size-pair '(15 . 18)
  "Default font size pair for (english . chinese)")

(defvar emacs-font-size-pair-list
  '(( 5 .  6) (10 . 12)
    (13 . 16) (15 . 18) (17 . 20)
    (19 . 22) (20 . 24) (21 . 26)
    (24 . 28) (26 . 32) (28 . 34)
    (30 . 36) (34 . 40) (36 . 44))
  "This list is used to store matching (englis . chinese) font-size.")

(defun font-exist-p (fontname)
  "Test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."

  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

;; Setup font size based on emacs-font-size-pair
(when (display-graphic-p)
  (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))

(defun emacs-step-font-size (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun restore-emacs-font-size ()
  "Restore emacs's font-size acording emacs-font-size-pair-list."
  (interactive)
  (setq emacs-font-size-pair '(15 . 18))
  (when emacs-font-size-pair
    (message "emacs font size set to %.1f" (car emacs-font-size-pair))
    (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)))

(defun increase-emacs-font-size ()
  "Decrease emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Increase emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(when (display-graphic-p)
  (global-set-key (kbd "C-x C-=") 'increase-emacs-font-size)
  (global-set-key (kbd "C-x C--") 'decrease-emacs-font-size)
  (global-set-key (kbd "C-x C-0") 'restore-emacs-font-size))

;;--------------------------
;; emacsclient kill server
;;--------------------------
(defun client-save-kill-emacs(&optional display)
  " This is a function that can bu used to shutdown save buffers and
shutdown the emacs daemon. It should be called using
emacsclient -e '(client-save-kill-emacs)'.  This function will
check to see if there are any modified buffers or active clients
or frame.  If so an x window will be opened and the user will
be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

                                        ; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
                                        (> (length (frame-list)) 1)
                                        ))

                                        ; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
        (message "Initializing x windows system.")
        (x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

                                        ; Save the current frame.
    (setq new-frame (selected-frame))

                                        ; When displaying the number of clients and frames:
                                        ; subtract 1 from the clients for this client.
                                        ; subtract 2 from the frames this frame (that we just created) and the default frame.
    (when ( or (not active-clients-or-frames)
               (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

                                        ; If the user quits during the save dialog then don't exit emacs.
                                        ; Still close the terminal though.
      (let((inhibit-quit t))
                                        ; Save buffers
        (with-local-quit
          (save-some-buffers))

        (if quit-flag
            (setq quit-flag nil)
                                        ; Kill all remaining clients
          (progn
            (dolist (client server-clients)
              (server-delete-client client))
                                        ; Exit emacs
            (kill-emacs)))
        ))

                                        ; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )

(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (buffer-modified-p buffer)
                 (not (buffer-base-buffer buffer))
                 (or
                  (buffer-file-name buffer)
                  (progn
                    (set-buffer buffer)
                    (and buffer-offer-save (> (buffer-size) 0))))
                 )
        (setq modified-found t)
        )
      )
    modified-found
    )
  )
