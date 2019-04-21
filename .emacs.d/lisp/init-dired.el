;;; Commentary:
;;
;; Directory configurations.
;;

;;; Code:

;; Directory operations
(use-package dired
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
    (setq insert-directory-program "gls"))

  (when (executable-find "ls")
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)

    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind (:map dired-mode-map
                  ("S" . hydra-dired-quick-sort/body))))

  ;; Colourful dired
  (use-package diredfl
    :straight t
    :init (diredfl-global-mode 1))

  ;; Shows icons
  (use-package all-the-icons-dired
    :straight t
    :diminish
    :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
        (setq-local all-the-icons-dired-displayed t)
        (let ((inhibit-read-only t)
              (remote-p (and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))))
          (save-excursion
            (setq tab-width 1)
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename nil)
                (let ((file (dired-get-filename 'verbatim t)))
                  (unless (member file '("." ".."))
                    (let ((filename (dired-get-filename nil t)))
                      (if (file-directory-p filename)
                          (let ((icon (cond
                                       (remote-p
                                        (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust all-the-icons-dired-v-adjust))
                                       ((file-symlink-p filename)
                                        (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust all-the-icons-dired-v-adjust))
                                       ((all-the-icons-dir-is-submodule filename)
                                        (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust all-the-icons-dired-v-adjust))
                                       ((file-exists-p (format "%s/.git" filename))
                                        (all-the-icons-octicon "repo" :height 1.1 :v-adjust all-the-icons-dired-v-adjust ))
                                       (t (let ((matcher (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
                                            (apply (car matcher) (list (cadr matcher) :face 'all-the-icons-dired-dir-face :v-adjust all-the-icons-dired-v-adjust)))))))
                            (insert (concat "\t" icon "\t")))
                        (insert (concat "\t" (all-the-icons-icon-for-file file :v-adjust -0.05) "\t")))))))
              (forward-line 1))))))
    (advice-add #'all-the-icons-dired--display :override #'my-all-the-icons-dired--display))

  ;; Extra Dired functionality
  (use-package dired-aux)
  (use-package dired-x
    :demand
    :config
    (let ((cmd "xdg-open"))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
