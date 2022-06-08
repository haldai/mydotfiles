;;; Commentary:
;;
;; EMMS configuration.
;;

;;; Code:

;; Emacs Multi-Media System
(use-package emms
  :straight t
  :config
  ;;** EMMS
  ;; Autoload the id3-browser and bind it to F7.
  ;; You can change this to your favorite EMMS interface.
  (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
  (global-set-key [(f7)] 'emms-smart-browse)

  (with-eval-after-load 'emms
    (emms-devel) ;; or (emms-devel) if you want all features
    (setq emms-source-file-default-directory "~/Music"
          emms-info-asynchronously t
          emms-show-format "♪ %s")

    ;; Might want to check `emms-info-functions',
    ;; `emms-info-libtag-program-name',
    ;; `emms-source-file-directory-tree-function'
    ;; as well.
    (setq emms-info-functions '(emms-info-exiftool)) ;; using Exiftool
    (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

    ;; Determine which player to use.
    ;; If you don't have strong preferences or don't have
    ;; exotic files from the past (wma) `emms-default-players`
    ;; is probably all you need.
    (if (executable-find "mplayer")
        (setq emms-player-list '(emms-player-mplayer))
      (emms-default-players))
    (require 'emms-player-mpd)
    (setq emms-player-mpd-server-name "127.0.0.1")
    (setq emms-player-mpd-server-port "6600")
    (setq emms-player-mpd-music-directory "~/Music")
    (add-to-list 'emms-info-functions 'emms-info-mpd)
    (add-to-list 'emms-player-list 'emms-player-mpd)

    (require 'emms-cue)
    (add-to-list 'emms-info-functions 'emms-info-cueinfo)
    )

  (use-package ivy-emms :straight t))

(provide 'init-emms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-emms.el ends here
