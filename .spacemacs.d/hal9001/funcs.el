;;----------------------
;; highlight keywords
;;----------------------
(defun my-highlight ()
  "highlight some words."
  (interactive)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|QUESTION\\|NOTE\\)"
                             1 font-lock-warning-face t))))

;;-----------------------
;; query mark for Prolog
;;-----------------------
(defun insert-prolog-query-mark ()
  "Insert an epiprolog query mark in prolog mode."
  (interactive)
  (insert "%?- "))
