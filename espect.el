;;; espect.el --- aspect-oriented buffer settings

;;; Commentary:

;; This mode makes it easy to configure settings for individual
;; buffers with a concice and extensible mini-language.  It abstracts
;; away common configuration selection tasks, like checking the mode
;; or filename, into a simple declarative syntax.  Declare conditions;
;; run a function when the new buffer matches them.  This makes it
;; easy to do things like turn on flyspell-prog-mode for your favorite
;; programming languages, or make all text-mode buffers ending in .mkn
;; have special properties.
;;
;; Additionally, the mini-language is extensible, so you can define
;; and use your own tests.  espect's "built-in" rules use the
;; extension mechanism, so your rules need not be second-class
;; citizens.

;;; Example configuration:

;; (setq espect-buffer-settings
;;       '((((:not ((:mode "^gnus") (:mode w3m-mode) (:mode "^erc")
;;                  (:mode eshell-mode) (:mode term-mode) (:mode gud-mode))))
;;          (lambda ()
;;            (highlight-beyond-fill-column)))
;;         ((:not ((:mode eshell-mode) (:mode term-mode)))
;;          (lambda () (hl-line-mode t)))
;;         (((:mode message-mode)
;;           (:mode org-mode)
;;           (:mode pod-mode)
;;           (:mode markdown-mode)
;;           (:mode git-commit-mode)
;;           (:filename "\\.\\(txt\\|mkn\\)$"))
;;          (lambda ()
;;            (flyspell-mode 1)
;;            (auto-fill-mode 1)))
;;         (((:mode c-mode)
;;           (:mode cperl-mode)
;;           (:mode emacs-lisp-mode))
;;          (lambda ()
;;            (flyspell-prog-mode)))
;;         (((:project "perl"))
;;          (lambda ()
;;            (when (save-excursion
;;                    (goto-char (point-min))
;;                    (re-search-forward "\t" nil t))
;;              (setq
;;               tab-width 8
;;               indent-tabs-mode t))))
;;         (((:project "Sub-Name"))
;;          (lambda ()
;;            (setq
;;             tab-width 8
;;             indent-tabs-mode t
;;             c-basic-offset 8)))
;;         (((:project "Memoize"))
;;          (lambda ()
;;            (setq
;;             tab-width 8
;;             indent-tabs-mode t)))
;;         (((:project "gnus"))
;;          (lambda ()
;;            (setq tab-width 8)))))


;;; Code:

(defvar espect-buffer-settings)

(defun espect-evaluate-buffer-condition (con)
  (cond
   ((functionp con) (funcall con))
   ((listp con)
    (cond
     ((listp (car con))
      (reduce (lambda (a b) (or a b))
              (mapcar #'espect-evaluate-buffer-condition con)
              :initial-value nil))
     (t
      (reduce
       (lambda (a b) (and a b))
       (let (ret)
         (while con
           (let ((k (pop con))
                 (v (pop con)))
             (push
              (cond
               ((eq k :fun) (funcall v))
               ((eq k :not)
                (when (not (listp v)) (error ":not requires a list"))
                (not (espect-evaluate-buffer-condition v)))
               ((eq k :mode)
                (if (stringp v)
                    (string-match-p v (symbol-name major-mode))
                  (eq v major-mode)))
               ((eq k :name)
                (string-match-p v (buffer-name)))
               ((eq k :filename)
                (cond ((and (buffer-file-name) (stringp v))
                       (string-match-p v (buffer-file-name)))
                      ((buffer-file-name) v)
                      (t (not v))))
               ((eq k :project)
                ;; assume eproject-maybe-turn-on was called from an earlier hook
                (cond
                 ((and eproject-root (stringp v))
                  (string= v (eproject-name)))
                 (eproject-root v)
                 (t (not v))))
               (t (error "unknown cond")))
              ret)))
         ret)
       :initial-value t))))
   (t (error "invalid condition"))))

;; FIXME: doesn't seem to work entirely when opening new files with emacsclient
;; or emacs -nw from the commandline
(defun espect-apply-buffer-settings (settings)
  (dolist (setting espect-buffer-settings)
    (let ((condition (car setting))
          (action (cadr setting)))
      (when (espect-evaluate-buffer-condition condition)
        (funcall action)))))

;;;###autoload
(add-hook 'after-change-major-mode-hook
          (lambda () (espect-apply-buffer-settings espect-buffer-settings))
          'append)

(provide 'espect)

;;; espect.el ends here
