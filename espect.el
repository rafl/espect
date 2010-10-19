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
;;       '(((:not (:mode "^gnus") (:mode w3m-mode) (:mode "^erc")
;;                (:mode eshell-mode) (:mode term-mode) (:mode gud-mode))
;;          (lambda () nil))
;;         ((:not (:mode eshell-mode) (:mode term-mode))
;;          (lambda () (hl-line-mode t)))
;;         ((:mode message-mode)
;;           (:mode org-mode)
;;           (:mode pod-mode)
;;           (:mode markdown-mode)
;;           (:mode git-commit-mode)
;;           (:filename "\\.\\(txt\\|mkn\\)$")
;;          (lambda ()
;;            (flyspell-mode 1)
;;            (auto-fill-mode 1)))
;;         ((:mode c-mode)
;;          (:mode cperl-mode)
;;          (:mode emacs-lisp-mode)
;;          (lambda ()
;;            (flyspell-prog-mode)))
;;         ((:project "perl")
;;          (lambda ()
;;            (when (save-excursion
;;                    (goto-char (point-min))
;;                    (re-search-forward "\t" nil t))
;;              (setq
;;               tab-width 8
;;               indent-tabs-mode t))))
;;         ((:project "Sub-Name")
;;          (lambda ()
;;            (setq
;;             tab-width 8
;;             indent-tabs-mode t
;;             c-basic-offset 8)))
;;         ((:project "Memoize")
;;          (lambda ()
;;            (setq
;;             tab-width 8
;;             indent-tabs-mode t)))
;;         ((:project "gnus")
;;          (lambda ()
;;            (setq tab-width 8)))))

;;; Code:

(defvar espect-rules (make-hash-table)
  "A hash table matching rule names to a lambda that implements them.")

(defmacro* define-espect-rule (name lambda-list &body code)
  "Define a rule name NAME that executes CODE."
  (puthash name `(lambda ,lambda-list ,@code) espect-rules))

(defun espect-eval-rule (name args)
  "Lookup rule NAME in `espect-rules', and evaluate it against ARGS.

NAME is the symbol to look up.
ARGS is the argument list to apply to the rule."
  (let ((rule (gethash name espect-rules)))
    (when (not rule) (error "No espect rule %s" name))
    (apply rule args)))

(defun espect-eval-conditions (list)
  "Evaluate, in order, each rule in LIST with `espect-eval-rule', returning nil as soon as a condition returns nil.  Otherwise return true."
  (let ((rule (car list))
        (rest (cdr list)))
    (and (espect-eval-rule (car rule) (cdr rule))
         (or (null rest)
             (espect-eval-conditions rest)))))

(defun espect-eval-script (list) ;; list = (rule rule ... code)
  (let* ((reversed (reverse list))
         (code (car reversed))
         (rules (reverse (cdr reversed))))
    (message "%s %s" code rules)
    (when (espect-eval-conditions rules) (funcall code))))


(define-espect-rule :not (&rest xs)       (not (espect-eval-conditions xs)))
(define-espect-rule :fun (fun &rest args) (apply fun args))
(define-espect-rule :mode (mode)          (cond ((stringp mode) (string-match mode (format "%s" major-mode)))
                                                ((symbolp mode) (eq mode major-mode))
                                                (t (error "Mode %s must be a string or symbol!" mode))))
(define-espect-rule :name (name)          (string-match name (buffer-name)))
(define-espect-rule :filename (filename)  (string-match filename buffer-file-name))
(define-espect-rule :project-type (type)  (ignore-errors (string-match type (eproject-type))))
(define-espect-rule :project-root (root)  (ignore-errors (string-match root (eproject-root))))
(define-espect-rule :project      (x)     (ignore-errors (or (string-match x (eproject-root))
                                                             (string-match x (eproject-type)))))

(defvar espect-buffer-settings nil
  "List of espect scripts to run for every new buffer.")

;; FIXME: doesn't seem to work entirely when opening new files with emacsclient
;; or emacs -nw from the commandline
(defun espect-apply-buffer-settings (settings)
  (dolist (script espect-buffer-settings) (espect-eval-script script)))

;;;###autoload
(add-hook 'after-change-major-mode-hook
          (lambda () (espect-apply-buffer-settings espect-buffer-settings))
          'append)

(provide 'espect)

;;; espect.el ends here
