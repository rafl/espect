;;; espect.el --- aspect-oriented buffer settings

;;; Commentary:
;;

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
