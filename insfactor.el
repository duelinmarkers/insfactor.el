(require 'nrepl)

(defun insfactor-index-project ()
  (interactive)
  (nrepl-send-string "(do
                        (require 'duelinmarkers.insfactor.project)
                        (duelinmarkers.insfactor.project/index-project!))"
                     (nrepl-make-response-handler (nrepl-current-repl-buffer)
                                                  (lambda (buffer v) (message "%s" v))
                                                  (lambda (buffer v) (message "%s" v))
                                                  (lambda (buffer v) (message "%s" v))
                                                  (lambda (buffer v) (message "%s" v)))))

(defun insfactor-find-usages (query)
  (interactive "P")
  (nrepl-read-symbol-name "Symbol, keyword, or string literal in \": " 'insfactor-get-usages query))

(defun insfactor-get-usages (expr)
  (let* ((first-char (substring expr 0 1))
         (expr (cond ((or (string= ":" first-char)
                          (string= "\"" first-char))
                      expr)
                     ((string= "'" first-char)
                      (concat "#" expr))
                     (t
                      (concat "#'" expr))))
         (form (format "(do
                          (in-ns '%s)
                          (duelinmarkers.insfactor/find-usages %s))"
                       (nrepl-current-ns)
                       expr)))
    (nrepl-send-string form
                       (insfactor-make-find-usages-handler)
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

(defun insfactor-make-find-usages-handler ()
  (nrepl-make-response-handler (nrepl-make-popup-buffer "Usages")
                               (lambda (buffer value)
                                 (nrepl-popup-buffer-display buffer)
                                 (insfactor-render-find-usages-result
                                  buffer
                                  (first (read-from-string value))))
                               ;; (lambda (buffer out)
                               ;;   (message "out handler: %s" out)
                               ;;   (nrepl-emit-output buffer out t)
                               ;;   (nrepl-popup-buffer-display buffer))
                               nil
                               (lambda (buffer err)
                                 (message "err handler: %s" err)
                                 (nrepl-emit-output buffer err t)
                                 (nrepl-popup-buffer-display buffer))
                               ;; nil
                               nil))

(defun insfactor-render-find-usages-result (buffer data)
  (let* ((title (car data))
         (body
          (apply 'concat (mapcan (lambda (ns-group)
                                   (lexical-let ((ns (car ns-group)))
                                     (mapcar (lambda (loc)
                                               (format "%s:%s: %s"
                                                       ns
                                                       (car loc)
                                                       ;; (car (cdr loc)) column unused
                                                       (shell-command-to-string (format "sed -n '%s,%ss/^[ ]*//p' %s"
                                                                                        (car loc)
                                                                                        (car loc)
                                                                                        ns))))
                                             (cdr ns-group))))
                                 (cdr data))))
         (output (concat title "\n\n" body)))
    (nrepl-emit-into-popup-buffer buffer output)
    (save-excursion
      (with-current-buffer buffer
        (compilation-minor-mode)))))

(provide 'insfactor)
