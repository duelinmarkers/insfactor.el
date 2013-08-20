(require 'nrepl)

(defun insfactor-find-usages (query)
  (interactive "P")
  (nrepl-read-symbol-name "Symbol, keyword, or string literal in \": " 'insfactor-get-usages query))

(defun insfactor-get-usages (expr)
  (let* ((first-char (substring expr 0 1))
         (expr (cond ((or (string= ":" first-char)
                          (string= "\"" first-char))
                      expr)
                     ((string= "'" first-char)
                     ((concat ) "#" expr))
                     (t
                      (concat "#'" expr))))
         (form (format "(do
                          (in-ns '%s)
                          (duelinmarkers.insfactor/find-usages %s))"
                       (nrepl-current-ns)
                       expr)))
    (nrepl-send-string form
                       ;; (insfactor-make-find-usages-handler)
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
  (nrepl-emit-into-popup-buffer buffer data)
  ;; TODO use mapc to iterate over data and format & insert each usage
  )
