(defvar concerto-default-username (format "%s@%s" (user-login-name) (system-name)))

(defvar concerto-broadcast-buffer-name "*concerto*")

(defun concerto-init-broadcast-buffer (connection buffer)
  (with-current-buffer buffer
    ;; TODO init?
    ))

(defun concerto-broadcast-buffer (&optional noprompt)
  "Return the broadcast buffer, create if necessary."
  (let ((buffer (get-buffer concerto-broadcast-buffer-name)))
    (or (if (buffer-live-p buffer) buffer)
        (let ((connection (get-process nrepl-connection-buffer)))
          (concerto-init-broadcast-buffer connection (get-buffer-create concerto-broadcast-buffer-name))))))

(defun concerto-insert (buffer face text)
  (with-current-buffer buffer
    (nrepl-propertize-region (list 'face face) (insert text))))

(defun concerto-broadcast-handler ()
  (lexical-let ((buffer (concerto-broadcast-buffer)))
    (lambda (username broadcast)
      (message "broadcast from %s: %s" username broadcast)
      (concerto-insert buffer 'nrepl-prompt-face (format "%s " username))
      (nrepl-dbind-response broadcast (code value ns out err status)
        (when code
          (concerto-insert buffer 'nrepl-prompt-face (format "%s> " ns))
          (concerto-insert buffer 'nrepl-input-face (format "%s\n" code)))
        (when value
          (concerto-insert buffer 'nrepl-result-face (format "%s\n" value)))
        (when out
          (concerto-insert buffer 'nrepl-output-face (format "%s\n" out)))
        (when err
          (concerto-insert buffer 'nrepl-error-face (format "%s\n" err)))))))

(concerto-insert (concerto-broadcast-buffer) 'nrepl-result-face "1")

(funcall (concerto-broadcast-handler) nil nil)

;; redefine the default handler so we can sneak in our broadcast handler
;; hopefully we can eventually add an extension mechanism to nrepl.el
(defun nrepl-default-handler (response)
  "Default handler which is invoked when no handler is found."
  (message "recv: %s" response)
  (nrepl-dbind-response response (out value username broadcast)
    (cond
     (out
      (nrepl-emit-interactive-output out))
     ((and username broadcast)
      (funcall (concerto-broadcast-handler) username broadcast)))))

(defun concerto-join-handler (buffer)
  (nrepl-make-response-handler buffer
                               nil
                               nil
                               (lambda (buffer err)
                                 (message (format "%s" err)))
                               (lambda (buffer)
                                 (message "Joined!"))))

(defun concerto-join (username)
    (message "Joining concerto as %s" username)
    (nrepl-send-request (list "op" "join"
                              "session" (nrepl-current-session)
                              "username" username)
                        (concerto-init-broadcast-buffer-handler concerto-concerto-buffer)))

;;;###autoload
(defun concerto (username)
  (interactive (list (read-string "Username: " concerto-default-username nil concerto-default-username)))
  (concerto-join username)
  (pop-to-buffer (concerto-broadcastbuffer)))
