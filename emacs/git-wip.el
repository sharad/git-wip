;; -*- mode: emacs-lisp; -*-


(defun git-wip-push (arg)
  (interactive "P")
  (when (string= (vc-backend (buffer-file-name)) "Git")
    (let ((start-marker (if (get-buffer git-wip-buffer-name)
                            (with-current-buffer (get-buffer git-wip-buffer-name)
                              (buffer-end)))))
      (make-process :name "git-wip"
                    :buffer git-wip-buffer-name
                    :command (if arg
                                 (list git-wip-path "push" "-f" "--")
                               (list git-wip-path "push" "--"))
                    :sentinel #'(lambda (process event)
                                  "Sentinel to handle process exit status."
                                  (when (string-match-p "finished\\|exited" event)
                                    (let ((exit-code (process-exit-status process)))
                                      (message "event %s" event)
                                      (message "exit-code %d" exit-code)
                                      (if (zerop exit-code)
                                          (message (concat "Wrote and git-wip-push'd "
                                                           (buffer-file-name)))
                                        (let* ((git-wip-buffer (get-buffer git-wip-buffer-name))
                                               (end-marker (if git-wip-buffer
                                                               (with-current-buffer git-wip-buffer
                                                                 (buffer-end))))
                                               (text (with-current-buffer git-wip-buffer
                                                       (if (and start-marker end-marker)
                                                           (buffer-substring start-marker end-marker)
                                                         (when end-marker (buffer-string))))))
                                          (message "Process %s-push failed with exit code %d"
                                                   (process-name process)
                                                   exit-code)
                                          (error "Process %s-push failed with exit code %d"
                                                 (process-name process)
                                                 exit-code))))))))))




(defun git-wip-wrapper ()
  (interactive)
  (let ((file-arg (shell-quote-argument (buffer-file-name))))
    (shell-command (concat "git-wip save \"WIP from emacs: " (buffer-file-name) "\" --editor -- " file-arg))
    (message (concat "Wrote and git-wip'd " (buffer-file-name)))))

(defun git-wip-if-git ()
  (interactive)
  (when (string= (vc-backend (buffer-file-name)) "Git")
    (git-wip-wrapper)))

(add-hook 'after-save-hook 'git-wip-if-git)


(provide 'git-wip)

