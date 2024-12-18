;;; git-wip.el --- git wip                -*- mode: emacs-lisp; lexical-binding: t; -*-

(provide 'git-wip)


;;;###autoload
(defun git-wip-push (arg)
  (interactive "P")
  (when (string= (vc-backend (buffer-file-name)) "Git")
    (let* ((git-wip-buffer (get-buffer git-wip-buffer-name))
           (start-marker (when git-wip-buffer
                           (with-current-buffer git-wip-buffer
                             (buffer-end 1)))))
      (make-process :name "git-wip"
                    :buffer git-wip-buffer
                    :command (if arg
                                 (list git-wip-path "push" "-f" "--")
                               (list git-wip-path "push" "--"))
                    :sentinel #'(lambda (process event)
                                  "Sentinel to handle process exit status."
                                  (when (string-match-p "finished\\|exited" event)
                                    (let ((exit-code (process-exit-status process)))
                                      (if (zerop exit-code)
                                          (message (concat "Wrote and git-wip-push'd "
                                                           (buffer-file-name)))
                                        (let* ((end-marker (if git-wip-buffer
                                                               (with-current-buffer git-wip-buffer
                                                                 (buffer-end 1))))
                                               (text (with-current-buffer git-wip-buffer
                                                       (if (and start-marker end-marker)
                                                           (buffer-substring start-marker end-marker)
                                                         (when end-marker (buffer-string))))))
                                          (error "Process %s-push failed with exit code %d\n%s"
                                                 (process-name process)
                                                 exit-code
                                                 text))))))))))
;;;###autoload
(defun git-wip-wrapper ()
  (interactive)
  (let ((file-arg (shell-quote-argument (buffer-file-name))))
    (shell-command (concat "git-wip save \"WIP from emacs: " (buffer-file-name) "\" --editor -- " file-arg))
    (message (concat "Wrote and git-wip'd " (buffer-file-name)))))
;;;###autoload
(defun git-wip-if-git ()
  (interactive)
  (when (string= (vc-backend (buffer-file-name)) "Git")
    (git-wip-wrapper)))
;;;###autoload
(defun git-wip-save-insinuate ()
  (add-hook 'after-save-hook
            'git-wip-if-git))

;;; git-wip.el ends here
