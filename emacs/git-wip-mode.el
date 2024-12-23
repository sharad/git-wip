;;; git-wip-mode.el --- Use git-wip to record every buffer save

;; Copyright (C) 2013  Jerome Baum

;; Author: Jerome Baum <jerome@jeromebaum.com>
;; Version: 0.1
;; Keywords: vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'vc)

(defvar git-wip-push-commits-count 0
  "Push to emote if no of commits exceeds git-wip-push-commits-count.")

(defvar git-wip-buffer-name " *git-wip*"
  "Name of the buffer to which git-wip's output will be echoed")

(defvar git-wip-path
  (or
   ;; Internal copy of git-wip; preferred because it will be
   ;; version-matched
   (let ((git-wip-path (expand-file-name "../git-wip"
                                         (file-name-directory
                                          (or load-file-name
                                              (locate-library "git-wip-mode"))))))
     (when (file-exists-p git-wip-path)
       git-wip-path))
   ;; Look in $PATH and git exec-path
   (let ((exec-path (append exec-path
                            (parse-colon-path
                             (replace-regexp-in-string
                              "[ \t\n\r]+\\'" ""
                              (shell-command-to-string "git --exec-path"))))))
     (executable-find "git-wip"))))

(defun git-wip-after-save ()
  (when (and (string= (vc-backend (buffer-file-name))
                      "Git")
             git-wip-path)
    (let ((start-marker (if (get-buffer git-wip-buffer-name)
                            (with-current-buffer (get-buffer git-wip-buffer-name)
                              (buffer-end 1)))))
      (make-process
       :name "git-wip"
       :buffer git-wip-buffer-name
       :command (list git-wip-path
                      "save"
                      (concat "WIP from emacs: "
                              (file-name-nondirectory buffer-file-name))
                      "--editor"
                      "--push" (number-to-string git-wip-push-commits-count)
                      "--"
                      (file-name-nondirectory buffer-file-name))
       :sentinel #'(lambda (process event)
                     "Sentinel to handle process exit status."
                     (when (string-match-p "finished\\|exited" event)
                       (let ((exit-code (process-exit-status process)))
                         (message "event %s" event)
                         (message "exit-code %d" exit-code)
                         (if (zerop exit-code)
                             (message (concat "Wrote and git-wip'd "
                                              (buffer-file-name)))
                           (let* ((git-wip-buffer (get-buffer git-wip-buffer-name))
                                  (end-marker (if git-wip-buffer
                                                  (with-current-buffer git-wip-buffer
                                                    (buffer-end 1))))
                                  (text (with-current-buffer git-wip-buffer
                                          (if (and start-marker end-marker)
                                              (buffer-substring start-marker end-marker)
                                            (when end-marker (buffer-string))))))
                             (message "Process %s-push failed with exit code %d\n%s"
                                      (process-name process)
                                      exit-code
                                      text)
                             (error "Process %s-push failed with exit code %d\n%s"
                                    (process-name process)
                                    exit-code
                                    text))))))))))

;;;###autoload
(define-minor-mode git-wip-mode
  "Toggle git-wip mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When git-wip mode is enabled, git-wip will be called every time
you save a buffer."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " WIP"
  :group 'git-wip

  ;; (de-)register our hook
  (if git-wip-mode
      (add-hook 'after-save-hook 'git-wip-after-save nil t)
    (remove-hook 'after-save-hook 'git-wip-after-save t)))

  (defun git-wip-mode-if-git ()
    (when (string= (vc-backend (buffer-file-name)) "Git")
      (git-wip-mode t)))

  (add-hook 'find-file-hook 'git-wip-mode-if-git)

  (provide 'git-wip-mode)
  ;;; git-wip-mode.el ends here
