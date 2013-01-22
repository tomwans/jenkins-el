
;;; jenkins.el --- Jenkins job control

;; Copyright (C) 2011, 2013  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords: tools, processes

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

;; (require 'jenkins)
;; M-x jenkins-select

;;; Code:

 ;; Dependencies, customizations, variables.

(require 'json)
(require 'url)
(eval-when-compile
  (require 'cl))

(defvar jenkins-url "http://jenkins.banksimple.com"
  "The current base URL of Jenkins.")
(make-local-variable 'jenkins-url)

(defvar jenkins-status nil
  "The status (job list etc) of the Jenkins server.")

(defgroup jenkins nil
  "Major mode for controlling Jenkins CI instances."
  :prefix "jenkins-"
  :group 'tools)

(defface jenkins-job
  '((t :weight bold))
  "Base face for Jenkins job lines.")

(defface jenkins-job-status-blue
  '((t :inherit jenkins-job
       :foreground "Black"
       :weight bold))
  "Face for blue (good) jobs."
  :group 'jenkins)

(defface jenkins-job-status-disabled
  '((t :inherit jenkins-job
       :foreground "#aaaaaa"
       :weight bold))
  "Face for disabled jobs."
  :group 'jenkins)

(defface jenkins-job-status-red
  '((t :inherit jenkins-job
       :foreground "Red"
       :slant italic))
  "Face for red (failing) jobs."
  :group 'jenkins)

(defface jenkins-job-number-face
  '((t :foreground "Orange"))
  "Face for job numbers."
  :group 'jenkins)

(defface jenkins-job-description-face
  '((t :foreground "Black"))
  "Face for job descriptions."
  :group 'jenkins)

 ;; Utils
(defun jenkins-trim (text)
  "Trim whitespace from the beginning and end of text."
  (replace-regexp-in-string "\\(^\\s-+\\|\\s-+$\\)" "" text))

 ;; Commands

(defun jenkins-select (base)
  (interactive (list (read-string "Jenkins URL: ")))
  (with-current-buffer (pop-to-buffer "*jenkins*")
    (setq jenkins-url base)
    (jenkins-refresh-status base 'jenkins-show-view)
    (jenkins-job-list-mode)
    (setq buffer-read-only t)
    (goto-char (point-min))))

 ;; Fetching

(defun jenkins-refresh-status (base &optional callback)
  (let ((buffer-read-only nil))
    (widen)
    (save-excursion
      (delete-region (point-min) (point-max))
      (url-retrieve
       (format "%s/api/json?depth=1" base)
       'jenkins-refresh-status-1 (list (current-buffer) callback)))))

(defun jenkins-refresh-status-1 (status buffer callback &rest args)
  (search-forward "\n\n")
  (setq jenkins-status (json-read))
  (when callback
    (with-current-buffer buffer
      (funcall callback)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(defun jenkins-redisplay ()
  (let ((buffer-read-only nil))
    (widen)
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (jenkins-refresh-status-1 nil (current-buffer))))

 ;; Job list

(defvar jenkins-job-list-mode-map
  (let ((map (make-sparse-keymap)))
    (mapcar (lambda (kpair) (apply 'define-key (cons map kpair)))
            '(("v" jenkins-switch-views)))
    map))

(define-derived-mode jenkins-job-list-mode fundamental-mode
  "A mode for viewing Jenkins job lists.

  \\{jenkins-job-list-mode-map}"
  :group 'jenkins

  (use-local-map jenkins-job-list-mode-map)
  (run-mode-hooks 'jenkins-job-list-mode-hook))

 ;; Jobs

(defun jenkins-jobs ()
  "Return an alist of all Jenkins jobs."
  (mapcar (lambda (job) (cons (cdr (assoc 'name job)) job))
          (cdr (assoc 'jobs jenkins-status))))

(defun jenkins-get-jobs (&rest jobs)
  "Return an alist of Jenkins jobs in JOBS."
  (let ((jenkins-jobs (jenkins-jobs)))
    (mapcar (lambda (job-name) (assoc job-name jenkins-jobs)) jobs)))

(defun jenkins-job-names ()
  "Return a list of Jenkins job names."
  (mapcar 'car (jenkins-jobs)))

(defun jenkins-job (job)
  "Return the Jenkins job named JOB."
  (cdr (assoc job (jenkins-jobs))))

(defun jenkins-list-jobs ()
  (interactive)
  (jenkins-refresh-status))

(defun jenkins-makeletvars (job)
  (mapcar (lambda (elt) (list (car elt) `(quote ,(cdr elt)))) job))

(defmacro with-job (job &rest body)
  `(let ,(jenkins-makeletvars job)
     ,@body))

(defmacro with-bounds (&rest body)
  "Execute BODY, and return (BEG . END) representing point's movement."
  (let ((startsym (gensym with-bounds-start))
        (endsym (gensym with-bounds-end)))
    `(let ((startsym (point))
           ,@body
           (cons startsym (point))))))

(defun jenkins-job-face (job)
  (intern (format "jenkins-job-status-%s" (cdr (assoc 'color job)))))

(defvar jenkins-job-mode-map
  (let ((map (make-sparse-keymap)))
    (mapcar (lambda (kpair) (apply 'define-key (cons map kpair)))
            '(("\r" jenkins-visit-current-job)
              ("c" jenkins-show-current-job-console)))
    map))

(defun jenkins-visit-current-job ()
  (interactive)
  (let ((job (get-text-property (point) 'job)))
    (browse-url (cdr (assoc 'url job)))))

(defun jenkins-show-current-job-console ()
  (interactive)
  (let ((job (get-text-property (point) 'job)))
    (jenkins-show-job-build-console
     (cdr (assoc 'name job))
     (cdr (assoc 'number (cdr (assoc 'lastBuild job)))))))

(defun jenkins-insert-job (job)
  (eval `(let ,(jenkins-makeletvars job)
           (insert
            (propertize displayName
                        'face (jenkins-job-face job)
                        'keymap jenkins-job-mode-map
                        'job job)
            (propertize " " 'job job)
            (propertize
             (let ((buildn (cdr (assoc 'number lastBuild))))
               (if buildn (format "#%d" buildn) "[Never built]"))
             'face 'jenkins-job-number-face
             'keymap jenkins-job-mode-map
             'job job)
            (propertize "\n"
                        'keymap jenkins-job-mode-map
                        'job job))
           (let ((desc (jenkins-trim (or description "")))
                 (p (point)))
             (unless (string= "" desc)
               (insert (propertize (format "  %s\n" desc)
                                   'face 'jenkins-job-description-face
                                   'keymap jenkins-job-mode-map
                                   'job job))
               (fill-region p (line-end-position)))))))

(defun jenkins-job-build-job (job)
  (macroexpand '(with-job job
                          (url-retrieve
                           (format "%s/api/json/job/%s/build" base name))))))


(defun jenkins-visit-job (job)
  (interactive (list (completing-read "Job: " (jenkins-jobs))))
  "Visit job JOB, optionally showing."
  (let ((job (if (stringp job) (jenkins-job job))))
    (browse-url (cdr (assoc 'url job)))))

(defun jenkins-choose-job-build ()
  (let* ((job (completing-read "Job: " (jenkins-jobs)))
         (build (completing-read
                 "Build: "
                 (mapcar (lambda (build)
                           (number-to-string (cdr (assoc 'number build))))
                         (cdr (assoc 'builds (jenkins-job job)))))))
    (list job (string-to-number build))))

(defun jenkins-job-special-builds (job)
  (let ((job (if (stringp job) (jenkins-job job) job))
        (special-alist '((firstBuild . :first)
                         (lastBuild . :last)
                         (lastCompletedBuild . :last-completed)
                         (lastFailedBuild . :last-failed)
                         (lastStableBuild . :last-stable)
                         (lastUnstableBuild . :last-unstable)
                         (lastSuccessfulBuild . :last-successful)
                         (lastUnsuccessfulBuild . :last-unsuccessful))))

    (mapcar (lambda (pair)
              (let* ((key (car pair))
                     (new-key (cdr pair))
                     (build (assoc key job)))
                (when build (cons new-key (cdr build)))))
            special-alist)))

(defun jenkins-job-builds (job)
  "Return an alist of the builds for JOB. Some special builds are included:

  :FIRST
  :LAST
  :LAST-COMPLETED
  :LAST-FAILED
  :LAST-STABLE
  :LAST-UNSTABLE
  :LAST-SUCCESSFUL
  :LAST-UNSUCCESSFUL"

  (let ((job (if (stringp job) (jenkins-job job) job)))
    (cons
     (jenkins-job-special-builds job)
     (mapcar
      (lambda (job)
        (cons (cdr (assoc 'number job)) job))
      (cdr (assoc 'builds job))))))

(defun jenkins-job-build (job build)
  (cond ((integerp build))
  (cdr (assoc build (jenkins-job-builds job)))))

(defun jenkins-visit-job-build (job build)
  "Visit job JOB, optionally showing."
  (interactive (jenkins-choose-job-build))
  (browse-url (cdr (assoc 'url (jenkins-job-build job build)))))

;; Consoles

(defvar jenkins-job-name nil
  "Current console job name.")
(make-variable-buffer-local 'jenkins-job-name)

(defvar jenkins-build-number nil
  "Current console build number.")
(make-variable-buffer-local 'jenkins-build-number)

(defvar jenkins-console-mode-map
  (let ((map (make-sparse-keymap)))
    (mapcar (lambda (kpair) (apply 'define-key (cons map kpair)))
            '(("q" bury-buffer)
              ("b" jenkins-job-build-console-show-build)
              ("w" jenkins-job-build-console-save-url)
              ("\C-c\C-o" jenkins)
              ("\M-n" jenkins-job-build-console-next-build)
              ("\M-p" jenkins-job-build-console-prev-build)
              ;; ("\C-c\C->" jenkins-show-last-job-build-console)
              ;; ("\C-c\C-<" jenkins-show-first-job-build-console)
              ("\C-c\C-b" jenkins-job-build-console-show-build)))
    map))

(defvar jenkins-console-font-lock-keywords
  `((,(concat "^" (regexp-opt '("archiving" "recording" "publishing")) ".*")
     (0 compilation-info))
    )

  "A list of keywords to highlight in Jenkins build consoles.")

(define-derived-mode jenkins-console-mode fundamental-mode
  "A mode for viewing Jenkins build consoles.
  \\{jenkins-console-mode-map}"
  :group 'jenkins

  (ansi-color-apply-on-region (point-min) (point-max))
  (save-excursion
    (replace-string "\r" "\n" nil (point-min) (point-max)))
  (set-buffer-modified-p nil)
  (set (make-local-variable 'font-lock-defaults)
       jenkins-console-font-lock-keywords)
  (setq buffer-read-only t))

(defun jenkins-job-build-console-save-url ()
  "Save the URL of this console in the kill ring."
  (interactive)
  (let ((url (format "%s/job/%s/%s/console" jenkins-url
                     jenkins-job-name jenkins-build-number)))
    (with-temp-buffer
      (insert url)
      (kill-ring-save (point-min) (point-max))
      (message "Saved console URL %s" url))))

(defun jenkins-job-build-console-show-build (&optional build)
  "Show the console for a specific build."
  (interactive "p")
  (let ((build (or build jenkins-job-build)))
    (url-retrieve (format "%s/job/%s/%s/consoleText" jenkins-url
                          jenkins-job-name build)
                  'jenkins-show-job-build-console-1
                  (list jenkins-url jenkins-job-name build))))

(defun jenkins-job-build-console-next-build ()
  "Show the console for the next build of this job."
  (interactive)
  (jenkins-show-job-build-console (+ jenkins-build-number 1)))

(defun jenkins-job-build-console-prev-build ()
  "Show the console for the previous build of this job."
  (interactive)
  (jenkins-show-job-build-console (- jenkins-build-number 1)))

(defun jenkins-job-build-console-last-build ()
  "Show the console for the last build of this job."

  )

(defun jenkins-visit-job-build-console (job build)
  "Visit job JOB, optionally showing."
  (interactive (jenkins-choose-job-build))
  (let ((build (jenkins-job-build job build)))
    (browse-url (format "%s/console" (cdr (assoc 'url build))))))

(defun jenkins-show-job-build-console-1 (status
                                         jenkins-url job-name build-number
                                         &rest args)
  (search-forward "\n\n")
  (let ((console (buffer-substring (point) (point-max))))
    (with-current-buffer
        (pop-to-buffer (format "*jenkins-console-%s*" job-name))
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert console)
        (setq jenkins-job-name job-name
              jenkins-build-number build-number
              jenkins-url jenkins-url)
        (jenkins-console-mode)))))

(defun jenkins-show-job-build-console (job build)
  "Visit job JOB, optionally showing."
  (interactive (jenkins-choose-job-build))
  (url-retrieve (format "%s/job/%s/%s/consoleText" jenkins-url job build)
                'jenkins-show-job-build-console-1
                (list jenkins-url job build)))

 ;; Views

(defun jenkins-show-view (&optional view)
  (let ((view (or view "Primary")))
    (with-current-buffer (pop-to-buffer "*jenkins*")
      (let ((buffer-read-only nil))
        (set-text-properties (point-min) (point-max) nil)
        (erase-buffer)
        (mapc 'jenkins-insert-job (mapcar 'cdr (jenkins-view-jobs view)))
        (set-buffer-modified-p nil)
        (goto-char (point-min))))))

(defun jenkins-view (view-name)
  "Return the view structure for a given view name."
  (cdr (assoc view-name (jenkins-views))))

(defun jenkins-view-names ()
  (mapcar 'car (jenkins-views)))

(defun jenkins-views ()
  (cons (cons "Primary" (cdr (assoc 'primaryView jenkins-status)))
   (mapcar (lambda (view) (cons (cdr (assoc 'name view)) view))
           (cdr (assoc 'views jenkins-status)))))

(defun jenkins-view-jobs (view)
  "Return jobs in VIEW."
  (let ((view (if (stringp view) (jenkins-view view) view)))
    (apply 'jenkins-get-jobs
           (mapcar
            (lambda (view-job) (cdr (assoc 'name view-job)))
            (cdr (assoc 'jobs view))))))

(defun jenkins-switch-views (view)
  (interactive (list (completing-read "View: " (jenkins-views) nil t)))
  (jenkins-show-view (jenkins-view view)))

(provide 'jenkins)
;;; jenkins.el ends here
