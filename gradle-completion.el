;;; gradle-completion.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 27 August 2017
;;

;; Author: Sébastien Le Maguer <slemaguer@coli.uni-saarland.de>

;; Package-Requires: ((emacs "25.2") (ivy "0.9.0"))
;; Keywords:
;; Homepage:

;; gradle-completion is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; gradle-completion is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gradle-completion.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'gradle-core)
(require 'ivy)

(defcustom gradle-cache-dir "~/.gradle/completion/"
  "Cache dir for the completion."
  :group 'gradle
  :type 'string)

(defvar gradle-ivy-hash-tasks nil
  "Cache of tasks"
  :group 'gradle
  :type 'hash)
(defun gradle-ivy-transformer (cmd)
  "Return CMD appended with the corresponding binding in the current window."
  (let ((desc (gethash cmd gradle-ivy-hash-tasks)))
    (if desc
      (format "%s (%s)"
              cmd (propertize (car desc) 'face 'font-lock-keyword-face))
      (format "%s" cmd))))

(ivy-set-display-transformer 'gradle-execute 'gradle-ivy-transformer)

(defun gradle-init-cache-dir ()
  "Initialize the cache dir by creating it"
  (mkdir gradle-cache-dir t))

(defun gradle-get-project-root-dir ()
  "Get the root project directory for the current build."
  (let ((current-dir (expand-file-name ".")) (root-dir nil))
    (while (and (not root-dir) (not (string= "/" current-dir)))
      ;; Remove the trailing "/" if some is detected
      (setq current-dir (replace-regexp-in-string "/$" "" current-dir))
      (when (or (file-exists-p (format "%s/gradlew" current-dir))
		(file-exists-p (format "%s/settings.gradle" current-dir)))
	(setq root-dir current-dir))
      (setq current-dir (file-name-directory current-dir)))
    root-dir))


(defun gradle-get-build-file ()
  "Get the project main build file."
  (interactive)
  (let ((root-dir (gradle-get-project-root-dir)))
    (if root-dir
	(format "%s/build.gradle" root-dir)
      (error "not in a gradle project!"))))

(defun gradle-get-cache-name ()
  "Get the name of the gradle cache."
  (replace-regexp-in-string "[^[:alnum:]]" "_" (gradle-get-build-file)))

(defun gradle-get-files-checksum ()
  "Get checksum for the project. It needs md5sum and awk for now."
  (interactive)
  (let ((cache-name (concat gradle-cache-dir (gradle-get-cache-name))) cmd)
    (setq cmd (format "cat %s" cache-name))
    (setq cmd (format "%s | xargs ls -o 2>/dev/null | md5sum | awk '{print $1}'" cmd))
  (replace-regexp-in-string "\n" "" (shell-command-to-string cmd))))

(defun gradle-generate-tasks-cache ()
  "Generate the task cache for gradle completion."
  (interactive)
  (let ((cmd (format "%s/gradlew" (gradle-get-project-root-dir)))
	gradle-tasks-output)
    (setq gradle-tasks-output
	  (shell-command-to-string
	   (format "%s --build-file %s -q tasks --all" cmd (gradle-get-build-file))))
    (message gradle-tasks-output)
    ))

(defun gradle-list-tasks ()
  "List the available tasks for the current project"
  (let ((default-directory  (gradle-run-from-dir (if gradle-use-gradlew
						     'gradle-is-gradlew-dir
						   'gradle-is-project-dir)))
	(root-file (concat gradle-cache-dir (gradle-get-cache-name) ".md5"))
	md5-filename list-tasks)

    ;; Create hash
    (setq gradle-ivy-hash-tasks (make-hash-table))

    ;; Fill hash
    (if (file-exists-p root-file)
	(progn
          ;; get hash filename
	  (setq md5-filename (concat gradle-cache-dir
				     (with-temp-buffer
				       (insert-file-contents root-file)
				       (replace-regexp-in-string "\n$" "" (buffer-string)))))

          ;; Fill hash

	  (if (file-exists-p md5-filename)
	      (progn
		(setq list-tasks (with-temp-buffer
				   (insert-file-contents md5-filename)
				   (split-string (buffer-string) "\n" t)))
		(cl-loop for task in list-tasks
			 collect  (let ((cur-task (split-string
						   (replace-regexp-in-string "[\\][:]" ":"
									     (replace-regexp-in-string "\\([^:]\\):\\([^:]*\\)$" "\\1\t\\2" task))
						   "\t" t)))
				    (puthash (car cur-task) (cdr cur-task) gradle-ivy-hash-tasks))))
            (display-warning 'gradle-mode (format-message "hash empty: %s doesn't exist, something went wrong" md5-filename)))
	  )

      (display-warning 'gradle-mode (format-message "%s doesn't exist, run init in your shell" root-file))


      ;; Return hash
      gradle-ivy-hash-tasks
      )))


(defun gradle-ivy-execute ()
  "Execute gradle command with TASKS supplied by user input."
  (interactive)
  (let ((prompt "Select a task to execute: ")
	task)
    (when (fboundp 'ivy-read)
      (progn
	(setq task
	      (ivy-read
	       prompt
	       (gradle-list-tasks)
               :require-match nil
	       :history 'gradle-tasks-history
	       ;; :initial-input initial-input
	       :caller 'gradle-execute)))
      (gradle-run task))))

(provide 'gradle-completion)
;;; gradle-completion.el ends here
