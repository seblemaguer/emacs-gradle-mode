;;; gradle-mode.el --- Gradle integration with Emacs' compile

;; Copyright (C) 2014 by Daniel Mijares

;; Author: Daniel Mijares <daniel.j.mijares@gmail.com>
;; Maintainer: Daniel Mijares <daniel.j.mijares@gmail.com>
;; URL: http://github.com/jacobono/emacs-gradle-mode
;; Version: 0.5.3
;; Keywords: gradle
;; Package-Requires: ((s "1.8.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Gradle integration into Emacs, through compile-mode.
;; see documentation on https://github.com/jacobono/emacs-gradle-mode

;;; Code:

;;; --------------------------
;; gradle-mode dependencies
;;; --------------------------

(require 's)
(require 'compile)

;;; --------------------------
;; gradle-mode variables
;;; --------------------------

;; error processing if no executable??
(defcustom gradle-executable-path (executable-find "gradle")
  "String representation of the Gradle executable location.
Absolute path, usually found with `executable-find'."
  :group 'gradle
  :type 'string)

(defcustom gradle-gradlew-executable "gradlew"
  "String representation of the gradlew executable."
  :group 'gradle
  :type 'string)

(defcustom gradle-continuous-option "--continuous"
  "String representation of the continuous option"
  :group 'gradle
  :type 'string)

(defcustom gradle-quiet-option "-q"
  "String representation of the quiet option"
  :group 'gradle
  :type 'string)

(defcustom gradle-use-gradlew nil
  "Use gradlew or `gradle-executable-path'.
If true, run gradlew from its location in the project file system.
If false, will find project build file and run `gradle-executable-path' from there."
  :group 'gradle
  :type 'boolean)

(defcustom gradle-quiet-activation nil
  "Silent main part except error and warnings provided by gradle"
  :group 'gradle
  :type 'boolean)

(defcustom gradle-continuous-activation nil
  "Continuous building"
  :group 'gradle
  :type 'boolean)

;;; --------------------------
;; gradle-mode private functions
;;; --------------------------

(defun gradle-is-project-dir (dir)
  "Is this DIR a gradle project directory with an extra convention.
A project dir is always considered if there is a 'build.gradle' there.
A project dir is also considered if there is a '{dirname}.gradle'.  This
is a convention for multi-build projects, where dirname is under some
'rootDir/dirname/dirname.gradle'."
  (let ((dirname (file-name-nondirectory
                  (directory-file-name (expand-file-name dir)))))
    (or (file-exists-p (expand-file-name "build.gradle" dir))
        (file-exists-p (expand-file-name
                        (concat dirname ".gradle") dir)))))

(defun gradle-is-gradlew-dir (dir)
  "Does this DIR contain a gradlew executable file."
  (file-exists-p (expand-file-name "gradlew" dir)))

(defun gradle-run-from-dir (is-dir)
  "Find the closest dir to execute the gradle command under via IS-DIR function.
If there is a folder you care to run from higher than this level, you need to move out to that level (eg. through dired etc.)."
  (locate-dominating-file default-directory is-dir))

(defun gradle-kill-compilation-buffer ()
  "Kill compilation buffer if present."
  (progn
    (if (get-buffer "*compilation*")
	(progn
	  (delete-windows-on (get-buffer "*compilation*"))
	  (kill-buffer "*compilation*")))))

(defun gradle-run (gradle-tasks)
  "Run gradle command with `GRADLE-TASKS' and options supplied."
  (gradle-kill-compilation-buffer)
  (let ((default-directory
          (gradle-run-from-dir (if gradle-use-gradlew
                                   'gradle-is-gradlew-dir
                                 'gradle-is-project-dir))))
    (compile (gradle-make-command gradle-tasks))))

(defun gradle-make-command (gradle-tasks)
  "Make the gradle command, using some executable path and GRADLE-TASKS."
  (let ((gradle-executable (if gradle-use-gradlew
                               gradle-gradlew-executable
                             gradle-executable-path))
        (gradle-cmd '(gradle-executable)))
    (progn
      (when gradle-quiet-activation
        (add-to-list 'gradle-cmd gradle-quiet-option))
      (when gradle-continuous-activation
        (add-to-list 'gradle-cmd gradle-continuous-option))
      (add-to-list 'gradle-cmd gradle-tasks)
      (s-join " " gradle-cmd))))



(defvar gradle-ivy-hash-tasks nil)

(defun gradle-ivy-transformer (cmd)
  "Return CMD appended with the corresponding binding in the current window."
  (let ((desc (gethash cmd gradle-ivy-hash-tasks)))
    (if desc
      (format "%s (%s)"
              cmd (propertize (car desc) 'face 'font-lock-keyword-face))
      (format "%s" cmd))))

(ivy-set-display-transformer
 'gradle-execute
 'gradle-ivy-transformer)

(defun gradle-list-tasks ()
  "List the available tasks for the current project"
  (let ((default-directory  (gradle-run-from-dir (if gradle-use-gradlew
						     'gradle-is-gradlew-dir
						   'gradle-is-project-dir)))
	(root-file (concat "~/.gradle/completion/"
			   (replace-regexp-in-string "[^[:alnum:]]" "_" (expand-file-name (concat default-directory "/build.gradle")))
			   ".md5"))
	md5-filename list-tasks)
    (if (file-exists-p root-file)
	(progn
	  (setq md5-filename (concat "~/.gradle/completion/"
				     (with-temp-buffer
				       (insert-file-contents root-file)
				       (replace-regexp-in-string "\n$" "" (buffer-string)))))
	  (if (file-exists-p md5-filename)
	      (progn
		(setq gradle-ivy-hash-tasks (make-hash-table))
		(setq list-tasks (with-temp-buffer
				   (insert-file-contents md5-filename)
				   (split-string (buffer-string) "\n" t)))
		(cl-loop for task in list-tasks
			 collect  (let ((cur-task (split-string
						   (replace-regexp-in-string "[\\][:]" ":"
									     (replace-regexp-in-string "\\([^:]\\):\\([^:]*\\)$" "\\1\t\\2" task))
						   "\t" t)))
				    (puthash (car cur-task) (cdr cur-task) gradle-ivy-hash-tasks)))
		gradle-ivy-hash-tasks)
	    (error (format-message "%s doesn't exist, something went wrong" md5-filename))))
      (display-warning 'gradle-mode (format-message "%s doesn't exist, run init in your shell" root-file))))
  )

;;; --------------------------
;; gradle-mode interactive functions
;;; --------------------------

(defun gradle-execute ()
  "Execute gradle command with TASKS supplied by user input."
  (interactive)
  (let ((prompt "Select a task to execute: ")
	(choices (gradle-list-tasks))
	task)
    (when (fboundp 'ivy-read)
      (progn
	(setq task
	      (ivy-read
	       prompt choices
	       :history 'gradle-tasks-history
	       ;; :initial-input initial-input
	       :caller 'gradle-execute)))
      (gradle-run task))))

(defun gradle-build ()
  "Execute gradle build command."
  (interactive)
  (gradle-run "build"))

(defun gradle-test ()
  "Execute gradle test command."
  (interactive)
  (gradle-run "test"))

(defun gradle-single-test (single-test-name)
  "Execute gradle test on file SINGLE-TEST-NAME supplied by user."
  (interactive "sType single test to run: ")
  (gradle-run
   (s-concat "test -Dtest.single=" single-test-name)))

(defun gradle-execute--daemon (tasks)
  "Execute gradle command, using daemon, with TASKS supplied by user input."
  (interactive "sType tasks to run: ")
  (gradle-run
   (s-concat tasks " --daemon")))

(defun gradle-build--daemon ()
  "Execute gradle build command, using daemon."
  (interactive)
  (gradle-run "build --daemon"))

(defun gradle-test--daemon ()
  "Execute gradle test command, using daemon."
  (interactive)
  (gradle-run "test --daemon"))

(defun gradle-single-test--daemon (single-test-name)
  "Execute gradle test, using daemon, on file SINGLE-TEST-NAME supplied by user."
  (interactive "sType single test to run: ")
  (gradle-run
   (s-concat "test -Dtest.single=" single-test-name " --daemon")))

;;; ----------
;; gradle-mode keybindings
;;; ----------

(defvar gradle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g b") 'gradle-build)
    (define-key map (kbd "C-c C-g t") 'gradle-test)
    (define-key map (kbd "C-c C-g s") 'gradle-single-test)
    (define-key map (kbd "C-c C-g C-d b") 'gradle-build--daemon)
    (define-key map (kbd "C-c C-g C-d t") 'gradle-test--daemon)
    (define-key map (kbd "C-c C-g C-d s") 'gradle-single-test--daemon)
    (define-key map (kbd "C-c C-g d") 'gradle-execute--daemon)
    (define-key map (kbd "C-c C-g r") 'gradle-execute)
    map)
  "Keymap for the gradle minor mode.")

;;;###autoload
(define-minor-mode gradle-mode
  "Emacs minor mode for integrating Gradle into compile.
Run gradle tasks from any buffer, scanning up to nearest gradle
directory to run tasks."
  :lighter " Gradle"
  :keymap 'gradle-mode-map
  :global t)

(provide 'gradle-mode)

;;; gradle-mode.el ends here
