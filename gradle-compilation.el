;;; gradle-compilation.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C)  6 January 2018
;;

;; Author:  <slemaguer@coli.uni-saarland.de>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; gradle-compilation is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; gradle-compilation is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gradle-compilation.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'compile)


;; ==================================================================
;; # Faces
;; ==================================================================
(defface gradle-compilation-success-face
  '((t :foreground "green"
       :weight bold))
  "Face for success."
  :group 'gradle-faces)

(defface gradle-compilation-error-face
  '((t :foreground "red"
       :weight bold))
  "Face for failure."
  :group 'gradle-faces)


;; ==================================================================
;; # Variables
;; ==================================================================
(defvar gradle-buffer--save-buffers-predicate
  (lambda ()
    (not (string= (substring (buffer-name) 0 1) "*"))))

(defvar gradle-buffer--buffer-name nil
  "Used to store compilation name so recompilation works as expected.")
(make-variable-buffer-local 'gradle-buffer--buffer-name)

(defvar gradle-buffer--error-link-options
  '(gradle "\\([-A-Za-z0-9./_]+\\):\\([0-9]+\\)\\(: warning\\)?" 1 2 nil (3) 1)
  "File link matcher for `compilation-error-regexp-alist-alist' (matches path/to/file:line).")


;; ==================================================================
;; # Hooks
;; ==================================================================
(defun gradle-buffer--handle-compilation ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun gradle-buffer--handle-compilation-once ()
  (remove-hook 'compilation-filter-hook 'gradle-buffer--handle-compilation-once t)
  (delete-matching-lines "\\(-*- mode:\\|^$\\|gradle run\\|Loading config\\|--no-single-run\\|Gradle finished\\|Gradle started\\|gradle-compilation;\\)"
                         (point-min) (point)))

;; ==================================================================
;; # Helper function function
;; ==================================================================
(defun gradle-buffer--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))


;; ==================================================================
;; # Compile function
;; ==================================================================
(defun gradle-compile (cmd buffer-name)
  "Run CMD in `buffer-name'.
Returns the compilation buffer.
Argument BUFFER-NAME for the compilation."
  (save-some-buffers (not compilation-ask-about-save) gradle-buffer--save-buffers-predicate)
  (let* ((gradle-buffer--buffer-name buffer-name)
         (compilation-filter-start (point-min)))
    (with-current-buffer
        (progn
          ;; Define some local info
          (setq compile-command cmd)
          (setq-local compilation-directory default-directory)

          ;; Compile
          (compilation-start cmd 'gradle-compilation-mode (lambda (b) gradle-buffer--buffer-name)))

      ;; Adapt some hook to color everything
      (setq-local compilation-error-regexp-alist-alist
                  (cons gradle-buffer--error-link-options compilation-error-regexp-alist-alist))
      (setq-local compilation-error-regexp-alist (cons 'gradle compilation-error-regexp-alist))
      (add-hook 'compilation-filter-hook 'gradle-buffer--handle-compilation nil t)
      (add-hook 'compilation-filter-hook 'gradle-buffer--handle-compilation-once nil t))))


;; ==================================================================
;; # Compilation mode
;; ==================================================================
(define-compilation-mode gradle-compilation-mode "Gradle compilation"
  "Gradle dedicated compilation mode."
  (progn
    (font-lock-add-keywords nil
                            '(("^:.*:" . 'font-lock-comment-face) ;; Project
			      ("\\(UP-TO-DATE\\|NO-SOURCE\\)" . 'gradle-compilation-success-face)
			      ))
    ;; Set any bound buffer name buffer-locally
    (setq gradle-buffer--buffer-name gradle-buffer--buffer-name)
    (set (make-local-variable 'kill-buffer-hook)
         'gradle-buffer--kill-any-orphan-proc)))




(provide 'gradle-compilation)

;;; gradle-compilation.el ends here
