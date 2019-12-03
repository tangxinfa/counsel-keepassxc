;;; counsel-keepassxc.el --- Complete keepassxc-cli with Ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018  Free Software Foundation, Inc.

;; Author: tangxinfa <tangxinfa@gmail.com>
;; Keywords: matching

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

(require 'ivy)

;;** `counsel-keepassxc'
(defvar counsel-keepassxc-database-file nil "Keepassxc password database file.")

(defun counsel-keepassxc--candidates (master-password)
  "Return list of keepassxc entries, MASTER-PASSWORD to open database."
  (unless counsel-keepassxc-database-file
    (signal
     'file-error
     (list "Opening `counsel-keepassxc-database-file'" "No such readable file"
           counsel-keepassxc-database-file)))
  (let* ((args)
         (entries
          (with-temp-buffer
            (insert master-password)
            (setq args (list (point-min)
                             (point-max)
                             "keepassxc-cli"
                             t t nil
                             "locate"
                             (expand-file-name counsel-keepassxc-database-file)
                             "/"))
            (if (not (eq 0 (apply 'call-process-region args)))
                (error
                 "Error: execute keepassxc-cli locate failed"))
            (split-string (buffer-string) "\n")))
         (candidates
          (remove nil
                  (mapcar
                   (lambda (entry)
                     (unless (string-prefix-p "Enter password to unlock"
                                              entry)
                       (list entry master-password)))
                   entries))))
    candidates))

(defun counsel-keepassxc--entry-parse (&optional entry-path)
  "Parse entry in current buffer, ENTRY-PATH for the path of the entry."
  (save-excursion
    (goto-char (point-min))
    (let ((entry)
          (fields)
          (filters '("UserName" "Password" "URL" "Notes")))
      (if entry-path
          (add-to-list 'entry (cons "Title" entry-path))
        (add-to-list 'filters "Title"))
      (while (not (eobp))
        (setq fields (split-string
                      (buffer-substring
                       (point)
                       (point-at-eol))
                      ": "))
        (if (member (car fields) filters)
            (add-to-list
             'entry
             (cons (car fields)
                   (string-trim (string-join (cdr fields) ": ")))
             t))
        (beginning-of-line 2))
      entry)))

(defun counsel-keepassxc--entry-get (candidate)
  "Get entry match CANDIDATE."
  (with-temp-buffer
    (insert (cadr candidate))
    (let* ((entry-path (car candidate))
           (args (list (point-min)
                       (point-max)
                       "keepassxc-cli"
                       t t nil
                       "show"
                       "--show-protected"
                       (expand-file-name
                        counsel-keepassxc-database-file)
                       entry-path)))
      (if (not (eq 0 (apply 'call-process-region args)))
          (error
           "Error: execute keepassxc-cli show failed"))
      (counsel-keepassxc--entry-parse entry-path))))

(defun counsel-keepassxc--copy-password (candidate)
  "Copy password of CANDIDATE into current buffer."
  (kill-new (assoc-default "Password" (counsel-keepassxc--entry-get candidate) nil "")))

(defun counsel-keepassxc--copy-username (candidate)
  "Copy username of CANDIDATE into current buffer."
  (kill-new (assoc-default "UserName" (counsel-keepassxc--entry-get candidate) nil "")))

(defun counsel-keepassxc--copy-url (candidate)
  "Copy url of CANDIDATE into current buffer."
  (kill-new (assoc-default "URL" (counsel-keepassxc--entry-get candidate) nil "")))

(defun counsel-keepassxc--copy-notes (candidate)
  "Copy notes of CANDIDATE into current buffer."
  (kill-new (assoc-default "Notes" (counsel-keepassxc--entry-get candidate) nil "")))

(defun counsel-keepassxc--entry-commit ()
  "Commit added or edited entry."
  (interactive)
  (let* ((entry (counsel-keepassxc--entry-parse))
         (return nil)
         (entry-buffer (current-buffer))
         (generates (split-string (assoc-default "Password" entry nil "") "Generate"))
         (args nil)
         (action (buffer-local-value 'keepassxc-action entry-buffer))
         (candidate (buffer-local-value 'keepassxc-candidate entry-buffer))
         (delete-old nil))
    (unless (or (string= action "edit")
                (string= action "add"))
      (error
       "Error: commit not allowed when %s keepassxc entry"
       action))
    ;;;; If title changed when editing, treat as add a new entry and delete old entry.
    (when (and (string= action "edit")
               (not (string= (string-trim-left (car candidate) "/")
                             (string-trim-left (assoc-default "Title" entry nil "") "/"))))
      (setq action "add")
      (setq delete-old t))
    (with-temp-buffer
      (insert (cadr (buffer-local-value 'keepassxc-candidate entry-buffer)))
      (insert "\n")
      (when (< (length generates) 2)
        (insert (assoc-default "Password" entry nil ""))
        (insert "\n"))
      (setq args
            (list
             (point-min)
             (point-max)
             "keepassxc-cli"
             t nil t
             action
             (expand-file-name counsel-keepassxc-database-file)
             (assoc-default "Title" entry nil "")
             "-u"
             (assoc-default "UserName" entry nil "")
             "--url"
             (assoc-default "URL" entry nil "")))
      (if (< (length generates) 2)
          (add-to-list 'args "-p" t)
        (add-to-list 'args "-g" t)
        (when (> (string-to-number (second generates)) 0)
          (add-to-list 'args "-l" t)
          (add-to-list 'args (second generates) t)))
      (setq return (apply 'call-process-region args)))
    (if (not (eq return 0))
        (error
         "Error: execute keepassxc-cli %s failed"
         action)
      (when delete-old (counsel-keepassxc--delete candidate))
      (kill-buffer entry-buffer)
      (message "keepassxc-cli %s entry \"%s\" succeed"
               action
               (assoc-default "Title" entry nil "")))))

(defun counsel-keepassxc--entry-edit ()
  "Enter edit entry mode."
  (interactive)
  (unless (eq major-mode 'counsel-keepassxc-entry-mode)
    (error
     "Error: major-mode must be `counsel-keepassxc-entry-mode'"))
  (let ((candidate keepassxc-candidate)
        (position (point)))
    (kill-buffer (current-buffer))
    (counsel-keepassxc--edit candidate)
    (goto-char position)))

(defun counsel-keepassxc--entry-abort ()
  "Abort added or edited entry."
  (interactive)
  (kill-buffer (current-buffer)))

(defun counsel-keepassxc--entry-next-field ()
  "Move to next field."
  (interactive)
  (unless (search-forward ": " nil t)
    (goto-char (point-min))
    (search-forward ": ")))

(setq counsel-keepassxc-entry-highlights '(("^Password: Generate[0-9]*" . font-lock-keyword-face)
                                           ("^Title: \\|^UserName: \\|^Password: \\|^URL: \\|^Notes: "
                                            . font-lock-type-face)))

(defvar counsel-keepassxc-entry-mode-map (make-sparse-keymap)
  "Keymap for `counsel-keepassxc-entry-mode'.")
(define-key counsel-keepassxc-entry-mode-map (kbd "C-c C-c") 'counsel-keepassxc--entry-commit)
(define-key counsel-keepassxc-entry-mode-map (kbd "C-c C-e") 'counsel-keepassxc--entry-edit)
(define-key counsel-keepassxc-entry-mode-map (kbd "C-c C-k") 'counsel-keepassxc--entry-abort)
(define-key counsel-keepassxc-entry-mode-map (kbd "TAB") 'counsel-keepassxc--entry-next-field)

(define-derived-mode counsel-keepassxc-entry-mode text-mode
  "counsel-keepassxc-entry-mode"
  "major mode for editing keepassxc entry."
  (setq font-lock-defaults '(counsel-keepassxc-entry-highlights)))

(defun counsel-keepassxc--view (&optional candidate)
  "View entry, CANDIDATE is the entry to view."
  (let ((buffer (generate-new-buffer "*keepassxc-view*"))
        (entry (counsel-keepassxc--entry-get candidate)))
    (with-current-buffer buffer
      (counsel-keepassxc-entry-mode)
      (insert (format
               "View Keepassxc Entry.\n========================\nTitle: %s\nUserName: %s\nPassword: %s\nURL: %s\nNotes: %s\n"
               (assoc-default "Title" entry nil "")
               (assoc-default "UserName" entry nil "")
               (assoc-default "Password" entry nil "")
               (assoc-default "URL" entry nil "")
               (assoc-default "Notes" entry nil "")))
      (forward-line -5)
      (goto-char (point-at-eol))
      (read-only-mode)
      (set (make-local-variable 'keepassxc-candidate) candidate)
      (set (make-local-variable 'keepassxc-action) "view"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--edit (&optional candidate)
  "Edit entry, CANDIDATE is the entry to edit."
  (let ((buffer (generate-new-buffer "*keepassxc-edit*"))
        (entry (counsel-keepassxc--entry-get candidate)))
    (with-current-buffer
        buffer (insert
                (format "Edit Keepassxc Entry.\n========================\nTitle: %s\nUserName: %s\nPassword: %s\nURL: %s\n"
                        (assoc-default "Title" entry nil "")
                        (assoc-default "UserName" entry nil "")
                        (assoc-default "Password" entry nil "")
                        (assoc-default "URL" entry nil "")))
        (forward-line -4)
        (goto-char (point-at-eol))
        (counsel-keepassxc-entry-mode)
        (set (make-local-variable 'keepassxc-candidate) candidate)
        (set (make-local-variable 'keepassxc-action) "edit"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--add (candidate)
  "Add entry, CANDIDATE is useless."
  (let ((buffer (generate-new-buffer "*keepassxc-add*")))
    (with-current-buffer buffer (insert
                                 "Add Keepassxc Entry.\n========================\nTitle: \nUserName: \nPassword: Generate10\nURL: \n")
                         (forward-line -4)
                         (goto-char (point-at-eol))
                         (counsel-keepassxc-entry-mode)
                         (set (make-local-variable 'keepassxc-candidate) candidate)
                         (set (make-local-variable 'keepassxc-action) "add"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--clone (candidate)
  "Clone entry, CANDIDATE is useless."
  (let ((buffer (generate-new-buffer "*keepassxc-clone*"))
        (entry (counsel-keepassxc--entry-get candidate)))
    (with-current-buffer buffer
      (insert (format
               "Clone Keepassxc Entry.\n========================\nTitle: %s\nUserName: %s\nPassword: %s\nURL: %s\n"
               (assoc-default "Title" entry nil "")
               (assoc-default "UserName" entry nil "")
               (assoc-default "Password" entry nil "")
               (assoc-default "URL" entry nil "")))
      (forward-line -4)
      (goto-char (point-at-eol))
      (counsel-keepassxc-entry-mode)
      (set (make-local-variable 'keepassxc-candidate) candidate)
      (set (make-local-variable 'keepassxc-action) "add"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--delete (candidate)
  "Delete entry, CANDIDATE is the entry to delete."
  (with-temp-buffer
    (insert (cadr candidate))
    (let ((args (list
                 (point-min)
                 (point-max)
                 "keepassxc-cli"
                 t t nil
                 "rm"
                 (expand-file-name counsel-keepassxc-database-file)
                 (car candidate))))
      (if (not (eq 0 (apply 'call-process-region args)))
          (error
           "Error: execute keepassxc-cli delete failed")
        (message "keepassxc-cli delete entry \"%s\" succeed" (car candidate))))))

(ivy-set-actions 'counsel-keepassxc '(("u" counsel-keepassxc--copy-username "copy username")
                                      ("p" counsel-keepassxc--copy-password "copy password")
                                      ("l" counsel-keepassxc--copy-url "copy url")
                                      ("n" counsel-keepassxc--copy-notes "copy notes")
                                      ("a" counsel-keepassxc--add "add entry")
                                      ("c" counsel-keepassxc--clone "clone entry")
                                      ("e" counsel-keepassxc--edit "edit entry")
                                      ("d" counsel-keepassxc--delete "delete entry")))

;;;###autoload
(defun counsel-keepassxc ()
  "Complete keepassxc password with Ivy."
  (interactive)
  (let ((master-password (read-passwd (format "Master password for %s: "
                                              counsel-keepassxc-database-file))))
    (ivy-read "keepassxc: " (counsel-keepassxc--candidates master-password)
              :history 'counsel-keepassxc-history
              :action #'counsel-keepassxc--view
              :caller 'counsel-keepassxc
              :require-match t)))

(provide 'counsel-keepassxc)

;;; counsel-keepassxc.el ends here
