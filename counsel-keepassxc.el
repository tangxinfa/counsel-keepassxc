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
(require 'password-cache)

;;** `counsel-keepassxc'
(defcustom counsel-keepassxc-cache-expiry 60
  "How many seconds to cache Keepassxc database password for.
NIL to disable expiry."
  :type '(choice (const :tag "Never" nil)
          (const :tag "All Day" 86400)
          (const :tag "2 Hours" 7200)
          (const :tag "30 Minutes" 1800)
          (integer :tag "Seconds")))

(defvar counsel-keepassxc-database-file nil "Keepassxc password database file.")

(defvar counsel-keepassxc--verbose nil "Output detailed error message for debug purpose.")

(defun counsel-keepassxc--error-message ()
  "Return error message when execute keepassxc-cli failed."
  (require 'cl-seq)
  (let ((output (string-trim (buffer-string))))
    (if (cl-search "Invalid credentials were provided" output)
        (password-cache-remove counsel-keepassxc-database-file))
    (if counsel-keepassxc--verbose
        (format "failed: %s" output)
      "failed")))

(defun counsel-keepassxc--candidates ()
  "Return list of keepassxc entries."
  (unless counsel-keepassxc-database-file
    (signal
     'file-error
     (list "Opening `counsel-keepassxc-database-file'" "No such readable file"
           counsel-keepassxc-database-file)))
  (let* ((args)
         (titles
          (with-temp-buffer
            (insert (counsel-keepassxc--read-password))
            (setq args (list (point-min)
                             (point-max)
                             "keepassxc-cli"
                             t t nil
                             "search"
                             (expand-file-name counsel-keepassxc-database-file)
                             ""))
            (if (not (eq 0 (apply 'call-process-region args)))
                (error
                 "Error: execute keepassxc-cli search %s" (counsel-keepassxc--error-message)))
            (split-string (buffer-string) "\n")))
         (candidates
          (remove nil
                  (mapcar
                   (lambda (title)
                     (unless (string-prefix-p "Enter password to unlock"
                                              title)
                       title))
                   titles))))
    candidates))

(defun counsel-keepassxc--entry-parse (&optional title)
  "Parse entry in current buffer with preferred TITLE."
  (save-excursion
    (goto-char (point-min))
    (let ((entry)
          (fields)
          (filters '("UserName" "Password" "URL" "Notes")))
      (if title
          (add-to-list 'entry (cons "Title" title))
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

(defun counsel-keepassxc--entry-get (title)
  "Get entry by TITLE."
  (with-temp-buffer
    (insert (counsel-keepassxc--read-password))
    (let* ((args (list (point-min)
                       (point-max)
                       "keepassxc-cli"
                       t t nil
                       "show"
                       "--show-protected"
                       (expand-file-name
                        counsel-keepassxc-database-file)
                       title)))
      (if (not (eq 0 (apply 'call-process-region args)))
          (error
           "Error: execute keepassxc-cli show %s" (counsel-keepassxc--error-message)))
      (counsel-keepassxc--entry-parse title))))

(defun counsel-keepassxc--copy-password (title)
  "Copy password of entry identified by TITLE"
  (kill-new (assoc-default "Password" (counsel-keepassxc--entry-get title) nil "")))

(defun counsel-keepassxc--copy-username (title)
  "Copy username of entry identified by TITLE."
  (kill-new (assoc-default "UserName" (counsel-keepassxc--entry-get title) nil "")))

(defun counsel-keepassxc--copy-url (title)
  "Copy url of entry identified by TITLE."
  (kill-new (assoc-default "URL" (counsel-keepassxc--entry-get title) nil "")))

(defun counsel-keepassxc--copy-notes (title)
  "Copy notes of entry identified by TITLE."
  (kill-new (assoc-default "Notes" (counsel-keepassxc--entry-get title) nil "")))

(defun counsel-keepassxc--copy-totp (title)
  "Copy TOTP of entry identified by TITLE."
  (kill-new
   (with-temp-buffer
     (insert (counsel-keepassxc--read-password))
     (let* ((args (list (point-min)
                        (point-max)
                        "keepassxc-cli"
                        t t nil
                        "show"
                        "--totp"
                        (expand-file-name
                         counsel-keepassxc-database-file)
                        title)))
       (if (not (eq 0 (apply 'call-process-region args)))
           (error
            "Error: execute keepassxc-cli show %s" (counsel-keepassxc--error-message)))
       (save-excursion
         (goto-char (point-min))
         (beginning-of-line 2)
         (buffer-substring
          (point)
          (point-at-eol)))))))

(defun counsel-keepassxc--entry-commit ()
  "Commit added or edited entry."
  (interactive)
  (let* ((entry (counsel-keepassxc--entry-parse))
         (entry-buffer (current-buffer))
         (generates (split-string (assoc-default "Password" entry nil "") "Generate"))
         (args nil)
         (action (buffer-local-value 'keepassxc-action entry-buffer))
         (title (buffer-local-value 'keepassxc-candidate entry-buffer))
         (delete-old nil))
    (unless (or (string= action "edit")
                (string= action "add"))
      (error
       "Error: commit not allowed when %s keepassxc entry"
       action))
    ;;;; If title changed when editing, treat as add a new entry and delete old entry.
    (when (and (string= action "edit")
               (not (string= (string-trim-left title "/")
                             (string-trim-left (assoc-default "Title" entry nil "") "/"))))
      (setq action "add")
      (setq delete-old t))
    (with-temp-buffer
      (insert (counsel-keepassxc--read-password))
      (insert "\n")
      (when (< (length generates) 2)
        (insert (assoc-default "Password" entry nil ""))
        (insert "\n"))
      (setq args
            (list
             (point-min)
             (point-max)
             "keepassxc-cli"
             t t nil
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
          (add-to-list 'args "-L" t)
          (add-to-list 'args (second generates) t)))
      (if (not (eq 0 (apply 'call-process-region args)))
          (error
           "Error: execute keepassxc-cli %s %s"
           action (counsel-keepassxc--error-message)))
      (when delete-old (counsel-keepassxc--delete title))
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
  (let* ((entry-buffer (current-buffer))
         (action (buffer-local-value 'keepassxc-action entry-buffer))
         (title (buffer-local-value 'keepassxc-candidate entry-buffer))
         (position (point)))
    (unless (string= action "view")
      (error
       "Error: edit not allowed when %s keepassxc entry"
       action))
    (kill-buffer (current-buffer))
    (counsel-keepassxc--edit title)
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
(define-key counsel-keepassxc-entry-mode-map (kbd "C-c C-a") 'counsel-keepassxc--add)
(define-key counsel-keepassxc-entry-mode-map (kbd "C-c C-c") 'counsel-keepassxc--entry-commit)
(define-key counsel-keepassxc-entry-mode-map (kbd "C-c C-e") 'counsel-keepassxc--entry-edit)
(define-key counsel-keepassxc-entry-mode-map (kbd "C-c C-k") 'counsel-keepassxc--entry-abort)
(define-key counsel-keepassxc-entry-mode-map (kbd "TAB") 'counsel-keepassxc--entry-next-field)

(define-derived-mode counsel-keepassxc-entry-mode text-mode
  "counsel-keepassxc-entry-mode"
  "major mode for editing keepassxc entry."
  (setq font-lock-defaults '(counsel-keepassxc-entry-highlights)))

(defun counsel-keepassxc--view (title)
  "View entry identified by TITLE."
  (let ((buffer (generate-new-buffer "*keepassxc-view*"))
        (entry (counsel-keepassxc--entry-get title)))
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
      (set (make-local-variable 'keepassxc-candidate) title)
      (set (make-local-variable 'keepassxc-action) "view"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--edit (title)
  "Edit entry identified by TITLE."
  (let ((buffer (generate-new-buffer "*keepassxc-edit*"))
        (entry (counsel-keepassxc--entry-get title)))
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
        (set (make-local-variable 'keepassxc-candidate) title)
        (set (make-local-variable 'keepassxc-action) "edit"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--add (&rest _)
  "Add entry."
  (let ((buffer (generate-new-buffer "*keepassxc-add*")))
    (with-current-buffer buffer (insert
                                 "Add Keepassxc Entry.\n========================\nTitle: \nUserName: \nPassword: Generate10\nURL: \n")
                         (forward-line -4)
                         (goto-char (point-at-eol))
                         (counsel-keepassxc-entry-mode)
                         (set (make-local-variable 'keepassxc-candidate) nil)
                         (set (make-local-variable 'keepassxc-action) "add"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--clone (title)
  "Clone entry identified by TITLE."
  (let ((buffer (generate-new-buffer "*keepassxc-clone*"))
        (entry (counsel-keepassxc--entry-get title)))
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
      (set (make-local-variable 'keepassxc-candidate) title)
      (set (make-local-variable 'keepassxc-action) "add"))
    (switch-to-buffer buffer)))

(defun counsel-keepassxc--delete (title)
  "Delete entry identified by TITLE."
  (with-temp-buffer
    (insert (counsel-keepassxc--read-password))
    (let ((args (list
                 (point-min)
                 (point-max)
                 "keepassxc-cli"
                 t t nil
                 "rm"
                 (expand-file-name counsel-keepassxc-database-file)
                 title)))
      (if (not (eq 0 (apply 'call-process-region args)))
          (error
           "Error: execute keepassxc-cli delete %s" (counsel-keepassxc--error-message))
        (message "keepassxc-cli delete entry \"%s\" succeed" title)))))

(ivy-set-actions 'counsel-keepassxc '(("u" counsel-keepassxc--copy-username "copy username")
                                      ("p" counsel-keepassxc--copy-password "copy password")
                                      ("l" counsel-keepassxc--copy-url "copy url")
                                      ("n" counsel-keepassxc--copy-notes "copy notes")
                                      ("t" counsel-keepassxc--copy-totp "copy TOTP")
                                      ("a" counsel-keepassxc--add "add entry")
                                      ("c" counsel-keepassxc--clone "clone entry")
                                      ("e" counsel-keepassxc--edit "edit entry")
                                      ("d" counsel-keepassxc--delete "delete entry")))

(defun counsel-keepassxc--read-password ()
  "Read master password."
  (let* ((password-prompt (format "Master password for %s: " counsel-keepassxc-database-file))
         (password-cache-expiry counsel-keepassxc-cache-expiry)
         (password
          (cond
           ((password-read-from-cache counsel-keepassxc-database-file))
           ((password-read password-prompt counsel-keepassxc-database-file)))))
    (password-cache-add counsel-keepassxc-database-file password)
    password))

(defun counsel-keepassxc-get-password (title)
  "Get password of entry identified by TITLE."
  (assoc-default
   "Password"
   (counsel-keepassxc--entry-get title)))

(defun counsel-keepassxc-get-username (title)
  "Get username of entry identified by TITLE."
  (assoc-default
   "UserName"
   (counsel-keepassxc--entry-get title)))

(defun counsel-keepassxc-insert-password ()
  "Insert password of entry."
  (interactive)
  (let ((counsel-keepassxc-default-action #'(lambda (&rest _))))
    (insert (counsel-keepassxc-get-password (counsel-keepassxc)))))

(defvar counsel-keepassxc-default-action #'counsel-keepassxc--view "`counsel-keepassxc' default action.")

;;;###autoload
(defun counsel-keepassxc ()
  "Complete keepassxc password with Ivy."
  (interactive)
  (ivy-read "keepassxc: "
            (counsel-keepassxc--candidates)
            :history 'counsel-keepassxc-history
            :action counsel-keepassxc-default-action
            :caller 'counsel-keepassxc
            :require-match t))

(provide 'counsel-keepassxc)

;;; counsel-keepassxc.el ends here
