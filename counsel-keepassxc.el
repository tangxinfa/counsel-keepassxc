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
(defvar counsel-keepassxc-database-file nil
  "Keepassxc password database file.")

(defun counsel-keepassxc--candidates (master-password search-keywords)
  "Return list of `counsel-keepassxc' candidates.
MASTER-PASSWORD is required to open database, SEARCH-KEYWORDS for filter entries."
  (unless counsel-keepassxc-database-file
    (signal
     'file-error
     (list "Opening `counsel-keepassxc-database-file'" "No such readable file"
           counsel-keepassxc-database-file)))
  (let* ((entries (with-temp-buffer (insert master-password)
                                    (call-process-region (point-min)
                                                         (point-max) "keepassxc-cli" t t nil
                                                         "locate" (expand-file-name
                                                                   counsel-keepassxc-database-file)
                                                         search-keywords)
                                    (split-string (buffer-string) "\n")))
         (candidates (remove nil (mapcar (lambda (entry)
                                           (unless (string-prefix-p "Insert password to unlock"
                                                                    entry)
                                             (list entry master-password))) entries)))) candidates))

(defun counsel-keepassxc--get-field (candidate field)
  "Get FIELD of CANDIDATE."
  (let ((rows (remove nil (with-temp-buffer (insert (cadr candidate))
                                            (call-process-region (point-min)
                                                                 (point-max) "keepassxc-cli" t t nil
                                                                 "show" (expand-file-name
                                                                         counsel-keepassxc-database-file)
                                                                 (car candidate))
                                            (split-string (buffer-string) "\n" t))))
        (value nil)
        (prefix (concat field ": ")))
    (mapcar (lambda (row)
              (when (and (not (string-prefix-p "Insert password to unlock" row))
                         (string-prefix-p prefix row))
                (setq value (trim-string (substring row (length prefix)))))) rows) value))

(defun counsel-keepassxc--insert-password (candidate)
  "Insert password of CANDIDATE into current buffer."
  (insert (counsel-keepassxc--get-field candidate "Password")))

(defun counsel-keepassxc--insert-username (candidate)
  "Insert username of CANDIDATE into current buffer."
  (insert (counsel-keepassxc--get-field candidate "UserName")))

(defun counsel-keepassxc--insert-url (candidate)
  "Insert url of CANDIDATE into current buffer."
  (insert (counsel-keepassxc--get-field candidate "URL")))

(defun counsel-keepassxc--insert-title (candidate)
  "Insert title of CANDIDATE into current buffer."
  (insert (counsel-keepassxc--get-field candidate "Title")))

(defun counsel-keepassxc--insert-notes (candidate)
  "Insert notes of CANDIDATE into current buffer."
  (insert (counsel-keepassxc--get-field candidate "Notes")))

(ivy-set-actions 'counsel-keepassxc '(("p" counsel-keepassxc--insert-password "insert password")
                                      ("u" counsel-keepassxc--insert-username "insert username")
                                      ("l" counsel-keepassxc--insert-url "insert url")
                                      ("t" counsel-keepassxc--insert-title "insert title")
                                      ("n" counsel-keepassxc--insert-notes "insert notes")))

;;;###autoload
(defun counsel-keepassxc (search-keywords)
  "Complete keepassxc password with Ivy.
SEARCH-KEYWORDS for filter entries."
  (interactive "MSearch keepassxc: ")
  (let ((master-password (read-passwd (format "Master password for %s: "
                                              counsel-keepassxc-database-file))))
    (ivy-read "keepassxc: " (counsel-keepassxc--candidates master-password search-keywords)
              :history 'counsel-keepassxc-history
              :action #'counsel-keepassxc--insert-password
              :caller 'counsel-keepassxc
              :require-match t)))

(provide 'counsel-keepassxc)

;;; counsel-keepassxc.el ends here
