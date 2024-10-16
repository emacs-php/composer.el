;;; composer-list.el --- Interface for viewing and manipulating composer packages -*- lexical-binding: t -*-

;; Copyright (C) 2024  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 15 June 2024
;; Version: 0.2.0
;; Keywords: tools php dependency manager
;; Homepage: https://github.com/zonuexe/composer.el
;; Package-Requires: ((emacs "25.1") (seq "1.9") (php-runtime "0.1.0"))
;; License: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface for viewing and manipulating composer packages.

;;; Code:
(require 'tabulated-list)
(require 'composer)
(require 'php-project nil t)
(require 'package)

(defgroup composer-list nil
  "Interface to PHP Composer."
  :group 'composer-list
  :tag "Composer List"
  :prefix "composer-list-")

(defcustom composer-list-name-column-width 30
  "Column width for the Package name in the composer list."
  :type 'natnum
  :group 'composer-list)

(defcustom composer-list-version-column-width 14
  "Column width for the Package version in the composer list."
  :type 'natnum
  :group 'composer-list)

(defcustom composer-list-status-column-width 12
  "Column width for the Package status in the composer list."
  :type 'natnum
  :group 'composer-list)

(defvar composer-list--load-no-latest nil)

(define-derived-mode composer-list-mode tabulated-list-mode "Composer packages"
  "Major mode for Composer list packages."
  :interactive nil
  (setq-local buffer-stale-function
              (lambda (&optional _noconfirm) 'fast))
  (setq tabulated-list-format
        `[("Package" ,composer-list-name-column-width t)
          ("Version" ,composer-list-version-column-width t)
          ("Latest" ,composer-list-version-column-width t)
          ("Status"  ,composer-list-status-column-width  t)
          ("Description" 0 package-menu--description-predicate)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'composer-list--refresh-packages nil t)
  (tabulated-list-init-header)
  (let ((composer-list--load-no-latest t))
    (composer-list--refresh-packages))
  (tabulated-list-print))

(defun composer-list--load-packages ()
  "List `composer' sub commands."
  (let ((output (if composer-list--load-no-latest
                    (composer--command-execute "show" "--locked" "--format=json")
                  (composer--command-execute "show" "--latest" "--all" "--format=json"))))
    (cdr-safe (assq 'locked (composer--parse-json-string output)))))

(defun composer-list--ensure-bool (value)
  "Ensure that the provided VALUE is a boolean.
If VALUE is :false or nil, return nil.
If VALUE is :true, return t.
Otherwise, if VALUE is already a boolean, return it as is."
  (cond
   ((or (eq :false value) (null value)) nil)
   ((eq :true value) t)
   ((booleanp value) value)))

(defun composer-list--ensure-string (value)
  "Ensure that the provided VALUE is a string.
If VALUE is :null or NIL, return an empty string \"\".
If VALUE is already a string, return it as is.
If VALUE is a number, convert it to a string using `number-to-string'."
  (cond
   ((or (eq :null value) (null value)) "")
   ((stringp value) value)
   ((numberp value) (number-to-string value))))

(defun composer-list--print-info (pkg)
  "Return a PKG package entry suitable for `tabulated-list-entries'."
  (let* ((latest-status (alist-get 'latest-status pkg))
         (face (pcase latest-status
                 ("update-possible" 'error)
                 ("semver-safe-update" 'warning)
                 ("up-to-date" 'success)))
         (name (alist-get 'name pkg))
         (version (alist-get 'version pkg))
         (direct (composer-list--ensure-bool (alist-get 'direct-dependency pkg)))
         (latest (or (alist-get 'latest pkg) ""))
         (warning (composer-list--ensure-bool (alist-get 'warning pkg)))
         (abandoned (alist-get 'warning pkg))
         (status (cond (abandoned "abandoned")
                       ((not direct) "dependency")
                       ("installed")))
         (desc (composer-list--ensure-string (alist-get 'description pkg)))
         (entry `[(,name
                    face link
                    font-lock-face link
                    follow-link t
                    package-desc ,name
                    action composer-list-describe-package)
                  ,version
                  ,(if face (propertize latest 'font-lock-face face) latest)
                  ,(if warning (propertize status 'font-lock-face 'error) status)
                  ,desc]))
    (list name entry)))

(defun composer-list--refresh-packages ()
  "Setup for `tabulated-list-format'."
  (let ((packages (composer-list--load-packages)))
    (tabulated-list-init-header)
    (setq tabulated-list-entries (seq-map #'composer-list--print-info packages))))

(define-derived-mode composer-list-describe-mode text-mode "Composer-pkg"
  "Major mode for viewing PsySH Doc."
  (setq show-trailing-whitespace nil)
  (goto-address-mode +1)
  (read-only-mode +1))

(defun composer-list-describe-package (package)
  "Display the full information of PACKAGE."
  (interactive (list (or (tabulated-list-get-id)
                         (completing-read
                          "Composer package: "
                          (let ((composer-use-ansi-color nil))
                            (split-string (composer--command-execute "show" "--name-only")))))))
  (let* ((buf (get-buffer-create "*Composer-pkg*"))
         (composer-use-ansi-color t)
         (command (composer--make-command-string "show" (list package "--ansi"))))
    (with-current-buffer buf
      (composer-list-describe-mode)
      (let ((default-directory (composer--find-composer-root default-directory))
            (buffer-read-only nil)
            (composer--quote-shell-argument t)
            pos)
        (erase-buffer)
        (insert command "\n\n")
        (setq pos (point))
        (shell-command command (current-buffer))
        (ansi-color-apply-on-region pos (point-max))))
    (pop-to-buffer-same-window buf)))

;;;###autoload
(defun composer-list-packages (directory)
  "Display a list of packages in DIRECTORY."
  (interactive
   (list (read-directory-name "Composer Directory: " (composer--find-composer-root default-directory))))
  (let* ((default-directory (composer--find-composer-root directory))
         (buf (get-buffer-create (format "*Composer: %s*" default-directory))))
    (with-current-buffer buf
      (composer-list-mode))
    (pop-to-buffer-same-window buf)))

(provide 'composer-list)
;;; composer-list.el ends here
