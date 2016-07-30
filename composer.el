;; composer.el --- Utillities for PHP composer -*- lexical-binding: t -*-

;; Copyright (C) 2015 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Dec 2015
;; Version: 0.0.1
;; Keywords: php dependency manager
;; Package-Requires: ((emacs "24") (s "1.9.0") (f "0.17") (request "0.2.0"))

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 's)
(require 'f)
(require 'request)

(defvar composer-executable-bin nil
  "Path to `composer.phar' exec file.")

(defun composer--find-executable ()
  "Return `composer' command name."
  (if (and composer-executable-bin (file-exists-p composer-executable-bin))
      composer-executable-bin
    (catch 'found
      (dolist (cmd '("composer.phar" "composer"))
        (when (executable-find cmd)
          (throw 'found cmd)))
      ;; ToDo: Returns project local binary file
      )))

(defun composer--find-composer-root (directory)
  "Return directory path which include composer.json by `DIRECTORY'."
  (let ((composer-json (f-join directory "composer.json")) parent)
    (if (file-exists-p composer-json)
        (concat (f-dirname composer-json) "/")
      (setq parent (f-dirname directory))
      (if (null parent)
          nil
        (composer--find-composer-root (f-dirname directory))))))

(defun composer-mode--composer-execute (&rest args)
  "Execute `composer.phar' command by ARGS."
  ;; You are running composer with xdebug enabled. This has a major impact on runtime performance. See https://getcomposer.org/xdebug
  (let ((default-directory (or (composer--find-composer-root default-directory)
                               default-directory)))
    (replace-regexp-in-string
     "^.+getcomposer.org/xdebug\n" ""
     (s-chomp
      (shell-command-to-string
       (mapconcat #'identity (cons (composer--find-executable) args) " "))))))

;;(composer-mode--composer-execute "require" "--dev" "phpunit/phpunit:^4.8")

(defun composer-install ()
  "Execute `composer.phar install' command."
  (interactive)
  (composer-mode--composer-execute "install"))

(defun composer-require (is-dev &optional package)
  "Execute `composer.phar require (--dev)' command."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (setq is-dev (not (eq is-dev 1)))
    (setq package (read-string
                   (if is-dev "Input package name(dev): " "Input package name: "))))
  (let ((args (list package)))
    (when is-dev (append "--dev"))
    (apply 'composer-mode--composer-execute "require" (nreverse args))))

(defun composer-self-update ()
  "Execute `composer.phar self-update' command."
  (interactive)
  (when (yes-or-no-p "Do composer self-update? ")
    (composer-mode--composer-execute "self-update")))

(define-derived-mode composer-mode json-mode "composer"
  "Major mode editing `composer.json'."
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("/composer\\.json'" . composer-mode))

(provide 'composer-mode)
;;; composer.el ends here
