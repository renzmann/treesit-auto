;;; treesit-auto.el --- Automatically use tree-sitter enhacned modes, if available  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: treesitter auto automatic major mode fallback
;; URL: https://github.com/renzmann/treesit-auto.git
;; Version: 0.1.4
;; Package-Requires: ((emacs "29.0"))

;; This file is not part of GNU Emacs.

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
;; If a tree-sitter grammer is available and installed, use it instead of the
;; corresponding default mode.  Conversely, when a tree-sitter grammar is not
;; available and a fallback major mode is available/specified, use it instead.

;;; Code:
(require 'treesit)

(defcustom treesit-auto-fallback-alist
  (mapcar
   (lambda (elt)
     (cons (purecopy (car elt)) (cdr elt)))
   `((toml-ts-mode . conf-toml-mode)
     ;; TODO: I do not know if the future treesitter mode for HTML will be called html-ts-mode or mhtml-mode
     (html-ts-mode . mhtml-mode)
     ;; See deprecation note in their README: https://github.com/emacs-typescript/typescript.el#a-short-note-on-development-halt
     (typescript-ts-mode . nil)
     (tsx-ts-mode . nil)))
  "Alist mapping treesitter modes to their respective fallback modes.
If the CDR of the association is nil, then no fallback will be
attempted when encountering a tree-sitter mode that is missing an
installation of its respecitve grammar.  If the CDR is non-nil,
then a fallback attempt is made to the specified mode.

If a treesitter mode is omitted from the keys of this alist
entirely, then a fallback is attempted by using the same name
prefix.  For example, `python-ts-mode' will attempt a fallback to
`python-mode'.

In any case, if the fallback mode does not exist, then no
fallback is attempted.  One example of this would be for
`go-mod-mode', which would be the automatic fallback for
`go-mod-ts-mode'.  If `go-mod-mode' isn't installed, then Emacs
will still use its default behavior of using `go-mod-ts-mode',
regardless of whether the grammar is installed or not."
  :type '(alist (symbol) (function))
  :group 'treesit)

(defvar treesit-auto--language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  "Default repository URLs for `treesit-install-language-grammar'.")

(defun treesit-auto--remap-language-source (language-source)
  "Determine mode for LANGUAGE-SOURCE.
If the grammar is installed, remap the base mode to its
tree-sitter variant in `major-mode-remap-alist'.  Otherwise,
remap the tree-sitter variant back to the default mode."
  (let* ((name (car language-source))
         (name-ts-mode (intern (concat (symbol-name name) "-ts-mode")))
         (fallback-assoc (assq name-ts-mode treesit-auto-fallback-alist))
         (fallback-name (cdr fallback-assoc))
         (name-mode (or fallback-name
                        (intern (concat (symbol-name name) "-mode"))))
         (name-mode-bound-p (fboundp name-mode))
         (skip-remap-p (and fallback-assoc
                            (not (cdr fallback-assoc)))))
    (and (not skip-remap-p)
         (fboundp name-ts-mode)
         (if (treesit-ready-p name t)
             (add-to-list 'major-mode-remap-alist `(,name-mode . ,name-ts-mode))
           (when name-mode-bound-p
             (add-to-list 'major-mode-remap-alist `(,name-ts-mode . ,name-mode)))))))

(defun treesit-auto-apply-remap ()
  "Adjust `major-mode-remap-alist' using installed tree-sitter grammars."
  (dolist (elt treesit-auto--language-source-alist)
    (add-to-list 'treesit-language-source-alist elt t))
  (mapcar 'treesit-auto--remap-language-source treesit-language-source-alist))

(provide 'treesit-auto)
;;; treesit-auto.el ends here
