;;; treesit-auto.el --- Automatically use tree-sitter enhanced major modes when available  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: treesitter auto automatic major mode fallback convenience
;; URL: https://github.com/renzmann/treesit-auto.git
;; Version: 0.3.0
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
;; If a tree-sitter grammar is available and installed, use it instead of the
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
  "Alist mapping tree-sitter modes to their respective fallback modes.

If the CDR of the association is nil, then no fallback will be
attempted when encountering a tree-sitter mode that is missing an
installation of its respective grammar.  If the CDR is non-nil,
then a fallback attempt is made to the specified mode.

If a tree-sitter mode is omitted from the keys of this alist
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

(defcustom treesit-auto-install nil
  "If non-nil auto install the missing grammar for the current `ts-mode'.

If set to `prompt' treesit-auto will confirm with the user before
downloading and installing the grammar."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Ask" prompt))
  :group 'treesit)

(defvar treesit-auto--language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (clojure "https://github.com/sogaiu/tree-sitter-clojure")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (css-in-js "https://github.com/orzechowskid/tree-sitter-css-in-js")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (java "https://github.com/tree-sitter/tree-sitter-java")
    (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (latex "https://github.com/latex-lsp/tree-sitter-latex")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  "Default repository URLs for `treesit-install-language-grammar'.")

(defun treesit-auto--available-modes ()
  "Build a list of all modes ending with `-ts-mode' as strings."
  (let ((result '()))
    (mapatoms (lambda (elt)
                (when-let* ((is-func (functionp elt))
                            (name (symbol-name elt))
                            (match (string-match "-ts-mode$" name))
                            (no-internals (not (string-match "--" name))))
                  (push (intern name) result))))
    result))

(defun treesit-auto--extract-lang (name-ts-mode)
  "Get language from the first component of NAME-TS-MODE."
  (replace-regexp-in-string "\\(.*\\)-ts-mode$" "\\1" name-ts-mode))

(defun treesit-auto--string-convert-ts-name (name-ts-mode)
  "Convert NAME-TS-MODE, a string, to `name-mode', a symbol."
  (intern (concat (treesit-auto--extract-lang name-ts-mode) "-mode")))

(defun treesit-auto--get-assoc (ts-name)
  "Build a cons like (`name-ts-mode' . `name-mode') based on TS-NAME."
  (or (assq ts-name treesit-auto-fallback-alist)
      (when-let (fallback-assoc (rassq ts-name treesit-auto-fallback-alist))
        ;; Reverse order so that ts-mode comes first
        `(,(cdr fallback-assoc) . ,(car fallback-assoc)))
      `(,ts-name .  ,(treesit-auto--string-convert-ts-name (symbol-name ts-name)))))

(defun treesit-auto--available-alist ()
  "Build an alist of available tree-sitter modes paired with original modes."
  (mapcar 'treesit-auto--get-assoc (treesit-auto--available-modes)))

(defun treesit-auto--lang (mode)
  "Determine the tree-sitter language symbol for MODE."
  (let* ((available (treesit-auto--available-alist))
         (ts-mode (or (car (rassq mode available))
                      (car (assq mode available))))
         (ts-name (symbol-name ts-mode)))
    (intern (treesit-auto--extract-lang ts-name))))

(defun treesit-auto--ready-p (mode)
  "Determine if MODE is tree-sitter ready.

MODE can be either of the form `name-ts-mode' or its associated
original mode, such as `name-mode'."
  (treesit-ready-p (treesit-auto--lang mode) t))

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

(defun treesit-auto--prompt-to-install-package (lang)
  "Ask the user if they want to install a tree-sitter grammar for `LANG'.

Returns `non-nil' if install was completed without error."
  (let ((repo (alist-get lang treesit-language-source-alist)))
    (when (cond ((eq t treesit-auto-install) t)
                ((eq 'prompt treesit-auto-install)
                 (yes-or-no-p (format "Tree-sitter grammar for %s is missing.  Would you like to install it from %s? "
                                      (symbol-name lang)
                                      (car repo)))))
      (message "Installing the tree-sitter grammar for %s" lang)
      ;; treesit-install-language-grammar will return nil if the
      ;; operation succeeded and 't if a warning was sent to the
      ;; warning buffer. I don't think this is by design but just
      ;; because of the way `display-warning' works, so this might not
      ;; work in the future.
      (not (treesit-install-language-grammar lang)))))

(defun treesit-auto--maybe-install-grammar ()
  "Try to install the grammar matching the current major-mode.

If the tree-sitter grammar is missing for the current major mode,
it will prompt the user if they want to install it from the
currently registered repository.  If the user chooses to install
the grammar it will then switch to the tree-sitter powered
version of the current major-mode."
  (when-let* ((not-ready (not (treesit-auto--ready-p major-mode)))
              (lang (treesit-auto--lang major-mode))
              (install-success (treesit-auto--prompt-to-install-package lang)))
    (funcall (intern (concat (symbol-name lang) "-ts-mode")))))

;;;###autoload
(defun treesit-auto-apply-remap ()
  "Adjust `major-mode-remap-alist' using installed tree-sitter grammars."
  (dolist (elt treesit-auto--language-source-alist)
    (add-to-list 'treesit-language-source-alist elt t))
  (mapcar #'treesit-auto--remap-language-source treesit-language-source-alist))

(defun treesit-auto--install-language-grammar-wrapper (&rest _r)
  "Run `treesit-auto-apply-remap' after `treesit-install-language-grammar'."
  (treesit-auto-apply-remap))

;;;###autoload
(define-minor-mode global-treesit-auto-mode
  "Toggle `global-treesit-auto-mode'."
  :group 'treesit
  :global 't
  (if global-treesit-auto-mode
      (progn
        (dolist (elt (flatten-tree (treesit-auto--available-alist)))
          (add-hook (intern (concat (symbol-name elt) "-hook")) #'treesit-auto--maybe-install-grammar))
        (advice-add 'treesit-install-language-grammar
		    :after #'treesit-auto--install-language-grammar-wrapper)
        (treesit-auto-apply-remap))
    (remove-hook 'prog-mode-hook #'treesit-auto--maybe-install-grammar)
    (dolist (elt (flatten-tree (treesit-auto--available-alist)))
      (remove-hook (intern (concat (symbol-name elt) "-hook")) #'treesit-auto--maybe-install-grammar))
    (advice-remove 'treesit-install-language-grammar #'treesit-auto--install-language-grammar-wrapper)))

(provide 'treesit-auto)
;;; treesit-auto.el ends here
