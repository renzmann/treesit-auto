;;; treesit-auto.el --- Automatically use tree-sitter enhanced major modes -*- lexical-binding: t -*-

;; Copyright (C) 2023 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: treesitter auto automatic major mode fallback convenience
;; URL: https://github.com/renzmann/treesit-auto.git
;; Version: 0.4.2
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
(require 'cl-lib)

(defcustom treesit-auto-install nil
  "If non-nil, auto install the missing tree-sitter grammars.

This variable takes affect whenever visiting a file that has a
tree-sitter version available or when using
`treesit-auto-install-all'.  If set to `prompt' treesit-auto will
confirm with the user before downloading and installing the
grammar."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Ask" prompt))
  :group 'treesit)

(defcustom treesit-auto-fallback-alist nil
  "Ignored.

Formerly the method of defining fallback & promotion modes
between tree-sitter and original modes.  This is handled instead
by manipulating the `treesit-auto-recipe-list' variable."
  :type '(alist (symbol) (function))
  :group 'treesit)

(defcustom treesit-auto-langs nil
  "Language symbols that should be automatically installed.

Setting this to a list of grammar symbols will modify the
behavior of `treesit-auto-install-all' and the
automatic/prompting behavior when visiting a buffer that has a
tree-sitter mode available.  For example, when set to \\='(python
rust go), then `treesit-auto-install-all' will only check and
install those three grammars.  Likewise, we will only get
automatic installation (or prompting, based on the value of
`treesit-auto-install') when visiting a Python, Go, or Rust file."
  :type '(list (symbol))
  :group 'treesit)

(cl-defstruct treesit-auto-recipe
  "Emacs metadata for a tree-sitter language grammar."
  lang ts-mode remap url revision source-dir cc c++)

(defvar treesit-auto-recipe-list
  `(
    ,(make-treesit-auto-recipe
      :lang 'bash
      :ts-mode 'bash-ts-mode
      :remap 'sh-mode
      :url "https://github.com/tree-sitter/tree-sitter-bash")
    ,(make-treesit-auto-recipe
      :lang 'bibtex
      :ts-mode 'bibtex-ts-mode
      :remap 'bibtex-mode
      :url "https://github.com/latex-lsp/tree-sitter-bibtex")
    ,(make-treesit-auto-recipe
      :lang 'c
      :ts-mode 'c-ts-mode
      :remap 'c-mode
      :url "https://github.com/tree-sitter/tree-sitter-c")
    ,(make-treesit-auto-recipe
      :lang 'c-sharp
      :ts-mode 'csharp-ts-mode
      :remap 'csharp-mode
      :url "https://github.com/tree-sitter/tree-sitter-c-sharp")
    ,(make-treesit-auto-recipe
      :lang 'clojure
      :ts-mode 'clojure-ts-mode
      :remap 'clojure-mode
      :url "https://github.com/sogaiu/tree-sitter-clojure")
    ,(make-treesit-auto-recipe
      :lang 'cmake
      :ts-mode 'cmake-ts-mode
      :remap 'cmake-mode
      :url "https://github.com/uyha/tree-sitter-cmake")
    ,(make-treesit-auto-recipe
      :lang 'commonlisp
      :ts-mode 'commonlisp-ts-mode
      :remap 'common-lisp-mode
      :url "https://github.com/theHamsta/tree-sitter-commonlisp")
    ,(make-treesit-auto-recipe
      :lang 'cpp
      :ts-mode 'c++-ts-mode
      :remap 'c++-mode
      :url "https://github.com/tree-sitter/tree-sitter-cpp")
    ,(make-treesit-auto-recipe
      :lang 'css
      :ts-mode 'css-ts-mode
      :remap 'css-mode
      :url "https://github.com/tree-sitter/tree-sitter-css")
    ,(make-treesit-auto-recipe
      :lang 'dockerfile
      :ts-mode 'dockerfile-ts-mode
      :remap 'dockerfile-mode
      :url "https://github.com/camdencheek/tree-sitter-dockerfile")
    ,(make-treesit-auto-recipe
      :lang 'go
      :ts-mode 'go-ts-mode
      :remap 'go-mode
      :url "https://github.com/tree-sitter/tree-sitter-go")
    ,(make-treesit-auto-recipe
      :lang 'gomod
      :ts-mode 'go-mod-ts-mode
      :remap 'go-mod-mode
      :url "https://github.com/camdencheek/tree-sitter-go-mod")
    ,(make-treesit-auto-recipe
      :lang 'html
      :ts-mode 'html-ts-mode
      :remap '(mhtml-mode sgml-mode)
      :url "https://github.com/tree-sitter/tree-sitter-html")
    ,(make-treesit-auto-recipe
      :lang 'java
      :ts-mode 'java-ts-mode
      :remap 'java-mode
      :url "https://github.com/tree-sitter/tree-sitter-java")
    ,(make-treesit-auto-recipe
      :lang 'julia
      :ts-mode 'julia-ts-mode
      :remap 'julia-mode
      :url "https://github.com/tree-sitter/tree-sitter-julia")
    ,(make-treesit-auto-recipe
      :lang 'javascript
      :ts-mode 'js-ts-mode
      :remap '(js-mode javascript-mode js2-mode)
      :url "https://github.com/tree-sitter/tree-sitter-javascript"
      :revision "master"
      :source-dir "src")
    ,(make-treesit-auto-recipe
      :lang 'json
      :ts-mode 'json-ts-mode
      :remap 'js-json-mode
      :url "https://github.com/tree-sitter/tree-sitter-json")
    ,(make-treesit-auto-recipe
      :lang 'latex
      :ts-mode 'latex-ts-mode
      :remap 'latex-mode
      :url "https://github.com/latex-lsp/tree-sitter-latex")
    ,(make-treesit-auto-recipe
      :lang 'lua
      :ts-mode 'lua-ts-mode
      :remap 'lua-mode
      :url "https://github.com/Azganoth/tree-sitter-lua")
    ,(make-treesit-auto-recipe
      :lang 'make
      :ts-mode 'makefile-ts-mode
      :remap 'makefile-mode
      :url "https://github.com/alemuller/tree-sitter-make")
    ,(make-treesit-auto-recipe
      :lang 'markdown
      :ts-mode 'markdown-ts-mode
      :remap '(poly-markdown-mode markdown-mode)
      :url "https://github.com/ikatyang/tree-sitter-markdown")
    ,(make-treesit-auto-recipe
      :lang 'python
      :ts-mode 'python-ts-mode
      :remap 'python-mode
      :url "https://github.com/tree-sitter/tree-sitter-python")
    ,(make-treesit-auto-recipe
      :lang 'r
      :ts-mode 'r-ts-mode
      :remap 'ess-mode
      :url "https://github.com/r-lib/tree-sitter-r")
    ,(make-treesit-auto-recipe
      :lang 'ruby
      :ts-mode 'ruby-ts-mode
      :remap 'ruby-mode
      :url "https://github.com/tree-sitter/tree-sitter-ruby")
    ,(make-treesit-auto-recipe
      :lang 'rust
      :ts-mode 'rust-ts-mode
      :remap 'rust-mode
      :url "https://github.com/tree-sitter/tree-sitter-rust")
    ,(make-treesit-auto-recipe
      :lang 'toml
      :ts-mode 'toml-ts-mode
      :remap '(conf-toml-mode toml-mode)
      :url "https://github.com/tree-sitter/tree-sitter-toml")
    ,(make-treesit-auto-recipe
      :lang 'tsx
      :ts-mode 'tsx-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "tsx/src")
    ,(make-treesit-auto-recipe
      :lang 'typescript
      :ts-mode 'typescript-ts-mode
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "typescript/src")
    ,(make-treesit-auto-recipe
      :lang 'yaml
      :ts-mode 'yaml-ts-mode
      :remap 'yaml-mode
      :url "https://github.com/ikatyang/tree-sitter-yaml")
    )
    "Map each tree-sitter lang to Emacs metadata.")

(defvar treesit-auto--lang-recipe-alist
  nil
  "Lookup recipe by lang.")

(defvar treesit-auto--mode-lang-alist
  nil
  "Lookup lang by remap mode.")

(defvar treesit-auto--original-language-source-alist
  (purecopy treesit-language-source-alist)
  "Keep track of `treesit-language-source-alist'.")

(defvar treesit-auto--original-major-mode-remap-alist
  (purecopy major-mode-remap-alist)
  "Keep track of `major-mode-remap-alist'.")

(defun treesit-auto--build-alists ()
  "Rebuild internal alists from language recipes."
  (setq treesit-auto--lang-recipe-alist ())
  (setq treesit-auto--mode-lang-alist ())
  (dolist (recipe treesit-auto-recipe-list)
    (when-let* ((lang (treesit-auto-recipe-lang recipe))
                (ts-mode (treesit-auto-recipe-ts-mode recipe))
                (remap (ensure-list (treesit-auto-recipe-remap recipe)))
                (fallback (car remap)))
      ;; For lang -> Emacs metadata lookups
      (push `(,lang . ,recipe) treesit-auto--lang-recipe-alist)
      ;; For mode -> lang lookup
      (dolist (mode remap)
        (push `(,mode . ,lang) treesit-auto--mode-lang-alist))
      (push `(,ts-mode . ,lang) treesit-auto--mode-lang-alist)
      ;; Tree-sitter <--> fallback automation happens here
      (if (treesit-auto--ready-p ts-mode)
          (dolist (mode remap)
            (add-to-list 'major-mode-remap-alist `(,mode . ,ts-mode)))
        (when (fboundp fallback)
          (add-to-list 'major-mode-remap-alist `(,ts-mode . ,fallback))))
      ;; For `treesit-install-langauge-grammar'
      (add-to-list 'treesit-language-source-alist
                   `(,(treesit-auto-recipe-lang recipe)
                     . (,(treesit-auto-recipe-url recipe)
                        ,(treesit-auto-recipe-revision recipe)
                        ,(treesit-auto-recipe-source-dir recipe)
                        ,(treesit-auto-recipe-cc recipe)
                        ,(treesit-auto-recipe-c++ recipe)))))))

(defun treesit-auto--ready-p (mode)
  "Determine if MODE is tree-sitter ready.
MODE can be either the tree-sitter enhanced version or one of the
fallback modes."
  (let* ((lang (alist-get mode treesit-auto--mode-lang-alist))
         (recipe (alist-get lang treesit-auto--lang-recipe-alist))
         (ts-mode (treesit-auto-recipe-ts-mode recipe)))
    (and (treesit-ready-p lang t)
         (fboundp mode)
         (fboundp ts-mode))))

(defun treesit-auto--prompt-to-install-package (lang)
  "Ask the user if they want to install a tree-sitter grammar for `LANG'.

non-nil only if installation completed without any errors."
  (when (cond ((eq t treesit-auto-install) t)
              ((eq 'prompt treesit-auto-install)
               (y-or-n-p (format "Tree-sitter grammar for %s is missing.  Install it from %s? "
                                 (symbol-name lang)
                                 (car (alist-get lang treesit-language-source-alist))))))
    (message "Installing the tree-sitter grammar for %s" lang)
    ;; treesit-install-language-grammar will return nil if the
    ;; operation succeeded and 't if a warning was sent to the
    ;; warning buffer. I don't think this is by design but just
    ;; because of the way `display-warning' works, so this might not
    ;; work in the future.
    (not (treesit-install-language-grammar lang))))

(defun treesit-auto--maybe-install-grammar ()
  "Try to install the grammar matching the current major-mode.

If the tree-sitter grammar is missing for the current major mode,
it will prompt the user if they want to install it from the
currently registered repository.  If the user chooses to install
the grammar it will then switch to the tree-sitter powered
version of the current major-mode."
  (when-let* ((not-ready (not (treesit-auto--ready-p major-mode)))
              (lang (alist-get major-mode treesit-auto--mode-lang-alist))
              (recipe (alist-get lang treesit-auto--lang-recipe-alist))
              (ts-mode (treesit-auto-recipe-ts-mode recipe))
              (ts-mode-exists (fboundp ts-mode))
              (install-success (treesit-auto--prompt-to-install-package lang)))
    (funcall ts-mode)))

(defvar treesit-auto-opt-out-list
  nil
  "Deprecated: language symbols to avoid when using `treesit-auto-install-all'.")

;;;###autoload
(defun treesit-auto-install-all ()
  "Install every available, maintained grammar.

See `treesit-auto-langs' and `treesit-auto-install' for
how to modify the behavior of this function."
  (interactive)
  (when-let* ((to-install (or treesit-auto-langs
                              (seq-filter
                               (lambda (lang) (not (treesit-ready-p lang t)))
                               (cl-set-difference
                                (mapcar 'car treesit-language-source-alist)
                                treesit-auto-opt-out-list))))
              (prompt (format "The following tree-sitter grammars are missing:\n%s\n"
                              (mapconcat 'symbol-name to-install "\n"))))
    ;; TODO QOL - it would be nice if this messaged what was installed or at
    ;; least mentioned that nothing was installed if skipped.
    (with-output-to-temp-buffer "*Treesit-auto install candidates*"
      (princ prompt))
    (when (or (eq treesit-auto-install t) ; Quiet mode is off
              (y-or-n-p "Install missing grammars? "))
      (mapcar 'treesit-install-language-grammar to-install))))

(defun treesit-auto--install-language-grammar-wrapper (&rest _r)
  "Run `treesit-auto-apply-remap' after `treesit-install-language-grammar'."
  (treesit-auto--build-alists))

(defun treesit-auto--setup ()
  "Set up global minor mode."
  (setq treesit-auto--original-major-mode-remap-alist (purecopy major-mode-remap-alist))
  (setq treesit-auto--original-language-source-alist (purecopy treesit-language-source-alist))
  ;; TODO add maybe-install hook to all remap modes
  (treesit-auto--build-alists)
  (dolist (elt (mapcar 'car treesit-auto--mode-lang-alist))
    (add-hook (intern (concat (symbol-name elt) "-hook"))
              #'treesit-auto--maybe-install-grammar))
  (advice-add 'treesit-install-language-grammar
	      :after #'treesit-auto--install-language-grammar-wrapper))

(defun treesit-auto--teardown ()
  "Undo any change made by global minor mode."
  (advice-remove 'treesit-install-language-grammar
                 #'treesit-auto--install-language-grammar-wrapper)
  (dolist (elt (mapcar 'car treesit-auto--mode-lang-alist))
    (remove-hook (intern (concat (symbol-name elt) "-hook"))
                 #'treesit-auto--maybe-install-grammar))
  (setq major-mode-remap-alist (purecopy treesit-auto--original-major-mode-remap-alist))
  (setq treesit-language-source-alist (purecopy treesit-auto--original-language-source-alist)))

;;;###autoload
(define-minor-mode global-treesit-auto-mode
  "Toggle `global-treesit-auto-mode'."
  :group 'treesit
  :global 't
  (if global-treesit-auto-mode
      (treesit-auto--setup)
    (treesit-auto--teardown)))

(provide 'treesit-auto)
;;; treesit-auto.el ends here
