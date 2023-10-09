;;; treesit-auto.el --- Automatically use tree-sitter enhanced major modes -*- lexical-binding: t -*-

;; Copyright (C) 2023 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: treesitter auto automatic major mode fallback convenience
;; URL: https://github.com/renzmann/treesit-auto.git
;; Version: 0.6.4
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
;;
;; This package also provides a `treesit-auto-install-all' function, which will
;; scan for tree-sitter grammars listed in `treesit-auto-recipe-list' that are
;; not installed or otherwise available on `treesit-extra-load-path'.  Automatic
;; installation of grammars when visiting a file is controlled by the
;; `treesit-auto-install' variable, which can be t, nil or `prompt'.  When t,
;; opening a file with a compatible tree-sitter mode will clone and install the
;; grammar defined by its recipe, if it isn't already installed.  `prompt' will
;; display a yes/no question in the minibuffer and wait for confirmation before
;; attempting the installation.

;;; Code:

(require 'treesit)
(eval-when-compile
  (require 'cl-lib)
  (require 'files))

(defcustom treesit-auto-install nil
  "If non-nil, auto install missing tree-sitter grammars.

This variable enables the automatic clone, compile, and
installation of tree-sitter grammars whenever visiting a file
that has a compatible tree-sitter mode.  If set to `prompt'
treesit-auto will ask for confirmation before downloading the
grammar.  Additionally, `treesit-auto-install-all' will skip the
yes/no prompt when this variable is t."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Ask" prompt))
  :group 'treesit)

(defcustom treesit-auto-fallback-alist nil
  "Ignored.

Formerly the method of defining fallback & promotion modes
between tree-sitter and original modes.  This is handled instead
by manipulating the `treesit-auto-recipe-list' variable."
  :type '(alist :key-type symbol :value-type function)
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
  :type '(repeat symbol)
  :group 'treesit)

(cl-defstruct treesit-auto-recipe
  "Emacs metadata for a tree-sitter language grammar."
  lang ts-mode remap requires url revision source-dir cc c++ ext)

(defvar treesit-auto-recipe-list
  `(,(make-treesit-auto-recipe
      :lang 'bash
      :ts-mode 'bash-ts-mode
      :remap 'sh-mode
      :url "https://github.com/tree-sitter/tree-sitter-bash"
      :ext "\\.sh\\'")
    ,(make-treesit-auto-recipe
      :lang 'bibtex
      :ts-mode 'bibtex-ts-mode
      :remap 'bibtex-mode
      :url "https://github.com/latex-lsp/tree-sitter-bibtex"
      :ext "\\.bib\\'")
    ,(make-treesit-auto-recipe
      :lang 'c
      :ts-mode 'c-ts-mode
      :remap 'c-mode
      :url "https://github.com/tree-sitter/tree-sitter-c"
      :ext "\\.c\\'")
    ,(make-treesit-auto-recipe
      :lang 'c-sharp
      :ts-mode 'csharp-ts-mode
      :remap 'csharp-mode
      :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
      :ext "\\.cs\\'")
    ,(make-treesit-auto-recipe
      :lang 'clojure
      :ts-mode 'clojure-ts-mode
      :remap 'clojure-mode
      :url "https://github.com/sogaiu/tree-sitter-clojure"
      :ext "\\.clj\\'")
    ,(make-treesit-auto-recipe
      :lang 'cmake
      :ts-mode 'cmake-ts-mode
      :remap 'cmake-mode
      :url "https://github.com/uyha/tree-sitter-cmake"
      :ext "\\.cmake\\'")
    ,(make-treesit-auto-recipe
      :lang 'commonlisp
      :ts-mode 'commonlisp-ts-mode
      :remap 'common-lisp-mode
      :url "https://github.com/theHamsta/tree-sitter-commonlisp"
      :ext "\\.cl\\'")
    ,(make-treesit-auto-recipe
      :lang 'cpp
      :ts-mode 'c++-ts-mode
      :remap 'c++-mode
      :url "https://github.com/tree-sitter/tree-sitter-cpp"
      :ext "\\.cpp\\'")
    ,(make-treesit-auto-recipe
      :lang 'css
      :ts-mode 'css-ts-mode
      :remap 'css-mode
      :url "https://github.com/tree-sitter/tree-sitter-css"
      :ext "\\.css\\'")
    ,(make-treesit-auto-recipe
      :lang 'dockerfile
      :ts-mode 'dockerfile-ts-mode
      :remap 'dockerfile-mode
      :url "https://github.com/camdencheek/tree-sitter-dockerfile"
      :ext "\\Dockerfile\\'")
    ,(make-treesit-auto-recipe
      :lang 'elixir
      :ts-mode 'elixir-ts-mode
      :remap 'elixir-mode
      :requires 'heex
      :url "https://github.com/elixir-lang/tree-sitter-elixir"
      :ext "\\.ex\\'")
    ,(make-treesit-auto-recipe
      :lang 'go
      :ts-mode 'go-ts-mode
      :remap 'go-mode
      :url "https://github.com/tree-sitter/tree-sitter-go"
      :ext "\\.go\\'")
    ,(make-treesit-auto-recipe
      :lang 'gomod
      :ts-mode 'go-mod-ts-mode
      :remap 'go-mod-mode
      :url "https://github.com/camdencheek/tree-sitter-go-mod"
      :ext "go\\.mod\\'")
    ,(make-treesit-auto-recipe
      :lang 'heex
      :ts-mode 'heex-ts-mode
      :remap 'heex-mode
      :url "https://github.com/phoenixframework/tree-sitter-heex"
      :ext "\\.heex\\'")
    ,(make-treesit-auto-recipe
      :lang 'html
      :ts-mode 'html-ts-mode
      :remap '(mhtml-mode sgml-mode)
      :url "https://github.com/tree-sitter/tree-sitter-html"
      :ext "\\.html\\'")
    ,(make-treesit-auto-recipe
      :lang 'java
      :ts-mode 'java-ts-mode
      :remap 'java-mode
      :url "https://github.com/tree-sitter/tree-sitter-java"
      :ext "\\.java\\'")
    ,(make-treesit-auto-recipe
      :lang 'javascript
      :ts-mode 'js-ts-mode
      :remap '(js-mode javascript-mode js2-mode)
      :url "https://github.com/tree-sitter/tree-sitter-javascript"
      :revision "master"
      :source-dir "src"
      :ext "\\.js\\'")
    ,(make-treesit-auto-recipe
      :lang 'json
      :ts-mode 'json-ts-mode
      :remap 'js-json-mode
      :url "https://github.com/tree-sitter/tree-sitter-json"
      :ext "\\.json\\'")
    ,(make-treesit-auto-recipe
      :lang 'julia
      :ts-mode 'julia-ts-mode
      :remap 'julia-mode
      :url "https://github.com/tree-sitter/tree-sitter-julia"
      :ext "\\.jl\\'")
    ,(make-treesit-auto-recipe
      :lang 'kotlin
      :ts-mode 'kotlin-ts-mode
      :remap 'kotlin-mode
      :url "https://github.com/fwcd/tree-sitter-kotlin"
      :ext "\\.kt\\'")
    ,(make-treesit-auto-recipe
      :lang 'latex
      :ts-mode 'latex-ts-mode
      :remap 'latex-mode
      :url "https://github.com/latex-lsp/tree-sitter-latex"
      :ext "\\.tex\\'")
    ,(make-treesit-auto-recipe
      :lang 'lua
      :ts-mode 'lua-ts-mode
      :remap 'lua-mode
      :url "https://github.com/Azganoth/tree-sitter-lua"
      :ext "\\.lua\\'")
    ,(make-treesit-auto-recipe
      :lang 'make
      :ts-mode 'makefile-ts-mode
      :remap 'makefile-mode
      :url "https://github.com/alemuller/tree-sitter-make"
      :ext "\\Makefile\\'")
    ,(make-treesit-auto-recipe
      :lang 'markdown
      :ts-mode 'markdown-ts-mode
      :remap '(poly-markdown-mode markdown-mode)
      :url "https://github.com/ikatyang/tree-sitter-markdown"
      :ext "\\.md\\'")
    ,(make-treesit-auto-recipe
      :lang 'proto
      :ts-mode 'protobuf-ts-mode
      :remap 'protobuf-mode
      :url "https://github.com/mitchellh/tree-sitter-proto"
      :ext "\\.proto\\'")
    ,(make-treesit-auto-recipe
      :lang 'python
      :ts-mode 'python-ts-mode
      :remap 'python-mode
      :url "https://github.com/tree-sitter/tree-sitter-python"
      :ext "\\.py\\'")
    ,(make-treesit-auto-recipe
      :lang 'r
      :ts-mode 'r-ts-mode
      :remap 'ess-mode
      :url "https://github.com/r-lib/tree-sitter-r"
      :ext "\\.r\\'")
    ,(make-treesit-auto-recipe
      :lang 'ruby
      :ts-mode 'ruby-ts-mode
      :remap 'ruby-mode
      :url "https://github.com/tree-sitter/tree-sitter-ruby"
      :ext "\\.rb\\'")
    ,(make-treesit-auto-recipe
      :lang 'rust
      :ts-mode 'rust-ts-mode
      :remap 'rust-mode
      :url "https://github.com/tree-sitter/tree-sitter-rust"
      :ext "\\.rs\\'")
    ,(make-treesit-auto-recipe
      :lang 'toml
      :ts-mode 'toml-ts-mode
      :remap '(conf-toml-mode toml-mode)
      :url "https://github.com/tree-sitter/tree-sitter-toml"
      :ext "\\.toml\\'")
    ,(make-treesit-auto-recipe
      :lang 'tsx
      :ts-mode 'tsx-ts-mode
      :requires 'typescript
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "tsx/src"
      :ext "\\.tsx\\'")
    ,(make-treesit-auto-recipe
      :lang 'typescript
      :ts-mode 'typescript-ts-mode
      :remap 'typescript-mode
      :requires 'tsx
      :url "https://github.com/tree-sitter/tree-sitter-typescript"
      :revision "master"
      :source-dir "typescript/src"
      :ext "\\.ts\\'")
    ,(make-treesit-auto-recipe
      :lang 'yaml
      :ts-mode 'yaml-ts-mode
      :remap 'yaml-mode
      :url "https://github.com/ikatyang/tree-sitter-yaml"
      :ext "\\.ya?ml\\'"))
  "Map each tree-sitter lang to Emacs metadata.")

(defun treesit-auto--maybe-install-grammar ()
  "Try to install the grammar matching the current major-mode.

If the tree-sitter grammar is missing for the current major mode,
this will silently fail, automatically install the grammar, or
prompt the user about automatic installation, depending on the
value of `treesit-auto-install'.  If installation of the grammar
is successful, activate the tree-sitter major mode."
  (when-let* ((not-ready (not (treesit-auto--ready-p major-mode)))
              (recipe (treesit-auto--get-mode-recipe))
              (ts-mode (treesit-auto-recipe-ts-mode recipe))
              (ts-mode-exists (fboundp ts-mode))
              (lang (treesit-auto-recipe-lang recipe)))
    ;; install required grammars
    (dolist (lang (ensure-list (treesit-auto-recipe-requires recipe)))
      (treesit-auto--prompt-to-install-package lang))
    (when (treesit-auto--prompt-to-install-package lang)
      (funcall ts-mode))))

(defun treesit-auto--ready-p (mode)
  "Determine if MODE is tree-sitter ready.

MODE can be either the tree-sitter enhanced version or one of the
fallback modes."
  (when-let* ((recipe (treesit-auto--get-mode-recipe mode))
              (lang (treesit-auto-recipe-lang recipe))
              (ts-mode (treesit-auto-recipe-ts-mode recipe)))
    (and (treesit-ready-p lang t)
         (fboundp mode)
         (fboundp ts-mode))))

(defun treesit-auto--prompt-to-install-package (lang)
  "Ask the user if they want to install a tree-sitter grammar for `LANG'.

Non-nil only if installation completed without any errors."
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

(defvar treesit-auto-opt-out-list
  nil
  "Language symbols to avoid when using `treesit-auto-install-all'.

This variable is ignored if `treesit-auto-langs' is non-nil.")

(defun treesit-auto--get-mode-recipe (&optional mode)
  "Look up the recipe for MODE.  If MODE is nil, use the current `major-mode'."
  (let ((mode (or mode major-mode)))
    (cl-loop for recipe in treesit-auto-recipe-list
             if (memq
                 mode
                 (cons (treesit-auto-recipe-ts-mode recipe)
                       (ensure-list (treesit-auto-recipe-remap recipe))))
             return recipe)))

(defun treesit-auto--build-major-mode-remap-alist ()
  "Construct `major-mode-remap-alist' using all known recipes."
  (append major-mode-remap-alist
          (let ((remap-alist '()))
            (cl-loop for recipe in treesit-auto-recipe-list
                     for ts-mode = (treesit-auto-recipe-ts-mode recipe)
                     when (treesit-auto--ready-p ts-mode)
                     do (dolist (remap (ensure-list (treesit-auto-recipe-remap recipe)))
                          (push (cons remap ts-mode) remap-alist))
                     finally return remap-alist))))

(defun treesit-auto--build-treesit-source-alist ()
  "Construct the `treesit-language-source-alist' using all known recipes."
  (append treesit-language-source-alist
          (cl-loop for recipe in treesit-auto-recipe-list
                   collect (cons (treesit-auto-recipe-lang recipe)
                                 `(,(treesit-auto-recipe-url recipe)
                                   ,(treesit-auto-recipe-revision recipe)
                                   ,(treesit-auto-recipe-source-dir recipe)
                                   ,(treesit-auto-recipe-cc recipe)
                                   ,(treesit-auto-recipe-c++ recipe))))))

(defun treesit-auto-install-all ()
  "Install every available, maintained grammar.

See `treesit-auto-langs' and `treesit-auto-install' for
how to modify the behavior of this function."
  (interactive)
  (when-let* ((treesit-language-source-alist (treesit-auto--build-treesit-source-alist))
              (to-install (or treesit-auto-langs
                              (seq-filter
                               (lambda (lang) (not (treesit-ready-p lang t)))
                               (cl-set-difference
                                (mapcar 'car treesit-language-source-alist)
                                treesit-auto-opt-out-list))))
              (prompt (format "The following tree-sitter grammars are/were missing:\n%s\n"
                              (mapconcat 'symbol-name to-install "\n"))))
    ;; TODO QOL - it would be nice if this messaged what was installed or at
    ;; least mentioned that nothing was installed if skipped.
    (with-output-to-temp-buffer "*Treesit-auto install candidates*"
      (princ prompt))
    (when (or (eq treesit-auto-install t) ; Quiet mode is off
              (y-or-n-p "Install missing grammars? "))
      (mapcar 'treesit-install-language-grammar to-install))))

(define-minor-mode treesit-auto-mode
  "Toggle `global-treesit-auto-mode'."
  :group 'treesit)

;; https://github.com/renzmann/treesit-auto/issues/47
(defvar global-treesit-auto-modes)

(define-globalized-minor-mode global-treesit-auto-mode treesit-auto-mode
  treesit-auto--on
  :group 'treesit
  :predicate
  ;; allow global mode to activate only on recipe modes,
  ;; but also allow to activate on remap and ts-modes
  ;; in case only the ts-mode is available.
  ;; non emacs core ts-modes might autoload and would be
  ;; nice to also prompt for grammar installation
  (let ((modes '()))
    (cl-loop for recipe in treesit-auto-recipe-list
             do (push (treesit-auto-recipe-ts-mode recipe) modes)
             do (dolist (mode (ensure-list (treesit-auto-recipe-remap recipe)))
                  (push mode modes))
             finally return modes))
  (if global-treesit-auto-mode
      ;; adding advice to set-auto-mode-0 is potentially dangerous
      ;; but we need to temporary update major-mode-remap-alist
      ;; and not modify the user specified list which will allow
      ;; the user to be in control of existing remaps.
      (advice-add #'set-auto-mode-0 :before #'treesit-auto--set-major-remap)
    (advice-remove #'set-auto-mode-0 #'treesit-auto--set-major-remap)))

(defun treesit-auto--set-major-remap (&rest _)
  "Locally set `major-mode-remap-alist' with all known recipes."
  ;; even though major-mode-remap-alist is set as local here,
  ;; when a major-mode matches the mode will be added to the top of
  ;; auto-mode-alist so it can't be really "switched off" afterwards.
  ;; The user needs to restart emacs or somehow reset auto-mode-alist to
  ;; the original.
  ;; For this mode to keep a cached copy is dangerous as it will be a global
  ;; replacement and ignores all changes while this mode is active, so
  ;; don't think it is a valid option.
  (setq-local major-mode-remap-alist (treesit-auto--build-major-mode-remap-alist)))

(defun treesit-auto--on ()
  "Turn `treesit-auto-mode' on."
  (setq-local treesit-language-source-alist
              (treesit-auto--build-treesit-source-alist))
  (treesit-auto--maybe-install-grammar))

(defun treesit-auto-add-to-auto-mode-alist ()
  "Register `auto-mode-alist' entries for ready tree-sitter recipes."
  (let ((installed-recipes
         (seq-filter
          (lambda (recipe) (treesit-ready-p (treesit-auto-recipe-lang recipe) t))
          treesit-auto-recipe-list)))
    (dolist (recipe installed-recipes)
      (add-to-list
       'auto-mode-alist
       (cons (treesit-auto-recipe-ext recipe) (treesit-auto-recipe-ts-mode recipe))))))

(provide 'treesit-auto)
;;; treesit-auto.el ends here
