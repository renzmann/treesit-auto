# Contributing new recipes

Each language's behavior for upgrade, fallback, and installation are defined by
a recipe in `treesit-auto-recipe-list`.  Here is an example entry for
JavaScript:

```elisp
    ,(make-treesit-auto-recipe
      :lang 'javascript
      :ts-mode 'js-ts-mode
      :remap '(js-mode javascript-mode js2-mode)
      :url "https://github.com/tree-sitter/tree-sitter-javascript"
      :revision "master"
      :source-dir "src")
```

1. `:lang` should *exactly* match the grammar name in the source repository's
   `grammar.js`.  Look for a line [like
   this](https://github.com/tree-sitter/tree-sitter-python/blob/9e53981ec31b789ee26162ea335de71f02186003/grammar.js#L28)
2. `:ts-mode` must be a single quoted symbol that is the tree-sitter mode Emacs
   should use for this grammar
3. `:remap` is either a quoted symbol or quoted list of symbols that are modes
   that should attempt to "switch up" to the tree-sitter major mode.  When the
   tree-sitter grammar isn't available, these modes are tried *in order* as
   fallback modes
4. `:url` must specify the grammar's source repository.  Many are already listed
   on the [tree-sitter GitHub](https://github.com/tree-sitter)
5. `:revision` and `:source-dir` are optional, but may be required if the
   `scanner.c` file is not found under the default directory or on the default
   branch
6. `:cc` and `:c++` should be left unspecified, as they are reserved for user
   customization

When submitting a pull request for a new recipe, use the title `New recipe:
<grammar name>`, and ensure that the patch consists only of the net-new recipe
lines, in the correct alphabetical position within `treesit-auto-recipe-list`.
