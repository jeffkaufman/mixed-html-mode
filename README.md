# mixed-html-mode

An Emacs major mode for HTML with inline CSS and JS.  Single file, no
dependencies beyond Emacs 27+.

This is a relatively minimal syntax highlighter for HTML, with two key
advantages:

1. It is performant even on very large files.

2. It handles inline CSS and JS without getting confused about what
   state it's in.

At a high level, it understands what the regions of the file are the
same way a web browser would, including understanding that a literal
`</script>` ends JS even if it's inside quotes.  Then within each
region it parses appropriately for that language.

## Install

Copy `mixed-html-mode.el` somewhere on your load path and add to your
init file:

```elisp
(require 'mixed-html-mode)
```

Or load it directly:

```elisp
(load-file "/path/to/mixed-html-mode.el")
```

Optionally byte-compile for faster fontification:

```
emacs --batch -f batch-byte-compile mixed-html-mode.el
```

Followed with:

```elisp
(load "/path/to/mixed-html-mode.elc")
```

It auto-activates on `.html` files.  To use it manually: `M-x
mixed-html-mode`.

## What it highlights

- **HTML**: tag names, attribute names, attribute values, comments,
  DOCTYPE, entity references
- **CSS** (inside `<style>`): selectors, property names, values,
  at-rules (`@media`, etc.), comments, strings
- **JS** (inside `<script>`): keywords, built-in objects, constants,
  strings (single/double/template literals with `${}`), regex
  literals, comments (`//` and `/* */`), numbers

## Caveats

- No indentation support.
- Inline `style="..."` and `onclick="..."` attributes are highlighted
  as plain strings, not as CSS/JS.
- The CSS parser uses brace-counting to distinguish selectors from
  properties, which can get confused by malformed CSS.
- Region detection rescans from the top of the buffer, so editing a
  tag name like `<style>` correctly triggers rehighlighting of all
  affected content below.

## Tests

```
emacs --batch -l mixed-html-mode.el -l mixed-html-mode-tests.el -f ert-run-tests-batch-and-exit
```

See `examples/` for some example pages it highlights correctly.

