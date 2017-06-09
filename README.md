# Emacs extensions
This is a small, independent collection of Emacs minor modes. Most of
these require a recent version of GNU emacs (i.e., version 25+).

## Quoting
__quote-per-event-mode__ is a minor mode that provides a single
function, "quote-per-event", for on-demand quoting and unquoting of
regions, things-at-point and points.

__quote-per-event-mode__ is not an "electric" mode, i.e., it's not
intended for rebinding self-insert characters nor for modifying
"self-insert-command". Instead of typing, e.g., `(`, to insert a pair of
balanced parentheses, the default binding is `CTRL + (`.

Alternative "balanced-pair" minor modes worth exploring:

* [Electric Pair](https://www.emacswiki.org/emacs/ElectricPair)
* [Par Edit](https://www.emacswiki.org/emacs/ParEdit)
* [SmartParens](https://github.com/Fuco1/smartparens)
* [Auto Pairs](https://www.emacswiki.org/emacs/AutoPairs)

## Jumping between edits
__mark-navigation-mode__ is a minor mode that provides bindings for
forward and backward traversal of local and global mark rings. In
particular, it's useful for returning to the location of a previous
edit or jumping between edits.

## Unicode keymap extensions
__unicode-keymap-extensions-mode__ is a minor mode that provides
extended Unicode (`CTRL-X 8` keymap) bindings.

## Align numbers
__align-numbers__ is an Emacs function to align a table of numbers on
their radix or decimal point. It's built around the native Emacs
function __align-regexp__.

## Insert magic numbers
__script-set-magic-mode__ is a minor mode that sets a script's magic
number (or "shebang"), if any, and makes it executable based on the
current major mode.

# Installation
Note that to use these extensions, as is, requires at least Emacs 25
(the current version as of this writing). To use them with earlier
versions of Emacs, in each source file comment out the "*-local-mode"
minor mode declaration.

## To install quote-per-event-mode
Place the file _quote-per-event.el_ in Emacs load path, and add to
Emacs init file *~/.emacs* or *~/.emacs.d/init.el*:

```lisp
(require 'quote-per-event)
```
To enable it globally, add:

```lisp
(quote-per-event-mode)
```

To enable it locally per major mode - .e.g, only in
__elisp-mode__ - use instead:

```lisp
(add-hook 'elisp-mode-hook
  #'quote-per-event-local-mode)
```

To enable it globally with some exceptions - e.g., everywhere
except __elisp-mode__ - use:

```lisp
(quote-per-event-mode)
(add-hook 'elisp-mode-hook
  (lambda ()
    (quote-per-event-local-mode -1)))
```

The default key bindings are as follows:


Binding  | Open-char | Close-char
---------|-----------|-----------
CTRL + ' | '  | '
CTRL + ` | ` | '
CTRL + " | " | "
CTRL + ＼ | ＼ | ＼
CTRL + : | : | :
CTRL + ( | ( | )
CTRL + < | < | >
CTRL + { | { | }
CTRL + $ | $ | $
CTRL + % | % | %
CTRL + * | * | *
CTRL + ｜ | ｜ | ｜
META + [ | [ | ]
META + _ | _ | _
CTRL + . | . | .
CTRL + , | , | ,
META + " | « | »
META + * | / | /
CTRL + META + ? | ? | ?
CTRL + META + ` | ` | `
CTRL + META + ' | ‘ | ’
CTRL + META + " | “ | ”

NB: the last two quotes are typographic.

To insert, for instance, a C-style comment `/* */`,
press `META + *` followed by `CTRL + *`.

To insert a TeX display environment `\[ \]` is trickier.  I use the
key sequence `CTRL + \`, then select one character forward `CTRL +
SHIFT + F` and finally `META + [`.

## To install unicode-keymap-extensions-mode
Place the file _unicode-keymap-extensions.el_ in Emacs load path, and
add to Emacs init file *~/.emacs* or *~/.emacs.d/init.el*:

```lisp
(require 'unicode-keymap-extensions)
```

To enable it globally, add:

```lisp
(unicode-keymap-extensions-mode)
```

To enable it locally per major mode - .e.g, only in
__elisp-mode__ - use instead:

```lisp
(add-hook 'elisp-mode-hook
  #'unicode-keymap-extensions-local-mode)
```

To enable it globally with some exceptions - e.g., everywhere
except __elisp-mode__ - use:

```lisp
(unicode-keymap-extensions-mode)
(add-hook 'elisp-mode-hook
  (lambda ()
    (unicode-keymap-extensions-local-mode -1)))
```

Now, typing `CTRL + x 8 g a` inserts the greek character α (alpha),
`CTRL + x 8 r f` inserts the Russian character ф (ef), etc.

## To install mark-navigation-mode
Place the file _mark-navigation.el_ in Emacs load path, and
add to Emacs init file *~/.emacs* or *~/.emacs.d/init.el*:

```lisp
(require 'mark-navigation)
```

To enable it globally, add:

```lisp
(mark-navigation-mode)
```

To enable it locally per major mode - .e.g, only in
__elisp-mode__ - use instead:

```lisp
(add-hook 'elisp-mode-hook
  #'mark-navigation-local-mode)
```

To enable it globally with some exceptions - e.g., everywhere
except __elisp-mode__ - use:

```lisp
(mark-navigation-mode)
(add-hook 'elisp-mode-hook
  (lambda ()
    (mark-navigation-local-mode -1)))
```

The default mappings are `META + P` and `META + N` for previous and
next, respectively. The mode is disabled in the mini-buffer.


## To install align-numbers
Place the file _align-numbers.el_ in Emacs load path, and
add to Emacs init file *~/.emacs* or *~/.emacs.d/init.el*:

```lisp
(require 'align-numbers)
```

Since this is just an Emacs function, invoke it on a region
with `META-x align-numbers`.

## To install script-set-magic-mode
Place the file _script-set-magic.el_ in Emacs load path, and
add to Emacs init file *~/.emacs* or *~/.emacs.d/init.el*:

```lisp
(require 'script-set-magic)
(script-set-magic-mode)
```

By default, this inserts magic numbers of the form
`#!/path/to/interpreter`.

To get magic numbers of the form `#!/usr/bin/env interpreter`, add
to Emacs init file:

```lisp
(setq executable-interpreter-path-absolute nil)
(setq executable-prefix "#!/usr/bin/env ")
```
