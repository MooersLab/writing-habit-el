# Installation

`writing-habit` runs inside GNU Emacs. It stores its data in SQLite through the
built-in `sqlite-*` functions, so Emacs must be built with SQLite support.

## Requirements

The package needs GNU Emacs 29.1 or newer, built with SQLite support. It checks
`sqlite-available-p` and reports a clear message when SQLite is missing. It also
needs `transient` 0.4 or newer, which ships with Emacs 28 and later.

Two dependencies are optional. `writing-schedule.el` is needed only for the
plan importer, and it is loaded lazily, so every other command runs without it.
`python3` with `matplotlib` is needed only for the comparison plot; the HTML
dashboard is pure Emacs Lisp and needs no Python.

## From MELPA

Once the package is accepted on [MELPA](https://melpa.org), install it with the
built-in package manager:

```
M-x package-install RET writing-habit RET
```

With `use-package`:

```elisp
(use-package writing-habit
  :ensure t
  :commands (writing-habit writing-habit-initdb))
```

## From the repository

Clone the repository and put it on your load path:

```elisp
(add-to-list 'load-path "/path/to/writing-habit-el")
(require 'writing-habit)
```

Or, with `use-package` and a version-controlled checkout on Emacs 30 or newer:

```elisp
(use-package writing-habit
  :vc (:url "https://github.com/MooersLab/writing-habit-el" :rev :newest)
  :commands (writing-habit writing-habit-initdb))
```

To enable the plan importer, also put `writing-schedule.el` on the load path.

## Verifying the installation

Open the menu with `M-x writing-habit`. The transient menu appears with its
three groups, namely Set up, Plan and track, and Review. If the menu opens, the
package is loaded. To confirm SQLite support, run `M-x writing-habit-initdb`
and give a scratch file name; a clear message reports success, and a message
about missing SQLite means Emacs was built without it.
