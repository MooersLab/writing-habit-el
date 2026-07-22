# Development

The package is small and split into focused module files, so a working session
needs only a checkout and Emacs 29.1 or newer built with SQLite support. This
page covers the test suite, the cross-port parity fixture, the byte-compile, and
building these docs.

## Testing

The tests use ERT and run in batch through the Makefile:

```sh
make test
```

The suite has a test file per module, namely name, db, plan, track, compare,
dashboard, and a main test for the batch dispatch. The tests that need SQLite
skip themselves when Emacs was built without it, and the plan-import tests skip
themselves when `writing-schedule.el` is not on the load path, so a partial
environment still gives a green run over the parts it can reach. Point
`WRITING_SCHEDULE_DIR` at a checkout to include the plan-import tests:

```sh
make test WRITING_SCHEDULE_DIR=/path/to/writing-schedule
```

## The cross-port parity fixture

`test/writing-habit-dashboard-tests.el` is the tuning fork between the two
ports. It renders the dashboard from `test/fixtures/cross-port.db` for the week
of 2026-01-19 and asserts that the result is byte-identical to
`test/fixtures/dashboard.html`. The same two fixture files ship with the Python
twin, and its suite asserts against them too, so this package and the Python
package stay aligned to one fixed rendering rather than to each other. When you
change the dashboard markup, regenerate the fixture in both repositories in the
same commit, because a one-byte drift fails both suites.

## The byte-compile

```sh
make compile
```

This byte-compiles every module with `byte-compile-error-on-warn` set, so a
clean compile is part of the bar for any change. Keep docstring lines within 80
characters, because the compiler flags longer ones. Run `M-x checkdoc` on a file
you touch, and run `package-lint` on the changed files before you open a pull
request. Continuous integration runs the same `make compile` and `make test`
across several Emacs versions on every pull request.

## Building these docs

The documentation is a Sphinx project under `docs/`, written in MyST markdown
and built with the Read the Docs theme. It documents the package rather than
importing it, so the build needs only the documentation requirements, not Emacs:

```sh
pip install -r docs/requirements.txt
sphinx-build -b html docs docs/_build/html
```

Open `docs/_build/html/index.html` in a browser. Read the Docs builds the same
project from `.readthedocs.yaml`, which installs `docs/requirements.txt` and
fails the build on any warning. Build with the warning flag before you push, so
a warning surfaces on your machine rather than on the Read the Docs build:

```sh
sphinx-build -W -b html docs docs/_build/html
```

## Regenerating the diagrams

The four Graphviz diagrams under `docs/imgs/` are built from the DOT sources in
`docs/_diagrams/`. Regenerate them with Graphviz installed:

```sh
for d in architecture schema-er workflow-loop schedule-code; do
  dot -Tpng -Gdpi=140 docs/_diagrams/$d.dot -o docs/imgs/$d.png
done
```

The two dashboard screenshots are renders of a dashboard HTML file, one in each
theme. The bar chart is the output of `compare --plot`. The canonical copies of
all these figures live under `assets/images/`, licensed CC BY 4.0.
