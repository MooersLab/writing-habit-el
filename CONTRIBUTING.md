# Contributing to writing-habit

Thank you for your interest in improving `writing-habit`. Bug reports, feature ideas, documentation fixes, and code are all welcome. This document explains how to set up, test, and submit changes.

## Ways to contribute

- Report a bug or request a feature by opening an issue. For a bug, include your Emacs version, your operating system, and the smallest steps that reproduce the problem.
- Improve the documentation. The README, the docstrings, and the naming spec in `docs/table-file-naming-rules.org` are all fair game.
- Submit code through a pull request, following the steps below.

## Development setup

You need GNU Emacs 29.1 or newer, built with SQLite support. The package checks `sqlite-available-p` and refuses to run without it.

Two dependencies are optional:

- `writing-schedule.el`, needed only for the plan importer and the plan-import tests. Clone it and point `WRITING_SCHEDULE_DIR` at the checkout.
- `python3` with `matplotlib`, needed only for the comparison plot. The HTML dashboard is pure Emacs Lisp and needs no Python.

Clone the repository and run the checks:

```sh
git clone https://github.com/MooersLab/writing-habit-el
cd writing-habit-el
make compile
make test WRITING_SCHEDULE_DIR=/path/to/writing-schedule
```

`make compile` byte-compiles every module and treats warnings as errors, so a clean compile is part of the bar for any change. `make test` runs the ERT suite. The tests that need SQLite skip themselves when Emacs was built without it, and the plan-import tests skip themselves when `writing-schedule.el` is not on the load path, so a partial environment still gives a green run over the parts it can reach.

## Coding conventions

- Follow the standard Emacs Lisp conventions. Every file uses lexical binding, and every public symbol is prefixed with `writing-habit-`. Private helpers use a double dash, for example `writing-habit-db--blank-p`.
- Keep the byte-compile clean under warnings-as-errors, and keep docstring lines within 80 characters, because the compiler flags longer ones.
- Run `M-x checkdoc` on a file you touch, and run `package-lint` on the changed files before you open a pull request.
- Add or update an ERT test for any behavior you change. New commands should register in the transient menu, in the batch dispatch inside `writing-habit.el`, and carry an autoload cookie where a user would call them cold.

## The interoperability contract

This package shares one SQLite schema, `schema.sql`, and one output format with the Python [writing-habit](https://github.com/MooersLab/writing-habit-py) package, so a database written by one reads in the other and the two produce byte-identical dashboards. That contract is load-bearing, so please protect it.

- When you change `schema.sql`, change it in both ports, because both read the same file.
- When you change a serialized output, the report text, the dashboard HTML, or the plot, regenerate the committed fixture under `test/fixtures/` from the Python side and update the guard test on this side, so the two ports stay pinned to the same bytes.
- Prefer reusing the shared views and the shared naming spec over writing new aggregation, because the schema is meant to be the single source of truth.

## Submitting a pull request

1. Create a branch from `main`.
2. Make focused commits with clear messages.
3. Run `make compile` and `make test` and make sure both pass.
4. Open a pull request that describes what changed and why, and links any related issue.

Continuous integration runs the same `make compile` and `make test` across several Emacs versions on every pull request, so a change that passes locally should pass there too.

## License

This project uses split licensing: the source code is under the MIT License, and the images in `assets/images/` are under CC BY 4.0.

By contributing code, you agree that your contributions are licensed under the MIT License; see [LICENSE](LICENSE). By contributing an image or figure, you agree that it is licensed under the Creative Commons Attribution 4.0 International License; see [assets/images/LICENSE](assets/images/LICENSE).
