# Function reference

The package is split into focused module files, all loaded by the
`writing-habit.el` aggregator. Every public symbol is prefixed with
`writing-habit-`, and private helpers use a double dash, for example
`writing-habit-db--blank-p`. The public functions are listed below by module.
The `-file` commands and the menu are interactive; the rest are library
functions you can call from your own Emacs Lisp.

## writing-habit (aggregator)

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit` | command | Open the transient menu. |
| `writing-habit-initdb` | command | Create the schema and seed the activities in a database. |
| `writing-habit-batch` | function | Batch entry point for `emacs --batch`; reads the argument list, runs the command, prints the result, and exits with a status code. |

## writing-habit-db

The data layer, the single contract between the stages. It reads the same
`schema.sql` the Python package uses.

| Symbol | What it does |
|--------|--------------|
| `writing-habit-db-connect` | Open a connection with foreign keys on. |
| `writing-habit-db-init` | Load the schema and seed the activities. |
| `writing-habit-db-get-category-id` | Return the category id for a name, or nil. |
| `writing-habit-db-get-or-create-project` | Return the project id for a legend code, creating and backfilling the row as needed. |
| `writing-habit-db-log-import` | Record the provenance of one import. |
| `writing-habit-db-minutes-between` | Minutes between two `HH:MM` times. |
| `writing-habit-db-close` | Close a connection. |

## writing-habit-name

Decodes a schedule file-name code. Needs no database and no third-party package.

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-name` | command | Decode a code and show a report; with a prefix argument, check it against a table legend. |
| `writing-habit-name-decode` | function | Return an alist of day to blocks for the week the code names. |
| `writing-habit-name-summary` | function | Return the block totals by activity and by project. |
| `writing-habit-name-read-legend` | function | Read a weekly table's legend into an alist. |
| `writing-habit-name-check-against-legend` | function | Match each project letter to a legend entry. |
| `writing-habit-name-report-string` | function | Return the full decode-and-check report as a string. |

## writing-habit-plan

Loads a weekly plan by reusing the `writing-schedule.el` parser. Loaded lazily,
so the rest of the toolkit runs without `writing-schedule.el`.

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-plan-import-file` | command | Import a weekly table for a week into a database file. |
| `writing-habit-plan-import` | function | Parse a table for a week and load its blocks into an open database. |
| `writing-habit-plan-parse-file` | function | Return the `writing-schedule` parse plist for a table file. |

## writing-habit-track

Records actual sessions four ways. The CSV reader is written in the package and
the ICS reader uses the bundled `icalendar.el`, so neither needs a third-party
package.

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-track-add-to-file` | command | Add one session by hand to a database file. |
| `writing-habit-track-import-csv-file` | command | Import the tracking CSV into a database file. |
| `writing-habit-track-import-ics-file` | command | Import an actuals calendar into a database file. |
| `writing-habit-track-harvest-clock-file` | command | Harvest completed org clocks into a database file. |
| `writing-habit-track-add` | function | Insert one session into an open database and return its id. |
| `writing-habit-track-import-csv` | function | Insert one session per data row of the tracking CSV. |
| `writing-habit-track-import-ics` | function | Insert one session per bracketed-code event of an ICS calendar. |
| `writing-habit-track-harvest-clock` | function | Insert one session per completed org `CLOCK` line, idempotently. |

## writing-habit-compare

Thin wrappers over the four comparison views plus the streak. It never writes
its own aggregation SQL.

| Symbol | What it does |
|--------|--------------|
| `writing-habit-compare-week-project` | Planned versus actual and adherence per project for a week. |
| `writing-habit-compare-week-category` | Planned versus actual per activity for a week. |
| `writing-habit-compare-week-barbell` | Planned versus actual per risk class for a week. |
| `writing-habit-compare-day-actual` | Actual minutes and a worked flag per day for a week. |
| `writing-habit-compare-current-streak` | Length of the run of consecutive worked days ending at the latest. |

## writing-habit-report

Renders the org report and the optional plot.

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-report-week` | command | Open the weekly comparison as an org buffer. |
| `writing-habit-report-week-string` | function | Return the weekly report as a string. |
| `writing-habit-report-write-plot` | function | Write the planned-versus-actual bar chart to a PNG. |

## writing-habit-dashboard

Renders the self-contained HTML dashboard, the twin of the Python
`dashboard.py`.

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-dashboard` | command | Write the dashboard and open it in a browser. |
| `writing-habit-dashboard-write` | function | Render the dashboard and write it to a path. |
| `writing-habit-dashboard-html` | function | Return the dashboard HTML as a string. |

## writing-habit-history

Ports `compare/history.py`.

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-history` | command | Show the history of a database as an org buffer. |
| `writing-habit-history-collect` | function | Return the weeks and the five adherence series. |
| `writing-habit-history-string` | function | Return the plain-text table, identical to Python. |
| `writing-habit-history-org` | function | Return the history as an org table. |
| `writing-habit-history-write-plots` | function | Write the five-panel plot with matplotlib. |
| `writing-habit-compare-project-mean-series` | function | Mean per-project adherence per week. |
| `writing-habit-compare-category-mean-series` | function | The same mean within one activity. |

## writing-habit-table

Ports `gui/weekly_table.py`. Row indexes are zero-based and count every table
line, rules included.

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-table-from-text`, `-from-file` | function | Parse a table document. |
| `writing-habit-table-to-text`, `-save` | function | Write it back, byte for byte when untouched. |
| `writing-habit-table-set-cell`, `-set-legend` | function | Rewrite one line. |
| `writing-habit-table-suggest-times`, `-insert-block` | function | Insert an empty block beside a row. |
| `writing-habit-table-move-block`, `-can-move` | function | Move a block, joining a new section when it passes a header. |
| `writing-habit-table-insert-legend`, `-move-legend` | function | Insert or move a project in the legend. |
| `writing-habit-table-sync-legend` | function | Give every grid code a legend row. |
| `writing-habit-table-rows-clear-of` | function | The blocks whose range does not overlap a row. |
| `writing-habit-table-overlaps`, `-conflicting-cells` | function | Clashes by the scheduler's rule. |
| `writing-habit-table-totals` | function | Planned minutes by day, project, and activity. |
| `writing-habit-table-code-or-problem` | function | The canonical schedule code, or why there is none. |
| `writing-habit-table-rename-to-canonical` | function | Rename the file to its canonical code. |
| `writing-habit-table-split-due-date`, `-project-info` | function | Read a due date and describe a code. |

## writing-habit-table-mode

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-table-mode` | minor mode | Tints, completion, eldoc, and the table commands. |
| `writing-habit-table-mode-maybe` | function | Turn the mode on in a schedule-code file, from `org-mode-hook`. |
| `writing-habit-table-menu` | command | The table menu. |
| `writing-habit-table-report` | command | The name, clashes, totals, and legend in a side window. |

The other commands are listed in {doc}`table-mode`.

## writing-habit-dispatch

| Symbol | Kind | What it does |
|--------|------|--------------|
| `writing-habit-dispatch` | command | The top menu, also run by `writing-habit`. |
| `writing-habit-dispatch-plan` and its siblings | command | One form per stage. |
| `writing-habit-dispatch-run-habit` | function | Run a tracker command, log it, and preview its files. |
| `writing-habit-dispatch-run-schedule` | function | Run a scheduler call and log it as a `writing-schedule.sh` line. |
| `writing-habit-dispatch-preview` | command | Show a file the way its kind deserves. |
| `writing-habit-dispatch-show-log` | command | Show the log. |

## Configuration

| Variable | Default | Meaning |
|----------|---------|---------|
| `writing-habit-db-schema-file` | `schema.sql` beside the package | Path to the shared schema. |
| `writing-habit-report-python` | `"python3"` | Interpreter for the optional plots. |
| `writing-habit-default-db` | nil | Database that seeds the forms of the menu. |
| `writing-habit-dispatch-auto-preview` | t | Show the main file a command writes. |
| `writing-habit-table-mode-auto` | t | Turn on the table mode in schedule-code files. |
| `writing-habit-table-mode-prefix` | `"C-c C-;"` | Key of the table menu. |
| `writing-habit-table-idle-delay` | 0.25 | Seconds of idle time before the tints refresh. |

Set them through `M-x customize-group RET writing-habit RET` or in your init
file.
