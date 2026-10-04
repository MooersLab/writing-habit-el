# The command menu

`M-x writing-habit` opens `writing-habit-dispatch`, the Emacs counterpart of
the tabs and forms of the Python package's graphical interface. It has one
group per stage of the weekly loop, the same groups as the Python tabs.

```
writing-habit   database: ~/habit.db
  Scheduler        Tracker          Across weeks          Session
  s  Schedule      p  Plan          h  History            l  Show the log
  g  Generate      t  Track         x  Context
  S  Sheets        c  Compare       n  Seasons and names
```

| Group | Commands | Program |
|-------|----------|---------|
| Schedule | edit table, check, template, new week from template, open recent table | `writing-schedule.el` |
| Generate | generate a week, generate a day, export `.ics`, list archived weeks | `writing-schedule.el` |
| Sheets | sheets for a week, a sheet for one day | `writing-schedule.el` |
| Plan | `initdb`, `plan import` | `writing-habit` |
| Track | `track import`, `track add`, harvest org clocks | `writing-habit` |
| Compare | `compare`, `dashboard` | `writing-habit` |
| History | `history` | `writing-habit` |
| Context | `context set`, `clear`, `list` | `writing-habit` |
| Seasons and names | `seasons`, `name` | `writing-habit` |

The Scheduler group appears only when `writing-schedule.el` is on the load
path, as the Python interface hides its scheduler tabs when that package is
missing.

## Forms

Each group opens a form whose options mirror the command line. A date option
reads a date from the org calendar, a file option reads a file name, and a
choice option completes from its choices. A suffix that still needs an option
names it rather than running.

The database option is seeded from `writing-habit-default-db` and then from the
database the last run used, and the week option starts at today, so a weekly
loop of `plan import`, `compare`, and `dashboard` needs no retyping. The table
option of the scheduler groups remembers the last table in the same way.

## The log

Every run is written to the `*writing-habit log*` buffer. A tracker run shows
two equivalent shell lines, the `writing-habit` line that the Python package
accepts and the Emacs batch line, then the output. A scheduler run shows the
`writing-schedule.sh` line. A setting that the shell line cannot carry, such as
the `--strict` switch, which binds `writing-schedule-overlap-action` to `error`
for one run, is named on a line of its own. You can copy any line into a
terminal, so the menu teaches the command line rather than replacing it.

## Seeing what a command wrote

A file that a command writes gets a button in the log. With
`writing-habit-dispatch-auto-preview` non-nil, the main file is shown at once.

| Output | How it is shown |
|--------|-----------------|
| the weekly dashboard and the seasons page | in eww, or in your browser when eww is missing |
| the `compare` and `history` plots | in image mode |
| the sheet PDFs | in doc-view |
| the schedule `.org`, the `.ics`, and the sheet `.org` or `.tex` | read-only as text |

Run `M-x writing-habit-dispatch-preview` to show any file the same way, and use
`browse-url-of-file` to open a dashboard in your own browser, which renders its
styling faithfully.
