# Commands and the batch interface

Every stage is an interactive command, gathered under one transient menu, and
every stage is also a batch subcommand, so the toolkit runs from a shell or a
Makefile the same way the Python command-line interface does.

## The transient menu

`M-x writing-habit` opens a menu that gathers every command under three groups:

```
writing-habit
  Set up
    d  Create a database
  Plan and track
    p  Import a weekly plan
    c  Import actuals CSV
    i  Import actuals ICS
    k  Harvest org clocks
    a  Add a session by hand
  Review
    r  Weekly report
    D  HTML dashboard
    n  Decode a schedule code
```

## Interactive commands

Each command prompts for what it needs, so you can run any one directly with
`M-x`. Every command that touches the database prompts for the database file,
so you can keep more than one.

| Command | What it does |
|---------|--------------|
| `writing-habit` | Open the transient menu. |
| `writing-habit-initdb` | Create the schema and seed the activities in a database. |
| `writing-habit-plan-import-file` | Load a weekly `writing-schedule` table for a week. Needs `writing-schedule.el`. |
| `writing-habit-track-add-to-file` | Add one session by hand. |
| `writing-habit-track-import-csv-file` | Import the tracking CSV. |
| `writing-habit-track-import-ics-file` | Import an actuals calendar. |
| `writing-habit-track-harvest-clock-file` | Harvest completed org-clock entries. |
| `writing-habit-report-week` | Show the weekly comparison as an org buffer. |
| `writing-habit-dashboard` | Write and open the HTML dashboard. |
| `writing-habit-name` | Decode a schedule code and check it against a legend. |

Every date prompt accepts any day inside the target week, because the week snaps
to the Monday on or before that date. The `writing-habit-name` command is the
only one that needs neither a database nor `writing-schedule.el`.

## The batch entry point

The package has a batch entry point, `writing-habit-batch`, so it runs from a
shell or a Makefile the way the Python argparse interface does. The subcommands
and their options mirror the Python command-line interface, so a Makefile that
drives one port drives the other with only the launcher changed.

```sh
emacs --batch -l writing-habit -f writing-habit-batch initdb --db DB
emacs --batch -l writing-habit -f writing-habit-batch plan import TABLE --week DATE --db DB
emacs --batch -l writing-habit -f writing-habit-batch track import FILE --format csv|ics --db DB
emacs --batch -l writing-habit -f writing-habit-batch \
      track add --day DATE --project CODE [--minutes N] [--category C] [--start HH:MM] [--end HH:MM] [--note TEXT] --db DB
emacs --batch -l writing-habit -f writing-habit-batch compare --week DATE [--plot FILE] --db DB
emacs --batch -l writing-habit -f writing-habit-batch dashboard --week DATE --out FILE --db DB
emacs --batch -l writing-habit -f writing-habit-batch name CODE [--table TABLE]
```

The batch runner reads the remaining command-line arguments, runs the command,
prints the result to standard output, and exits with a status code, namely zero
on success and one on a usage error or a failure. Plan import additionally needs
`writing-schedule.el` on the load path; the other subcommands do not.

### The subcommands

`initdb` creates the schema and seeds the three activities. It is safe to run
twice, because the schema uses `IF NOT EXISTS` throughout.

`plan import` parses a weekly `writing-schedule` table and fills the `project`,
`category`, and `plan_block` tables for the week that contains `--week`.

`track import` loads actual sessions from a file, with `--format csv` for the
tracking CSV and `--format ics` for an actuals calendar. `csv` is the default.

`track add` inserts one session. Give `--minutes`, or both `--start` and
`--end`, and give `--category` when the activity matters.

`compare` prints the planned-versus-actual report for the week, by project, by
activity, and by barbell class, and prints the current streak. With `--plot` it
also writes a bar chart, which needs `python3` with `matplotlib`.

`dashboard` renders the self-contained HTML dashboard for the week and writes it
to `--out`.

`name` decodes a schedule code and, with `--table`, checks its project letters
against that table's legend.
