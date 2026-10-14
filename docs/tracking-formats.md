# Tracking formats

The track stage records the sessions you actually worked. It accepts four
inputs, namely a one-line manual entry, a CSV, an ICS calendar, and an org-clock
harvest. All four land in the same `session` table, so you can mix them within a
week. None of them needs a third-party package, because the CSV reader is
written in the package and the ICS reader uses the `icalendar.el` library
bundled with Emacs.

## A session by hand

At the end of a day you can add a single session with
`M-x writing-habit-track-add-to-file`, which prompts for the database, the day,
the project code, the minutes, the category, and a note. From a shell:

```sh
emacs --batch -l writing-habit -f writing-habit-batch \
      track add --day 2026-01-19 --project A --minutes 75 --category generative --db habit.db
```

Give either the minutes, or both a start and an end time, from which the minutes
are computed. A category left off means the session still counts toward the
project and the barbell views, and it is left out of the activity view.

## The tracking CSV

The CSV template has the columns `date`, `start`, `end`, `minutes`,
`project_code`, `category`, and `note`. Excel and Google Sheets both export CSV,
so this path is the universal bridge.

```
date,start,end,minutes,project_code,category,note
2026-01-19,04:00,05:30,,A,generative,morning block on DNPH1
2026-01-19,,,45,EM,support,end-of-day inbox triage entered by hand
2026-01-20,09:15,10:45,,B,editing,rewrite of results section
```

Enter either a start and an end time, or a minutes value. The `project_code`
must match a legend code, and the `category` must be generative, editing, or
support, or left blank. Import it with
`M-x writing-habit-track-import-csv-file`, or from a shell:

```sh
emacs --batch -l writing-habit -f writing-habit-batch \
      track import actuals.csv --format csv --db habit.db
```

## The actuals ICS

Keep your real work blocks in their own calendar so the plan and the actuals
never collide. Put the legend code in the event summary in brackets, for example
`[A] DNPH1 docking`, and put the activity in the categories field, for example
`generative`. The `DTSTART` and `DTEND` give the real minutes, read as written,
so use local floating times, the form a personal calendar exports for real work
blocks. An event whose summary carries no bracketed code is skipped. Import it
with `M-x writing-habit-track-import-ics-file`, or from a shell:

```sh
emacs --batch -l writing-habit -f writing-habit-batch \
      track import actuals.ics --format ics --db habit.db
```

## The org-clock harvest

This is the capture path the Python version cannot offer, because Emacs already
measures writing time. `M-x writing-habit-track-harvest-clock-file` reads
completed `CLOCK` lines from an org file. A clock is attributed to a project by
a bracketed code in its heading, the same convention the ICS importer uses, and
to an activity by a generative, editing, or support tag on the heading.

```org
** [A] DNPH1 docking :generative:
:LOGBOOK:
CLOCK: [2026-01-19 Mon 04:00]--[2026-01-19 Mon 05:30] =>  1:30
:END:
```

The harvest is idempotent, so running it again over the same clocks inserts
nothing new. Harvested rows carry the `manual` source with a `source_ref` that
begins `orgclock:`, which keeps the shared schema unchanged, so a database that
holds harvested clocks still reads cleanly in the Python port.

## A mytime.db time-tracking database

`M-x writing-habit-track-sync-mytime-file` copies timed sessions from the
`zTimeSpent` table of a mytime.db database. It prompts for the habit database
and for the mytime.db, which defaults to `writing-habit-track-mytime-db`
(`~/6003TimeTracking/cb/mytime.db`). With a prefix argument it also asks for
the scope and makes a dry run that writes nothing. The report opens in a
buffer. In the menu, `t` then `y` runs the same sync, and the "Sync from
mytime.db" group sets the scope, the first and last days, a fallback table, and
the dry run. From a shell:

```sh
emacs --batch -l writing-habit -f writing-habit-batch \
      track sync ~/6003TimeTracking/cb/mytime.db --db ~/org/writing-habit.db --dry-run
```

The tracker names a project by its four-digit number and directory, such as
`1008ToolsForQCrOfRNA`, and the weekly table names it by a legend code, such
as `Z`. The legend of the week each session falls in joins the two, so a code
may stand for a different project from one week to the next. The table that
planned the week comes first. A week with no imported plan uses `--table`, then
the nearest earlier planned week. The tracker's Activity tag sets the activity
by its first letter, `g` generative, `e` or `r` editing, and `s` support. A
session with no tag takes the activity of the planned block it overlaps.
Sessions that cross midnight wrap, so 21:00 to 00:00 is 180 minutes.

Each copied row carries the source `sqlite` and the `source_ref`
`mytime.db:zTimeSpent:<id>`, so a rerun updates a session you edited in the
tracker and never duplicates one. The sync matches `track sync` of the Python
package row for row and prints the same report, which ends with the projects
it left out and their minutes.

## Marking safe and speculative projects

The barbell view needs to know which projects are safe and which are
speculative. Set that class in the weekly table, not in the tracker, by adding a
risk tag to the end of a legend description in either the org-tag form `:safe:`
or the parenthesis form `(safe)`. The recognized tags are `safe`,
`speculative`, and `support`.

```org
| A: DNPH1 docking :safe:      |  |  |  |  |  |
| W: 2026words :risky:   |  |  |  |  |  |
```

Inside a table cell `:safe:` is literal text, because org reads `:tag:` syntax
only on headlines, so the tag does not affect the table or its export. Plan
import strips the tag before it stores the description, so the project name stays
clean.
