# Editing the weekly table

`writing-habit-table-mode` is the Emacs counterpart of the Schedule tab of the
Python package's graphical interface. The org table in your buffer is the grid,
so the mode adds only what the table cannot show on its own. It tints clashes,
shows which times are free, completes and explains project codes, and gives the
table commands that keep the file readable by both the scheduler and the
tracker.

## Turning it on

The mode turns itself on in an org buffer whose file name is a schedule code
and that holds a weekly table. Both `4gAeA-gW.org` and
`2026-01-19_4gAeA-gW.org` qualify, while `my-week.org` does not. Set
`writing-habit-table-mode-auto` to nil to stop that, and run
`M-x writing-habit-table-mode` by hand instead.

Opening an ordinary org file loads only the small schedule-code decoder in
`writing-habit-table-auto.el`. The mode itself, with `writing-schedule.el`,
loads only for a file whose name qualifies.

## What the mode shows

| Signal | Meaning |
|--------|---------|
| a red day cell | the block overlaps another block on the same day |
| a yellow row | with point in the Time column of a block, this block does not overlap it |
| a pale blue, green, or lilac day cell | the block counts as generative, editing, or support writing |
| `WH[2 clashes]` in the mode line | the number of overlapping pairs in the week |
| the echo area | the name, due date, risk, and activity of the code in the day cell at point |

The overlap rule is the scheduler's own, because the mode calls
`writing-schedule-overlaps` from the public API of `writing-schedule.el` 0.3.1.
Each range runs from its start up to, but not including, its end, so blocks
that only touch, such as 04:00-05:30 and 05:30-07:00, do not overlap. A range
whose end is not after its start runs past midnight. A clash cell stays red
inside a yellow row, because the clash is the more urgent thing to see.

Press `M-TAB` or `C-M-i` in a day cell to complete a project code from the
legend, also after an activity letter such as the `e` of `eA`. Set
`writing-habit-table-tint-activities` to nil to turn the activity tints off, or
customize the faces `writing-habit-table-generative`, `-editing`, and
`-support`. The tints refresh after `writing-habit-table-idle-delay` seconds of
idle time.

## Commands

`C-c C-;` opens the table menu. Change the key through
`writing-habit-table-mode-prefix` before the mode first loads.

| Key in the menu | Command | What it does |
|-----------------|---------|--------------|
| `a` | `writing-habit-table-insert-above` | insert an empty block above the block or section header at point |
| `b` | `writing-habit-table-insert-below` | insert an empty block below the block or section header at point |
| `<up>`, `<down>` | `writing-habit-table-move-up`, `-move-down` | move the block or legend entry at point |
| `d` | `writing-habit-table-delete-row` | delete the time block at point |
| `p` | `writing-habit-table-insert-project-above` | insert a project into the legend above the entry at point |
| `P` | `writing-habit-table-insert-project-below` | insert a project below the entry at point, or at the end of the legend |
| `D` | `writing-habit-table-delete-project` | delete the legend entry at point |
| `m` | `writing-habit-table-move-activities` | write each section's activity letter into its cells, then offer to delete the section rows |
| `s` | `writing-habit-table-update-legend` | give every code in the grid a legend row |
| `N` | `writing-habit-table-sort-by-number` | order the legend, or the semester list at point, by project number |
| `t` | `writing-habit-table-split-legend` | split each legend description into a project name and a `YYYY-MM-DD` deadline |
| `l` | `writing-habit-table-copy-to-legend` | copy the semester-list project at point into this week's legend |
| `C` | `writing-habit-table-mark-completed` | move the current-semester project at point to the completed list |
| `S` | `writing-habit-table-sort-by-number` | the same command, listed under "Semester lists" |
| `r` | `writing-habit-table-report` | show the name, clashes, totals, and legend in a side window |
| `c` | `writing-habit-table-rename-file` | rename the file to the canonical code of its grid |
| `n` | `writing-schedule-new-week-from-template` | start this week from a template |
| `o` | `writing-schedule-open-recent` | open the most recent working table |

`M-<up>` and `M-<down>` move the block or legend entry at point without the
menu. Elsewhere in the buffer they keep their org meaning.

### Inserting a time block

Put point on a time block or a section header and insert above or below. The
prompt offers a block of the same length placed flush against its neighbour, so
a block inserted below 05:45-07:15 is offered as 07:15-08:45. Beside a section
header, the nearest block of the neighbouring section serves as the model. The
new line copies the column widths of the line beside it, and it copies how that
line pads its time, so a table whose Time column is padded on the left stays
that way. The file gains exactly one line.

| Where you insert | The new block belongs to |
|------------------|--------------------------|
| above or below a time block | that block's section |
| below a section header | that header's section |
| above a section header | the section before the header |

### Moving a block or a project

A block trades places with the grid row beside it, and point stays in the same
column, so pressing the key again keeps moving the same block. A section header
is a grid row too, so a block can move into the next section, and the echo area
says so, for example `Moved the 05:45-07:15 block down into Rewriting`. The
section decides the activity the block counts toward. A block cannot rise above
the first section header, and nothing moves past the top or bottom of the grid.

A legend entry trades places with the entry beside it and never leaves the
legend. The order matters when a code is defined twice, because both the editor
and the scheduler keep the first definition.

Both moves swap lines rather than rewrite them, so a move up undoes a move down
byte for byte.

### Deleting a time block

Put point on a time block and press `d` in the menu, or run
`writing-habit-table-delete-row`. A row that still holds project codes asks
first and lists them. An empty row goes at once. Point lands on the grid row
that took its place, in the same cell, or on the last grid row when the last
row went. A section header is never deleted, because the blocks under it would
silently join the section above and change the activity they count toward.

The line is removed and every other line is left alone, so the file is one line
shorter. The legend keeps every entry you typed. A blank entry that a sync
added for a code no cell uses any more is dropped, as when the cell is cleared.

### Inserting a project

The prompt for the code starts with the first code that neither the legend nor
the grid uses yet. The single letters come first, then the digits `0` to `9`,
and once those 36 are all taken the prompt offers `AA`, `AB`, and so on to `ZZ`,
which makes room for 712 projects. A grid cell takes a digit or a two-letter
code just as it takes a letter, and completion offers it. A code is a single
digit, or a capital letter followed by up to three capitals or digits. A legend
row whose label reads as a time, such as `5:00-06:30`, is a time block and not
project 5. A code the legend already defines is refused.

The prompt then asks for the project name, such as `0201dusp1`, and for its
deadline as `YYYY-MM-DD`. Leave the deadline empty when there is none. The two
are written into the legend as `C: 0201dusp1 2026-11-01`, with the deadline
last, the same form the Python version writes. The risk tag is one of `none`,
`safe`, and `risky`, and the default activity is one of `none`, `generative`,
`editing`, and `support`.

### Deleting a project

Put point on a legend entry and press `D` in the menu, or run
`writing-habit-table-delete-project`. A project that cells of the grid still use
asks first and says how many cells, because those cells then show as not in the
key. Point lands on the entry that took its place. The next sync of the legend,
by `s` in the menu, adds back a blank entry for every code the grid still uses,
so clear those cells first when the project should go for good. Deleting a
project never changes the grid, the totals, or the canonical name.

### Choosing the activity of each block

The activity of a block comes from the first of these that is present.

| Source | Written as | Example |
|--------|------------|---------|
| A letter in the cell | lowercase `g`, `e`, or `s` before the code | `eA` is editing on A |
| The project's default | a tag in the legend description | `E: email @support` |
| The section header | a header row above the block | `Rewriting:` |
| Nothing at all | | counted as generative |

A letter counts only when it is a lowercase `g`, `e`, or `s` followed by a
capital, so a cell typed as `ga` is still the code `GA`. The scheduler applies
the same rule, so the calendar, the dated schedule, and the plan importer agree
with the buffer. Eldoc ends with the activity and where it came from, as in
`editing (from the cell)`. The report lists the cells whose letter overrides
their section, the default activities, and any block counted as generative
because it has none of the four.

With letters in the cells, a table needs no section rows, and one time slot can
hold different activities on different days. `m` in the menu, or
`writing-habit-table-move-activities`, writes each section's letter into the
bare cells under it and then asks whether to delete the section rows. With a
prefix argument it deletes them without asking. A cell that has a letter keeps
it, and a cell whose project has a default activity stays bare. Every block
keeps its activity, and the canonical name does not change.

For a smaller starter table, run `C-u M-x writing-schedule-insert-template`
from `writing-schedule.el` 0.4.0. It offers three-block days for a morning
writer, an evening writer, and a split day.

### Renaming to the canonical name

The tracker groups weeks by the schedule code captured at plan import, and that
code comes from the file name. A table whose name has drifted from its grid is
filed under the wrong plan shape, and the seasons dashboard then groups it with
the wrong weeks. `writing-habit-table-rename-file` saves the buffer if you
agree, renames the file to the code of its grid, and refuses to overwrite a
different file. A week has no canonical name when a cell holds a code of more
than one letter, and the report says which code is at fault. Everything else
reads a two-letter code whole, including the scheduler, the plan importer, the
totals, and eldoc, so a legend with more than 26 projects is fine. Only a week
whose cells use the two-letter codes loses its file name.

## Deadlines and sorting the legend

`t` in the menu, `writing-habit-table-split-legend`, rewrites each legend
description as a project name followed by an ISO deadline. A description such
as `0104BaGquadShelxD November 15` becomes `0104BaGquadShelxD 2026-11-15`. A
month and day with no year take the current year, unless that puts them more
than about two months in the past, when they take the next year. A date led by
`submitted` or `sent` records something done, not a deadline, so it is dropped
rather than used. The command shows every change first and asks before it
writes. The rule matches `tidy_description` in the Python package line for
line.

`N` in the menu, `writing-habit-table-sort-by-number`, orders the legend by the
four-digit project number at the start of each project name. Projects without
a number, such as `email`, keep their order after the numbered ones. Only the
legend rows trade places, so the grid and the rules stay where they are. With
point in a semester list, `N` sorts that list instead.

## The semester project lists

The weekly legend holds only this week's projects. Longer horizons live in
named org tables after the weekly table, which the scheduler never reads:

| Name | Holds |
|------|-------|
| `current-semester-projects` | every project of this semester |
| `completed-this-semester` | projects finished this semester |
| `next-semester-projects` | next semester |
| `next-next-semester-projects` | the semester after that |
| `next-next-next-semester-projects` | three semesters out |
| `next-next-next-next-semester-projects` | four semesters out |

```org
#+NAME: current-semester-projects
| H: 0104BaGquadShelxD 2026-11-15 :safe: |
| - 0920ReviewForJBC                    |
```

Each row has the legend's one-cell form. A row that has no code yet starts
with `-`. Put point on a row and use the "Semester lists" column of the menu.
`l` copies the project into this week's legend, keeping its code when the
legend does not use it and taking the first free code otherwise. `C` moves a
row of the current list to the completed list in one step and creates that
list when it is missing. `S` sorts the list at point by project number. The
lists are the same tables the Python GUI shows as tabs beside "This week", so
either tool edits them.

## Project tables in columns

The legend and the lists may also be kept as tables with one column per
field, the layout the Python editor writes:

```org
#+NAME: This week
| Code | Project Number | Project Name | Deadline   | Risk tag | Default activity |
|------+----------------+--------------+------------+----------+------------------|
| B:   | 1006           | AIrxOpt      | 2026-12-15 | :safe:   | editing          |

#+NAME: current-semester-projects
#+CAPTION: Fall 2026 projects
| Code | Project Number | Project Name | Deadline   | Risk tag | Default activity |
|------+----------------+--------------+------------+----------+------------------|
| -    | 1452           | PHFSboXt     | 2027-03-01 | :risky:  |                  |

#+NAME: grasscatcher
#+CAPTION: Grasscatcher
| Code | Project Number | Project Name | Priority | Task               |
|------+----------------+--------------+----------+--------------------|
| B:   | 1006           | AIrxOpt      | [#A1]    | Read the reviews.  |
```

A table is read in columns when its first row starts with `Code`. The
`This week` table is the legend, so the week grid needs no legend rows
below it. The `#+NAME:` line holds the list's key, and the `#+CAPTION:`
line holds the title of its tab. A `#+NAME:` that holds a plain title,
such as `Current semester projects`, is found under its key too.

The mode reads every table into the one-cell model and keeps track of
which file line holds each row, so the commands work on the rows you
see. A file you did not change saves byte for byte. After an edit, the
week grid is written as before and every project table is written back
in columns, with its caption kept. writing-schedule 0.6.0 or newer reads
the `This week` table when it builds the calendar, and a Default activity
there counts as the `@` tag of the legend.

## How edits keep the file intact

Every edit goes through the model in `writing-habit-table.el`, a buffer-free
port of the Python `weekly_table.py`. The model keeps every line of the file
and classifies each line the way the scheduler parser does. A cell edit
rewrites one line, a value that fits its column keeps the table aligned, and a
longer value widens only its own slot. The buffer is then updated with
`replace-buffer-contents`, which changes only the text that differs, so undo,
point, and marks behave as after a hand edit. Realign with `C-c C-c` when you
want to.

`test/parity/run.sh` checks the Elisp model against the Python one over every
shipped table, reading and editing each table in every supported way.
