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
| `WH[2 clashes]` in the mode line | the number of overlapping pairs in the week |
| the echo area | the name, due date, and risk of the code in the day cell at point |

The overlap rule is the scheduler's own, because the mode calls
`writing-schedule-overlaps` from the public API of `writing-schedule.el` 0.3.1.
Each range runs from its start up to, but not including, its end, so blocks
that only touch, such as 04:00-05:30 and 05:30-07:00, do not overlap. A range
whose end is not after its start runs past midnight. A clash cell stays red
inside a yellow row, because the clash is the more urgent thing to see.

Press `M-TAB` or `C-M-i` in a day cell to complete a project code from the
legend. The tints refresh after `writing-habit-table-idle-delay` seconds of
idle time.

## Commands

`C-c C-;` opens the table menu. Change the key through
`writing-habit-table-mode-prefix` before the mode first loads.

| Key in the menu | Command | What it does |
|-----------------|---------|--------------|
| `a` | `writing-habit-table-insert-above` | insert an empty block above the block or section header at point |
| `b` | `writing-habit-table-insert-below` | insert an empty block below the block or section header at point |
| `<up>`, `<down>` | `writing-habit-table-move-up`, `-move-down` | move the block or legend entry at point |
| `p` | `writing-habit-table-insert-project-above` | insert a project into the legend above the entry at point |
| `P` | `writing-habit-table-insert-project-below` | insert a project below the entry at point, or at the end of the legend |
| `s` | `writing-habit-table-update-legend` | give every code in the grid a legend row |
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

### Inserting a project

The prompt for the code starts with the first letter that neither the legend
nor the grid uses yet. A code is a capital letter followed by up to three
capitals or digits. A code the legend already defines is refused. The
description may end in a due date such as `Sept 25`, which eldoc reads, and the
risk tag is one of `none`, `safe`, and `risky`.

### Renaming to the canonical name

The tracker groups weeks by the schedule code captured at plan import, and that
code comes from the file name. A table whose name has drifted from its grid is
filed under the wrong plan shape, and the seasons dashboard then groups it with
the wrong weeks. `writing-habit-table-rename-file` saves the buffer if you
agree, renames the file to the code of its grid, and refuses to overwrite a
different file. A week has no canonical name when a cell holds a code of more
than one letter, and the report says which code is at fault.

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
