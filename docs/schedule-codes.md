# Schedule codes and the name command

A weekly table can be named by a compact code that encodes the whole week, for
example `4gAAeAsA-gWW.org`. A folder of schedules named this way is
self-documenting and sorts sensibly. The `writing-habit-name` command decodes a
code and checks its project letters against a table legend. It needs no database
and no third-party package, so it stands on its own. The full specification
lives in `docs/table-file-naming-rules.org`, and this page is the working
summary.

## The alphabet

The code uses three character classes that never overlap, so a day needs no
separators inside it. Lowercase letters name activities, namely `g` for
generative, `e` for editing, and `s` for support. Uppercase letters name
projects, and one uppercase letter is one block on that project. A digit at the
start of a group is the count of consecutive days that share the pattern. A
hyphen marks the boundary where the daily pattern changes. A lone `o` is an open
day that carries no blocks.

## How to read a code

Split the code on hyphens into day-groups, then read the groups in order onto
the week starting at Monday. A day-group is an optional leading count followed
by a day-pattern, and the count defaults to one. A day-pattern is a sequence of
activity runs, where each run opens with an activity letter and is followed by
one project letter per block. Order the blocks within a day by time of day,
generative first by convention, then editing, then support.

![The code 4gAAeAsA-gWW read token by token into a week of blocks.](imgs/schedule-code.png)

The figure decodes `4gAAeAsA-gWW`. The leading `4` sets Monday through Thursday
to the pattern `gAAeAsA`, which is two generative blocks on A, one editing block
on A, and one support block on A. After the hyphen, `gWW` sets Friday to two
generative blocks on W. Days beyond the last group are open by default, so the
week ends on Friday without a trailing `o`.

## Decode a code

Run `M-x writing-habit-name` and enter the code. The command shows the week day
by day, then a summary of the blocks by activity and by project. With a prefix
argument it also prompts for a table file and checks the code's project letters
against that table's legend. From a shell:

```sh
emacs --batch -l writing-habit -f writing-habit-batch name 4gAAeAsA-gWW --table my-week.org
```

A letter resolves as an exact match when the legend has that code, as an alias
when exactly one legend code starts with the letter, and as ambiguous or unknown
otherwise. An unresolved letter is reported, so you catch a letter that no
legend describes before you rely on the name.

## Multi-letter projects

The code assumes a project is a single uppercase letter, because a run such as
`gAA` depends on each letter being one block. Several legend codes have two
letters, for example `EM` for email and `TT` for teaching, so give every
multi-letter project a single-letter alias for the file name and keep the full
code in the table legend. Reserve digits for counts, so a project alias never
contains a digit.
