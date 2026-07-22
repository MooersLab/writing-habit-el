"""Sphinx configuration for the writing-habit (Emacs Lisp) documentation."""

from __future__ import annotations

# -- Project information ------------------------------------------------------

project = "writing-habit-el"
author = "Blaine Mooers"
copyright = "2026, Blaine Mooers"

release = "0.1.0"
version = ".".join(release.split(".")[:2])

# -- General configuration ----------------------------------------------------

# This package is Emacs Lisp, not Python, so there is no autodoc. The site is
# a set of hand-written MyST pages, and the function reference is maintained by
# hand in function-reference.md.
extensions = [
    "myst_parser",
]

source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
}

# _diagrams holds the DOT sources for the images, not documentation pages.
exclude_patterns = ["_build", "_diagrams", "Thumbs.db", ".DS_Store"]

# -- MyST ---------------------------------------------------------------------

myst_enable_extensions = ["colon_fence", "deflist"]
myst_heading_anchors = 3

# -- HTML output --------------------------------------------------------------

html_theme = "sphinx_rtd_theme"
html_static_path = ["_static"]
html_title = f"writing-habit-el {release}"
