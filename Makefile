# Makefile for the writing-habit Emacs Lisp package.
#
#   make test      run the ERT suite in batch
#   make compile   byte-compile every module, treating warnings as failures
#   make clean     remove byte-compiled files
#
# The plan-import tests need writing-schedule.el.  Point WRITING_SCHEDULE_DIR
# at its checkout to include them; otherwise those tests skip themselves.
#
#   make test WRITING_SCHEDULE_DIR=/path/to/writing-schedule

EMACS ?= emacs
WRITING_SCHEDULE_DIR ?=

MODULES = writing-habit-db.el \
          writing-habit-name.el \
          writing-habit-plan.el \
          writing-habit-track.el \
          writing-habit-compare.el \
          writing-habit-report.el \
          writing-habit-dashboard.el \
          writing-habit-context.el \
          writing-habit-seasons.el \
          writing-habit.el

TESTS = test/writing-habit-name-tests.el \
        test/writing-habit-db-tests.el \
        test/writing-habit-plan-tests.el \
        test/writing-habit-track-tests.el \
        test/writing-habit-compare-tests.el \
        test/writing-habit-dashboard-tests.el \
        test/writing-habit-main-tests.el \
        test/writing-habit-seasons-tests.el

LOADPATH = -L . $(if $(WRITING_SCHEDULE_DIR),-L $(WRITING_SCHEDULE_DIR),)
TESTLOAD = $(foreach t,$(TESTS),-l $(t))

.PHONY: test compile clean

test:
	$(EMACS) -Q -batch $(LOADPATH) -l ert $(TESTLOAD) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q -batch $(LOADPATH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(MODULES)

clean:
	rm -f *.elc test/*.elc
