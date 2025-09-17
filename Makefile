# Convenience module.
# Note that `rebar3 as` can't be used in rebar aliases - the make targets
# below act as are a workaround.
#
# This file also acts as informal list of useful rebar3 commands.
# ------------------------------------------------------------------------------

# Compile the lib.
.PHONY: compile
compile:
	rebar3 compile

# Build the documentation.
.PHONY: doc
doc:
	rebar3 ex_doc

# Run static checks on `src` and `benchmark` code.
# Note: `test` is omitted as eunit and eunit macros cause dialyzer warnings.
.PHONY: check
check:
	rebar3 as default,benchmark check

# Run tests (also compiles `src`).
# This also produces test coverage info, about how much of `src/*.erl` gets tested.
# The `-v` flag prints a summary of the coverage result to the shell,
# details about which lines are covered/uncovered can be found in the result written to file
# in _build/test
.PHONY: test
test:
	rebar3 do eunit, cover -v

# Start an interactive Erlang shell.
.PHONY: shell
shell:
	rebar3 shell

# Compile benchmark code (also compiles `src`).
.PHONY: bcompile
bcompile:
	rebar3 as benchmark compile

# Start an interactive Erlang shell to manually run benchmark modules.
# These are currently:
#   na_benchmark_get:run().
#   na_benchmark_replace:run().
.PHONY: bshell
bshell:
	rebar3 as benchmark shell
