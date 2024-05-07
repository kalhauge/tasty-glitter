This file shows how to use the tasty-glitter-test test-suite:
```shell
$> cabal run tasty-glitter-test -- --help
Mmm... tasty test suite

Usage: tasty-glitter-test [-p|--pattern PATTERN] [-t|--timeout DURATION] 
                          [-l|--list-tests] [-j|--num-threads NUMBER] 
                          [-q|--quiet] [--hide-successes] 
                          [--color never|always|auto] [--ansi-tricks ARG] 
                          [--changed-files ARG]

Available options:
  -h,--help                Show this help text
  -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
                           expression
  -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
                           default: s)
  -l,--list-tests          Do not run the tests; just print their names
  -j,--num-threads NUMBER  Number of threads to use for tests execution
                           (default: # of cores/capabilities)
  -q,--quiet               Do not produce any output; indicate success only by
                           the exit code
  --hide-successes         Do not print tests that passed successfully
  --color never|always|auto
                           When to use colored output (default: auto)
  --ansi-tricks ARG        Enable various ANSI terminal tricks. Can be set to
                           'true' or 'false'. (default: true)
  --changed-files ARG      choose the strategy for finding changed files.
                           (default: unstaged)
```
