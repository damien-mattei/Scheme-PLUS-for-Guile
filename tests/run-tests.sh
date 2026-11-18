#!/bin/bash
# run-tests.sh : the tests of Scheme+ for Guile
# each tests/<name>+.scm (Scheme+) is converted by curly-infix2prefix4guile.scm (sweet-infix2prefix4guile.scm for the
# files <name>-sweet+.scm , written in sweet-expressions) , compiled and run under SRFI 64 ; the failed checks are
# printed. Then the test suite of the sweet-expression reader (run-sweet-testsuite.scm) when all the tests are run.
# Usage : tests/run-tests.sh [name ...]   (default : all the tests)
G=$(cd "$(dirname "$0")/.." && pwd)
OUT=$(mktemp -d)
names="$*"
[ -z "$names" ] && names=$(cd $G/tests && ls *+.scm | sed 's/+\.scm$//')
status=0
for n in $names; do
  case $n in *-sweet) parser=sweet-infix2prefix4guile.scm ;; *) parser=curly-infix2prefix4guile.scm ;; esac
  if ! guile -L $G -e main -s $G/$parser $G/tests/$n+.scm > $OUT/$n.scm 2> $OUT/$n.err; then
    echo "$n : conversion failed" ; grep -v "^;;;" $OUT/$n.err | tail -5 ; status=1 ; continue
  fi
  (cd $G/tests && guile -L $G $G/tests/run-test.scm $n $OUT/$n.scm 2>&1 | grep -v "^;;;") || status=1
done
rm -rf $OUT
if [ -z "$*" ]; then
  (cd $G/tests && guile -L $G run-sweet-testsuite.scm 2>&1 | grep "tests ,") || status=1
fi
exit $status
