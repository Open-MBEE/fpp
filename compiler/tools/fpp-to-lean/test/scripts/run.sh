#!/bin/sh

. $COMPILER_ROOT/scripts/test-utils.sh

fpp_to_lean=$COMPILER_ROOT/bin/fpp-to-lean

# The generated header and the Lean module name the fpp version; the reference output must not
remove_version()
{
  sed -e 's/(fpp [^)]*)/(fpp [version])/' -e 's/^def fppVersion : String := .*/def fppVersion : String := "[version]"/'
}

compare()
{
  outfile=$1
  diff -u $outfile.ref.txt $outfile.out.txt > $outfile.diff.txt 2>&1
}

run_test()
{
  args=$1
  infile=$2
  if test -n "$3"
  then
    outfile=$3
  else
    outfile=$infile
  fi
  $fpp_to_lean $args $infile.fpp 2>&1 | remove_version | remove_path_prefix > $outfile.out.txt
  compare $outfile
}

. ./tests.sh

. ./run.sh
run_suite $tests
