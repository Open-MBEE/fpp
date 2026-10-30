#!/bin/sh

. $COMPILER_ROOT/scripts/test-utils.sh

fpp_to_sysml=$COMPILER_ROOT/bin/fpp-to-sysml

remove_version()
{
  sed -e 's/(fpp [^)]*)/(fpp [version])/' -e 's/^def fppVersion : String := .*/def fppVersion : String := "[version]"/'
}

update()
{
  args=$1
  infile=$2
  if test -n "$3"
  then
    outfile=$3
  else
    outfile=$infile
  fi
  $fpp_to_sysml $args $infile.fpp 2>&1 | remove_version | remove_path_prefix > $outfile.ref.txt
}

for file in `find . -name '*.ref.txt'`
do
  rm $file
done

. ./tests.sh

. ./update-ref.sh

for t in $tests
do
  echo "updating ref output for $t"
  $t
done
