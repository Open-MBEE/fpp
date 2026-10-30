imports=config.fpp,fw.fpp,comps.fpp

basic()
{
  run_test "-i $imports" basic
}

basicOptions()
{
  run_test "-i $imports -n Demo.Gen -s the-model" basic basicOptions
}

fileOut()
{
  $fpp_to_lean -i $imports -o fileOut.out.txt basic.fpp 2>&1 | remove_version | remove_path_prefix > fileOut.err.txt
  remove_version < fileOut.out.txt > fileOut.tmp.txt && mv fileOut.tmp.txt fileOut.out.txt
  cat fileOut.err.txt >> fileOut.out.txt
  compare fileOut
}

multipleTops()
{
  run_test "-i $imports" multipleTops
}

noTopology()
{
  run_test "-i $imports" noTopology
}

