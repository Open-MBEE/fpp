imports=config.fpp,fw.fpp,comps.fpp

basic()
{
  update "-i $imports" basic
}

basicOptions()
{
  update "-i $imports -p DemoGen -s the-model" basic basicOptions
}

fileOut()
{
  $fpp_to_sysml -i $imports -o fileOut.out.txt basic.fpp 2>&1 | remove_version | remove_path_prefix > fileOut.ref.txt
  remove_version < fileOut.out.txt >> fileOut.ref.txt
  rm fileOut.out.txt
}

multipleTops()
{
  update "-i $imports" multipleTops
}

noTopology()
{
  update "-i $imports" noTopology
}

