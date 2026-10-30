tests="
basic
basicOptions
collision
exposedPorts
fileOut
multipleTops
noTopology
portless
sameName
unusedSerial
"

collision()
{
  run_test "-i $imports" collision
}

exposedPorts()
{
  run_test "-i $imports" exposedPorts
}

portless()
{
  run_test "-i $imports" portless
}

sameName()
{
  run_test "-i $imports" sameName
}

unusedSerial()
{
  run_test "-i $imports" unusedSerial
}
