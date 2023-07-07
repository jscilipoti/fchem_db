program main
  use substance

  implicit none
  type(substances) :: compound

  call compound%read("Water")
  print *, compound%molecular_weight%value
  print *, compound%Acentric_factor%value
  print *, compound%Ideal_gas_heat_capacity__RPP%value
  print *, compound%Name%value


end program main