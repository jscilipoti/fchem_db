program main
  use substance

  implicit none
  type(substances) :: compound

  call compound%read("Water")
  print *, compound%molecular_weight
  print *, compound%Acentric_factor
  print *, compound%Ideal_gas_heat_capacity__RPP


end program main
