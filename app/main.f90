program main
  use substance

  implicit none
  type(substances) :: compound

  call compound%read("Dichloroacetaldehyde")
  print *, "MW: ",compound%molecular_weight%value
  print *, "Acentric factor: ",compound%Acentric_factor%value
  print *, "Ideal_gas_heat_capacity__RPP: ", compound%Ideal_gas_heat_capacity__RPP%value
  print *, "Name: ", compound%Name%value_str


end program main