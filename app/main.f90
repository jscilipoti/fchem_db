program main
  use substance

  implicit none
  type(substances) :: compound

  call compound%read("Water")
  print *, "Name: ", compound%Name%value_str
  print *, "MW name: ", compound%molecular_weight%name
  print *, "MW: ",compound%molecular_weight%value
  print *, "Tc name: ", compound%Critical_temperature%name
  print *, "Tc: ", compound%Critical_temperature%value
  print *, "Tc units: ", compound%Critical_temperature%units
  print *, "Acentric factor: ",compound%Acentric_factor%value
  print *, "Ideal_gas_heat_capacity__RPP: ", compound%Ideal_gas_heat_capacity__RPP%value
  print *, "Name: ", compound%Name%value_str



end program main