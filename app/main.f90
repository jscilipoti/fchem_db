program main
  use substance
  use constants, only: database_dir

  implicit none
  type(substances) :: compound

  ! defino database_dir para usarlo en este paquete
  ! Si no lo defino, el código supone que está en:
  ! build/dependencies/fchem_db/files/db_json
  database_dir = "files/db_json/"


  call compound%read("1,2-butadiene")
  print *, "Name: ", compound%Name%value_str
  print *, "Unifac: ", compound%UNIFAC%group
  print *, "MW name: ", compound%molecular_weight%name
  print *, "MW: ",compound%molecular_weight%value
  print *, "Tc name: ", compound%Critical_temperature%name
  print *, "Tc: ", compound%Critical_temperature%value
  print *, "Tc units: ", compound%Critical_temperature%units
  print *, "Acentric factor: ",compound%Acentric_factor%value
  print *, "Ideal_gas_heat_capacity__RPP: ", compound%Ideal_gas_heat_capacity__RPP%value
  print *, "Solid_density eq: ", compound%Solid_density%eqno
  print *, "Solid_density A: ", compound%Solid_density%A

end program main