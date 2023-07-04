program main
  use json_module
  use substance

  implicit none
  type(substances) :: comp

  call comp%read1("Water")
  write(*,*)comp%molecular_weight




end program main
