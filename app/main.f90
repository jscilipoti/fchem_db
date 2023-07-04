program main
  use json_module
  use substance

  implicit none
  type(substances) :: compound

  call compound%read("Water")
  write(*,*)compound%molecular_weight

end program main
