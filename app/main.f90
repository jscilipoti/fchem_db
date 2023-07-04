program main
  use json_module
  use substance

  implicit none
  type(substances) :: compound

  compound%search("water")
  write(*,*)compound%molecular_weight

end program main
