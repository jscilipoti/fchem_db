program ejemplo
  implicit none
  integer :: a, b, c
  namelist /datos/ a, b, c
  
  open(unit=10, file='app/datos.txt', status='old')
  read(10, nml=datos)
  close(10)
  
  print *, "Los valores leídos son:", a, b, c
end program ejemplo