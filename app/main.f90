program main
  use json_module
  use substance

  implicit none
  type(json_file) :: json
  logical :: found
  real*8 :: i

  ! initialize the class
  call json%initialize()  
  
  ! read the file
  call json%load(filename = 'lecture/db_json/Water.json')
  
  ! print the file to the console
  call json%print()

  call json%get('LibraryIndex.value', i, found)
  if ( .not. found ) stop 1


end program main
