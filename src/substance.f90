module substance
  use json_module
  use constants, only: pr
  implicit none
  
  type :: substances
    real(pr) :: molecular_weight
  contains 
    procedure :: read1 => read_db
  end type substances

contains

  subroutine read_db(self, name)
    class(substances), intent(inout) :: self
    character(len=*), intent(in) :: name
    
    type(json_file) :: json
    logical :: found
    real(pr) :: MW


    ! initialize the class
    call json%initialize()  
  
    ! read the file
    call json%load(filename = 'lecture/db_json/'//name//'.json')
    
    call json%get('MolecularWeight.value', self%molecular_weight, found)
   ! self%molecular_weight = MW
   ! write(*,*) self%molecular_weight

   ! if ( .not. found ) stop 1    
    ! print the file to the console
    !call json%print()
  end subroutine read_db
end module substance
