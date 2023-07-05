module substance
  use json_module
  use constants, only: pr
  implicit none
  
  type :: substances
    real(pr) :: molecular_weight
  contains 
    procedure :: read => read_db
    !procedure :: search => search_compound
  end type substances

contains

  subroutine read_db(self, name)
    class(substances), intent(inout) :: self
    character(len=*), intent(in) :: name
    
    type(json_file) :: json
    logical :: found
    real(pr) :: MW

    if (search_compound(name)) then
      ! initialize the class
      call json%initialize()  
  
      ! read the file
      call json%load(filename = 'lecture/db_json/'//name//'.json')
    
      call json%get('MolecularWeight.value', self%molecular_weight, found)
    else 
      write(*,*) "Compound not found"
    end if
  end subroutine read_db

  logical function search_compound(name) result(found)
  !  class(substances), intent(in) :: self
    character(len=*), intent(in) :: name
    real :: r
    integer :: i,reason,NstationFiles,iStation
    character(100) :: stationFileNames, nameint
    !logical :: found

    nameint = name//".json"
    ! get the files
    call system('ls ./lecture/db_json > fileContents.txt')
    open(31,FILE='fileContents.txt',action="read")
    !how many
    i = 0
    do
      read(31,FMT='(a)',iostat=reason) r
      if (reason/=0) EXIT
      i = i+1
    end do
    
    NstationFiles = i

    rewind(31)
    found = .false.
    do i = 1,NstationFiles
      read(31,'(a)') stationFileNames
      if (stationFileNames == nameint) then
        found = .true.
        return
      end if
    enddo
    
  end function search_compound  
end module substance
