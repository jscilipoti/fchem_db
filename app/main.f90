program leer_nombres_archivos
    real :: r
    integer :: i,reason,NstationFiles,iStation
    character(LEN=100), dimension(:), allocatable :: stationFileNames
  
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
    write(*,'(a,I0)') "Number of station files: " , NstationFiles
    allocate(stationFileNames(NstationFiles))
    rewind(31)
    do i = 1,NstationFiles
     read(31,'(a)') stationFileNames(i)
    enddo
    
end program leer_nombres_archivos
