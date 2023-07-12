module constants
    use iso_fortran_env, only: real32, real64, real128
    implicit none
    
    integer, parameter :: pr = real32
    integer, parameter :: properties_number = 60
    character(len=:), allocatable :: database_dir

end module constants