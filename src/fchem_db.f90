module fchem_db
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fchem_db!"
  end subroutine say_hello
end module fchem_db
