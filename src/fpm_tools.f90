module fpm_tools
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fpm_tools!"
  end subroutine say_hello
end module fpm_tools
