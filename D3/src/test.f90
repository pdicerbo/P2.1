program test
  use list_tools
  implicit none

  integer :: i, ret
  
  i = 971
  ret = next_prime(i)
  write(*,*) ret
end program test
