module list_tools

  implicit none
  integer :: j, check = 0

contains
  integer function is_sorted(arr)
    real, dimension(:) :: arr
    do j=1, size(arr)-1
       if( arr(j) > arr(j+1)) then
          check = check + 1
       end if
    end do
    
    is_sorted = check
  end function is_sorted

end module
