module list_tools

  implicit none
  logical, parameter :: ascending = .true.
  logical, parameter :: descending = .false.

  interface is_sorted
     module procedure is_sorted_int, is_sorted_real
  end interface

contains

  integer function is_sorted_int(arr, sort_ord)
    integer, dimension(:) :: arr
    integer :: j, check = 0
    logical, optional:: sort_ord
    
    do j=1, size(arr)-1
       if( arr(j) > arr(j+1)) then
          check = check + 1
       end if
    end do

    if(present(sort_ord)) then
       if(sort_ord .eqv. ascending) then
          if(check == size(arr) - 1) then
             write(*,*) "the array is not in ascending order"
          else if(check == 0) then
             write(*,*) "the array is in ascending order"
          end if
       else if (sort_ord .eqv. descending) then
          if(check == size(arr) - 1) then
             write(*,*) "the array is in descending order"
          else if(check == 0) then
             write(*,*) "the array is not in descending order"
          end if
       end if
    end if
    
    is_sorted_int = check
  end function is_sorted_int
     
  integer function is_sorted_real(arr, sort_ord)
    real, dimension(:) :: arr
    integer :: j, check = 0
    logical, optional:: sort_ord
    
    do j=1, size(arr)-1
       if( arr(j) > arr(j+1)) then
          check = check + 1
       end if
    end do

    if(present(sort_ord)) then
       if(sort_ord .eqv. ascending) then
          if(check == size(arr) - 1) then
             write(*,*) "the array is not in ascending order"
          else if(check == 0) then
             write(*,*) "the array is in ascending order"
          end if
       else if (sort_ord .eqv. descending) then
          if(check == size(arr) - 1) then
             write(*,*) "the array is in descending order"
          else if(check == 0) then
             write(*,*) "the array is not in descending order"
          end if
       end if
    end if
    
    is_sorted_real = check
  end function is_sorted_real

end module
