module list_tools
  use list_types
  
  implicit none
  logical, parameter :: ascending = .true.
  logical, parameter :: descending = .false.
  logical, parameter :: bykey = .true.
  logical, parameter :: byvalue = .false.
  public :: is_sorted, swap, next_prime
  
  interface is_sorted
     module procedure is_sorted_int, is_sorted_real, is_sorted_pair
  end interface

contains
  ! trivial implementation of the function
  ! that return the next prime number greater than
  ! the given argument n
  integer function next_prime(n)
    integer :: n, count, nprime
    logical :: is_prime

    if(n == 1) then
       next_prime = 2
    else
       
       ! count = 2
       nprime = n + 1
       
       do
          is_prime = .true.

          if(mod(nprime, 2) == 0) then
             do count=2,n
                if(mod(nprime, count) == 0)then
                   is_prime = .false.
                end if
             end do
          else
             do count=3,n,2
                if(mod(nprime, count) == 0)then
                   is_prime = .false.
                end if
             end do
          end if
          
          if(is_prime .eqv. .true.) then
             exit
          else
             nprime = nprime + 1
          end if
       end do
       next_prime = nprime
    endif
  end function next_prime
  
  ! pick two randomly chosen elements in array 'dat'
  ! and swap them. do this 'count' times.
  SUBROUTINE swap(dat,count)
    IMPLICIT NONE
    REAL, DIMENSION(:),INTENT(inout) :: dat
    INTEGER, INTENT(in) :: count
    REAL,DIMENSION(2) :: rval
    INTEGER :: i,num,i1,i2
    REAL :: tmp
    num = SIZE(dat,1)
    DO i=1,count
       ! pick two elements at random
       CALL RANDOM_NUMBER(rval)
       rval = rval*REAL(num)+0.5
       i1 = INT(rval(1))
       i2 = INT(rval(2))
       ! paranoia check to avoid out-of-bounds access
       IF ((i1 < 1) .OR. (i1 > num) .OR. (i2 < 1) .OR. (i2 > num)) CYCLE
       ! swap the elements
       tmp = dat(i1)
       dat(i1) = dat(i2)
       dat(i2) = tmp
    END DO
  END SUBROUTINE swap
    
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
          if(check == 0) then
             write(*,*) "the array is in ascending order"
          else
             write(*,*) "the array is not in ascending order"
          end if
       else if (sort_ord .eqv. descending) then
          if(check == size(arr) - 1) then
             write(*,*) "the array is in descending order"
          else
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
          if(check == 0) then
             write(*,*) "the array is in ascending order"
          else
             write(*,*) "the array is not in ascending order"
          end if
       else if (sort_ord .eqv. descending) then
          if(check == size(arr) - 1) then
             write(*,*) "the array is in descending order"
          else
             write(*,*) "the array is not in descending order"
          end if
       end if
    end if
    
    is_sorted_real = check
  end function is_sorted_real
  
  integer function is_sorted_pair(arr, sort_ord, k_v)
    type (mytype), dimension(:) :: arr
    integer :: j, check = 0
    logical, optional :: sort_ord
    logical, optional :: k_v
    
    if(present(k_v)) then
       if(k_v .eqv. bykey) then
          do j=1, size(arr)-1
             if( arr(j) % key > arr(j+1) % key) then
                check = check + 1
             end if
          end do
       else ! k_v .eqv. byvalue
          do j=1, size(arr)-1
             if( arr(j) % val > arr(j+1) % val) then
                check = check + 1
             end if
          end do
       end if
    else ! k_v not present
       ! by default is performed the check on the values
       do j=1, size(arr)-1
          if( arr(j) % val > arr(j+1) % val) then
             check = check + 1
          end if
       end do
    end if
    
    if( present(sort_ord) .and. present(k_v) ) then
       if(sort_ord .eqv. ascending) then
          if(k_v .eqv. bykey) then
             if(check == 0) then
                write(*,*) "the array keys are in ascending order"
             else
                write(*,*) "the array keys are not in ascending order"
             end if
          else ! k_v .eqv. byvalue
             if(check == 0) then
                write(*,*) "the array values are in ascending order"
             else
                write(*,*) "the array values are not in ascending order"
             end if
          end if
       else if (sort_ord .eqv. descending) then
          if(k_v .eqv. bykey) then
             if(check == size(arr) - 1) then
                write(*,*) "the array keys are in descending order"
             else
                write(*,*) "the array keys are not in descending order"
             end if
          else ! k_v .eqv. byval
             if(check == size(arr) - 1) then
                write(*,*) "the array values are in descending order"
             else
                write(*,*) "the array values are not in descending order"
             end if
          end if
       end if
    else if(present(sort_ord)) then
       if(sort_ord .eqv. ascending) then
          if(check == 0) then
             write(*,*) "the array values are in ascending order"
          else
             write(*,*) "the array values are not ascending order"
          end if
       else if (sort_ord .eqv. descending) then
          if(check == size(arr) - 1) then
             write(*,*) "the array values are in descending order"
          else if(check == 0) then
             write(*,*) "the array values are not in descending order"
          end if
       end if
    else
       write(*,*) "sort_ord or k_v missing"
       write(*,*) "performed check by values"
    end if
    
    is_sorted_pair = check
  end function is_sorted_pair
  
end module list_tools
