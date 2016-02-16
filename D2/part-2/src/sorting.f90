MODULE sorting
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: simplesort, quicksort, bubblesort, insertionsort, mergesort
CONTAINS
  
  ! pathetically bad sorting algorithm:
  ! loop over all unique pairs and swap the values
  ! if the left element is larger than the right one.
  SUBROUTINE simplesort(dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, i, j
    REAL :: tmp
    
    num = SIZE(dat,1)
    IF (num < 2) RETURN
    DO i=1,num-1
       DO j=i+1,num
          IF (dat(i) > dat(j)) THEN
             tmp = dat(i)
             dat(i) = dat(j)
             dat(j) = tmp
          END IF
       END DO
    END DO
  END SUBROUTINE simplesort

  subroutine mergesort(dat)
    implicit none
    real, dimension(:), intent(inout) :: dat
    real :: tmp
    integer :: num, block_size, j
    logical :: first_cycle
    
    num = size(dat)
    if (num < 2) return
    write(*,*) "within mergesort() function"
    if(mod(num, 2) .ne. 0) then
       write(*,*) "lenght of the array must be even"
       write(*,*) "return"
       return
    endif

    block_size = 2
    first_cycle = .true.

    do while(block_size <= size(dat)/2)
       if(block_size == 2 .and. first_cycle .eqv. .true.) then
          j = 1
          do while(j < num)
             if(dat(j) > dat(j+1)) then
                tmp = dat(j)
                dat(j) = dat(j+1)
                dat(j+1) = tmp
             end if
             j = j + 2
          end do
          first_cycle = .false.
          write(*,*) "block_size = ", block_size
          write(*,*) dat
       else
          j = 1
          
          do while(j < num)
             if(dat(j+block_size-1) > dat(j+block_size)) call mymerge(dat, block_size, j)
             write(*,*) "block_size = ", block_size, "j = ", j
             write(*,*) dat
             j = j + 2*block_size
          end do
          block_size = 2*block_size
       end if
    end do
  end subroutine mergesort
  
  subroutine mymerge(dat, bsize, ind)
    implicit none
    real, dimension(:), intent(inout) :: dat
    integer :: bsize, ind, left, right, j
    real, dimension(2*bsize) :: tmp_dat
    ! write(*,*) "within mymerge() function"
    left = ind
    right = ind + bsize
    j = 1
    
    ! do while(j <= 2*bsize)
    do while(left < ind+bsize .and. right < ind+2*bsize)
       if(dat(left) > dat(right)) then
          tmp_dat(j) = dat(right)
          right = right + 1
          j = j + 1
       else
          tmp_dat(j) = dat(left)
          left = left + 1
          j = j + 1
       end if
    end do

    do while(left < ind+bsize)
       tmp_dat(j) = dat(left)
       left = left + 1
       j = j + 1       
    end do

    do while(right < ind+2*bsize)
       tmp_dat(j) = dat(right)
       right = right + 1
       j = j + 1
    end do
    
    j = 1
    ! write(*,*) "copy values"
    do while(j <= 2*bsize .and. (ind + j - 1) <= size(dat))
       dat(ind + j - 1) = tmp_dat(j)
       j = j + 1
    end do
  end subroutine mymerge
  
  ! implementation of the insertion sort algorithm
  subroutine insertionsort(dat)
    implicit none
    real, dimension(:), intent(inout) :: dat
    integer :: num, i, j
    real :: tmp

    num = size(dat, 1)
    if (num < 2) return

    do i=2,num
       tmp = dat(i)
       j = i - 1
       do while(j >= 0 .and. tmp < dat(j))
          dat(j+1) = dat(j)
          j = j - 1
       end do
       dat(j+1) = tmp       
    end do
    
  end subroutine insertionsort
  
  ! implementation of the bubblesort algorithm
  ! sorting is alwais by ascending values
  subroutine bubblesort(dat)

    implicit none
    real, dimension(:), intent(inout) :: dat
    integer :: num, i
    real :: tmp
    logical :: swap
    
    num = size(dat, 1)
    if (num < 2) return

    swap = .true.
    
    do while( swap .eqv. .true. )
       swap = .false.

       do i=1,num-1
          if(dat(i) > dat(i + 1)) then
             tmp = dat(i)
             dat(i) = dat(i+1)
             dat(i+1) = tmp
             swap = .true.
          end if
       end do
    end do
    
  end subroutine bubblesort
  
  ! quicksort implementation via recursion
  ! top-level takes whole array, recursions work on subsets.
  ! pick pivot element and recursively sort the two sublists.
  SUBROUTINE quicksort(dat)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: num, p
    
    num = SIZE(dat,1)
    IF (num < 2) RETURN
    
    p = select_pivot(dat,1,num)
    CALL quicksort_recurse(dat,1,p-1)
    CALL quicksort_recurse(dat,p+1,num)
  END SUBROUTINE quicksort
  
  RECURSIVE SUBROUTINE quicksort_recurse(dat,left,right)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER,INTENT(in) :: left, right
    INTEGER :: p
    
    IF (left < right) THEN
       p = select_pivot(dat,left,right)
       CALL quicksort_recurse(dat,left,p-1)
       CALL quicksort_recurse(dat,p+1,right)
    END IF
  END SUBROUTINE quicksort_recurse
  
  ! core step in quicksort. pick pivot value. then swap
  ! array elements so that smaller values are to the left of
  ! it and all larger values to the right. store pivot in
  ! the remaining spot. this element is now in its final location.
  ! return the index of the pivot element.
  ! The choice of the pivot is arbitrary, but crucial for getting
  ! good performance with presorted data.
  RECURSIVE FUNCTION select_pivot(dat,left,right) RESULT(i)
    IMPLICIT NONE
    REAL,DIMENSION(:),INTENT(inout) :: dat
    INTEGER :: i, j, right, left
    REAL :: tmp, pivot
    
    ! this is the classic choice of pivot element, assuming random data
    ! pivot = dat(right)
    ! an element in the middle is a much better choice for presorted data
    pivot = dat((left+right)/2)
    i = left
    DO j=left,right-1
       IF (pivot > dat(j)) THEN
          tmp = dat(i)
          dat(i) = dat(j)
          dat(j) = tmp
          i = i+1
       END IF
    END DO
    dat(right) = dat(i)
    dat(i) = pivot
  END FUNCTION select_pivot
  
END MODULE sorting
