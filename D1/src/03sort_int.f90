program read_integer_from_stdin
  implicit none

  integer :: arr_len = 0
  integer :: i = 1, read_checksum, checksum = 0, is_s
  integer, allocatable, dimension(:) :: input_data

  interface
     integer function is_sorted(arr)
       implicit none
       integer :: j, check
       integer, dimension(:) :: arr
     end function is_sorted
  end interface
  
  read(5,*) arr_len
  
  write(*,*) 
  write(*,*) "the array lenght is: ", arr_len

  allocate(input_data(arr_len))

  read(5, *) input_data
  read(5, *) read_checksum

  is_s = is_sorted(input_data)

  if(is_s == 0 .or. is_s == arr_len - 1) then
     write(*,*) "the array loaded is sorted. is_s = ", is_s
  else
     write(*,*) "the array loaded is not sorted. is_s = ", is_s
  end if
  
  ! checksum control
  do
     checksum = checksum + input_data(i)
     i = i + 1
     if(i > arr_len) exit
  end do

  write(*,*)
  
  if( read_checksum == checksum) then
     write(*,*) "the checksum calculated is correct"
  else
     write(*,*) "the checksum calculated is wrong"     
     write(*,*) "read_checksum = ", read_checksum, " my_checksum = ", checksum
  end if

  write(*,*)
  
end program read_integer_from_stdin

integer function is_sorted(arr)
  implicit none
  integer :: j, check = 0
  integer, dimension(:) :: arr

  do j=1, size(arr)-1
     if( arr(j) > arr(j+1)) then
        check = check + 1
     end if
  end do

  is_sorted = check
end function is_sorted
