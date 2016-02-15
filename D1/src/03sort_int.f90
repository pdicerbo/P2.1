program read_integer_from_stdin
  implicit none

  integer :: arr_len = 0
  integer :: i = 1, read_checksum, checksum = 0, iss
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

  ! iss = is_sorted(input_data)
  
  write(*,*) "sorting = ", is_sorted(input_data)
  ! if( is_sorted(input_data) == 0 ) then
  !    write(*,*) "the array loaded is sorted"
  ! else
  !    write(*,*) "the array loaded is not sorted"
  
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
     if( arr(j) > arr(j + 1)) check = check + 1
  end do
  
  is_sorted = check
end function is_sorted
