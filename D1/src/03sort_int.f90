program read_integer_from_stdin
  use list_tools
  implicit none

  integer :: arr_len = 0
  integer :: i = 1, is_s, read_checksum, checksum = 0
  integer, allocatable, dimension(:) :: input_data
  
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

