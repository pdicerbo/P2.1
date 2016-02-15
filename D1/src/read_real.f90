program read_integer_from_stdin
  implicit none

  integer :: arr_len = 0
  integer :: i = 1, read_checksum, checksum = 0
  real, allocatable, dimension(:) :: input_data
  
  read(5,*) arr_len
  
  write(*,*) 
  write(*,*) "the array lenght is: ", arr_len

  allocate(input_data(arr_len))

  read(5, *) input_data
  read(5, *) read_checksum
  
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
