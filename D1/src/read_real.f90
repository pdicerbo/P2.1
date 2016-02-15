program read_integer_from_stdin
  implicit none

  integer :: arr_len = 0
  integer :: i = 1
  real :: read_checksum, checksum = 0., diff
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

  diff = abs(read_checksum - checksum) / checksum
  if( diff < 1.e-5  ) then
     write(*,*) "the checksum calculated is correct"
  else
     write(*,*) "the checksum calculated is wrong"     
     write(*,*) "read_checksum = ", read_checksum, " my_checksum = ", checksum
     write(*,*) "the relative difference is ", diff
  end if

  write(*,*)
  
end program read_integer_from_stdin
