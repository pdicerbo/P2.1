program read_integer_from_stdin
  implicit none

  integer :: arr_len = 0

  read(5,*) arr_len

  write(*,*) "the array lenght is: ", arr_len

  
end program read_integer_from_stdin
