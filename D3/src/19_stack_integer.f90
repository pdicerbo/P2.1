PROGRAM stack_integer
  use list_types

  type (StackArray), pointer :: FirstStack
  type (StackArray), pointer :: SecondStack => NULL()
  integer :: dlen, i, checksum, stack_len
  integer, allocatable, dimension(:) :: dat
  
  read(5,*) dlen
  allocate(dat(dlen))
  read(5,*) (dat(i),i=1,dlen)
  read(5,*) checksum

  allocate(FirstStack)
  allocate(SecondStack)

  stack_len = 40
  
  FirstStack = Stack_Init(stack_len)

  do i=1,dlen/2
     call FirstStack % push(dat(i))
  end do

  SecondStack = Stack_Init(FirstStack)
  do i=dlen/2, dlen
     call SecondStack % push(dat(i))
  end do

  ! clean StackArrays

  stack_len = FirstStack % length()
  do i=1, stack_len
     dlen = FirstStack % pop()
  end do
  ! print*,FirstStack % length()
  
  stack_len = SecondStack % length()
  do i=1, stack_len
     dlen = SecondStack % pop()
  end do
  ! print*,SecondStack % length()

  call FirstStack % free_stack()
  call SecondStack % free_stack()
  deallocate(FirstStack)
  deallocate(SecondStack)
  deallocate(dat)
  
END PROGRAM stack_integer
