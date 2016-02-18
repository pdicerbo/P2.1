PROGRAM stack_integer
  use list_types

  type (StackList), pointer :: FirstStack
  type (StackList), pointer :: SecondStack
  integer :: dlen, i, stack_len, counter
  type (pair), allocatable, dimension(:) :: dat
  real :: checksum
  
  read(5,*) dlen
  allocate(dat(dlen))
  read(5,*) (dat(i),i=1,dlen)
  read(5,*) checksum

  allocate(FirstStack)
  allocate(SecondStack)

  stack_len = 40
  counter = 0
  FirstStack = Stack_Init()

  do i=1,dlen/2
     call FirstStack % push_ll(dat(i))
     counter = counter + 1
  end do
  print*, FirstStack % get_length(), dlen/2, counter

  SecondStack = Stack_Init(FirstStack)
  do i=dlen/2+1, dlen
     call SecondStack % push_ll(dat(i))
     counter = counter + 1
  end do
  ! do i=1,dlen/2
  !    call SecondStack % push_ll(dat(i))
  !    counter = counter + 1
  ! end do
  print*, SecondStack % get_length(), dlen, counter

  ! clean StackArrays

  ! stack_len = FirstStack % length()
  ! do i=1, stack_len
  !    dlen = FirstStack % pop()
  ! end do
  ! print*,FirstStack % length()
  
  ! stack_len = SecondStack % length()
  ! do i=1, stack_len
  !    dlen = SecondStack % pop()
  ! end do
  ! print*,SecondStack % length()

  ! call FirstStack % free_stack()
  ! call SecondStack % free_stack()
  call FirstStack % free_stack_ll()
  deallocate(FirstStack)
  call SecondStack % free_stack_ll()
  deallocate(SecondStack)
  ! deallocate(SecondStack)
  deallocate(dat)
  
END PROGRAM stack_integer
