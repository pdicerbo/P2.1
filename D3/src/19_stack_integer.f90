PROGRAM stack_integer
  use list_types

  type (StackArray), pointer :: MyStack
  integer :: first_stack_l, ll

  first_stack_l = 1
  allocate(MyStack)

  call MyStack % stack_init(first_stack_l)
  call MyStack % push(first_stack_l) 
  call MyStack % push(first_stack_l)
  call MyStack % push(first_stack_l)

  print*, MyStack % len

  ll = MyStack % length()
  
  print*, ll
  
  call MyStack % free_stack()
  deallocate(MyStack)
  
END PROGRAM stack_integer
