PROGRAM stack_integer
  use list_types

  type (StackArray), pointer :: MyStack
  type (StackArray), pointer :: MyNewStack
  integer :: first_stack_l, ll

  first_stack_l = 1
  allocate(MyStack)
  allocate(MyNewStack)
  
  MyStack = Stack_Init(first_stack_l)
  
  call MyStack % push(first_stack_l) 
  call MyStack % push(first_stack_l)
  call MyStack % push(first_stack_l)

  MyNewStack = Stack_Init(MyStack)
  print*,  MyStack % length()
  ll = MyNewStack % length()
  
  print*, ll
  
  call MyStack % free_stack()
  deallocate(MyStack)
  call MyNewStack % free_stack()
  deallocate(MyNewStack)
  
END PROGRAM stack_integer
