module list_types

  implicit none
  
  type pair
     integer :: key
     real :: val
  end type pair

  type LinkedList
     type (pair) :: val
     type(LinkedList), pointer :: next => NULL()

   contains
     procedure :: init_ll
     procedure :: add_ll
  end type LinkedList
  

contains
  
  ! the initialization is done by assing
  ! the first element of the list
  subroutine init_ll(self, a)
    class(LinkedList), intent(inout) :: self
    type (pair), intent(in) :: a
    self % val = a
    allocate(self % next)
  end subroutine init_ll

  subroutine add_ll(self, a)
    class(LinkedList), intent(inout), target :: self
    type (pair), intent(in) :: a
    type (LinkedList), pointer :: tmp
    type (LinkedList), pointer :: new_cell
    
    tmp => self
    
    do while ( associated(tmp % next))
       tmp => tmp % next
    end do

    allocate(new_cell)
    
    tmp % val = a
    tmp % next => new_cell

  end subroutine add_ll

end module list_types
