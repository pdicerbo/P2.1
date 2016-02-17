module list_types

  implicit none
  
  type pair
     integer :: key
     real :: val
  end type pair

  type LinkedList
     real :: val
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
    real, intent(in) :: a
    self % val = a
    ! allocate(self % next)
  end subroutine init_ll

  subroutine add_ll(self, a)
    class(LinkedList), intent(inout) :: self
    real, intent(in) :: a
    type (LinkedList), pointer :: tmp

    allocate(tmp)
    tmp = self
    
    do while (associated(tmp % next))
       write(*,*) "im in", tmp%val
       tmp => self % next
       write(*,*) "im assigned pointer"
    end do
    
    print*,"allocation"
    allocate(tmp % next)
    print*,"allocation done"
    ! tmp % next % val = a
    tmp  % val = a
    print*,"new_item = ", tmp%val

    deallocate(tmp)
  end subroutine add_ll

end module list_types
