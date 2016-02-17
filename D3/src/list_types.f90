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
     
     procedure :: add_ll
     procedure :: find_by_key
     procedure :: free_all
  end type LinkedList
  
contains
  
  ! the initialization is done by simply adding
  ! a new item in the list
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
  
  type(pair) function find_by_key(self, mykey) result(pair_ret)
    class (LinkedList), intent(in), target :: self
    integer, intent(in) :: mykey
    type (LinkedList), pointer :: tmp
    
    tmp => self
    
    do while(mykey .ne. tmp % val % key)
       tmp => tmp % next
       if(.not. associated(tmp % next))then
          write(*,*) "key ", mykey, "not found"
          return
       end if
    end do
    pair_ret = tmp % val
  end function find_by_key

  recursive subroutine free_all(self)
    class (LinkedList), intent(inout), target :: self
    type (LinkedList), pointer :: tmp
    type (LinkedList), pointer :: sec_tmp
    
    tmp => self

    if(associated(tmp % next)) then
       sec_tmp => tmp % next
       call sec_tmp % free_all()
       deallocate(tmp % next)
    else
       return
    end if
  end subroutine free_all
  
end module list_types
