module MyTree
  use list_types
  use sorting
  implicit none
  
  type node
     type (pair) :: value
     type (node), pointer :: left => NULL()
     type (node), pointer :: right => NULL()
     
   contains
     procedure :: add_node
     procedure :: free_all_nodes
     procedure :: find_in_nodes
  end type node
  
  type tree
     private
     type (node) :: root
     integer :: n_nodes
     
   contains
     procedure :: add_tree
     procedure :: free_tree
     procedure :: get_nodes
     procedure :: find_in_tree
  end type tree

contains

  type (tree) function tree_init(n) result(new_tree)
    type (pair), intent(in) :: n
    
    new_tree % root % value = n
    new_tree % n_nodes = 1
    
  end function tree_init
  
  subroutine add_tree(self, n)
    class (tree), intent(inout) :: self
    type (pair), intent(in) :: n
    
    call self % root % add_node(n)
    self % n_nodes = self % n_nodes + 1
    
  end subroutine add_tree
  
  recursive subroutine add_node(self, n)
    class (node), intent (inout) :: self
    type (pair), intent(in) :: n
    type (node), pointer :: new_node => NULL()
    
    if(self % value % key > n % key) then
       if(associated(self % left)) then
          print*,"go to left"
          call self % left % add_node(n)
       else
          print*,"new left allocation for value = ", n % val
          allocate(new_node)
          new_node % value = n
          self % left => new_node
       end if
    else
       if(associated(self % right)) then
          print*,"go to the right"
          call self % right % add_node(n)
       else
          print*,"new right allocation for value = ", n % val
          allocate(new_node)
          new_node % value = n
          self % right => new_node
       end if
    end if
  end subroutine add_node
  
  subroutine free_tree(self)
    class (tree), intent(inout) :: self
    
    call self % root % free_all_nodes()
    self % n_nodes = 1
    
  end subroutine free_tree
  
  recursive subroutine free_all_nodes(self)
    class (node), intent(inout) :: self
    
    if(associated(self % left)) then
       call self % left % free_all_nodes()
       deallocate(self % left)
    end if
    if(associated(self % right)) then
       call self % right % free_all_nodes()
       deallocate(self % right)
    end if
    
  end subroutine free_all_nodes
  
  integer function get_nodes(self) result(NNodes)
    
    class (tree), intent(in) :: self
    
    NNodes = self % n_nodes
    
  end function get_nodes
  
  type (pair) function find_in_tree(self, MyKey) result(ret)
    class (tree), intent(in) :: self
    integer, intent(in) :: MyKey
    
    ret = self % root % find_in_nodes(MyKey)
    
  end function find_in_tree
  
  type (pair) recursive function find_in_nodes(self, n) result(ret)
    class (node), intent(in) :: self
    integer, intent(in) :: n
    
    if(n == self % value % key) then
       ret % val = self % value % val
       ret % key = self % value % key

    else if(n < self % value % key) then
       if(associated(self % left)) then
          ret = self % left % find_in_nodes(n)
       else
          print*,"key = ", n, " not found"
          print*,"return ''0''"
          ret % key = 0
          ret % val = 0.
       end if
    else
       if(associated(self % right)) then
          ret = self % right % find_in_nodes(n)
       else
          print*,"key = ", n, " not found"
          print*,"return ''0''"
          ret % key = 0
          ret % val = 0.
       end if
    end if
    
  end function find_in_nodes

end module MyTree
