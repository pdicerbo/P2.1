module MyTree
  use list_types

  implicit none
  
  type node
     type (pair) :: value
     type (node), pointer :: left => NULL()
     type (node), pointer :: right => NULL()
     integer :: NodeDepth
     
   contains
     procedure :: add_node
     procedure :: free_all_nodes
     procedure :: find_in_nodes
     procedure :: find_depth
     procedure :: leafs_enum
     procedure :: get_all_nodes
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
     procedure :: print_tree_depth
     procedure :: print_nleafs
     procedure :: extract_sorted_array
  end type tree

contains

  type (tree) function tree_init(n) result(new_tree)
    type (pair), intent(in) :: n
    
    new_tree % root % value = n
    new_tree % root % NodeDepth = 1 ! "minimum" depth = 1
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

          ! print*,"go to LEFT"
          call self % left % add_node(n)
          
       else

          ! print*,"new left allocation for value = ", n % val, " key = ", n % key
          allocate(new_node)
          new_node % value = n
          new_node % NodeDepth = self % NodeDepth + 1
          ! print*,"for key = ", n % key,", depth is", new_node % NodeDepth
          self % left => new_node
          
       end if
    else
       if(associated(self % right)) then

          ! print*,"go to the RIGHT"
          call self % right % add_node(n)
          
       else

          ! print*,"new right allocation for value = ", n % val, " key = ", n % key
          allocate(new_node)
          new_node % value = n
          new_node % NodeDepth = self % NodeDepth + 1
          ! print*,"for key = ", n % key,", depth is", new_node % NodeDepth
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

  subroutine print_tree_depth(self)
    class (tree), intent(in) :: self
    integer :: depth
    depth = 1
    call self % root % find_depth(depth)
    print*,""
    print*,"The depth of the tree is equal to ", depth
    print*,""
  end subroutine print_tree_depth

  recursive subroutine find_depth(self, depth)
    class (node), intent(in) :: self
    integer, intent(inout) :: depth
    
    if(self % NodeDepth > depth) then
       depth = self % NodeDepth
    end if

    if(associated(self % left)) then
       call self % left % find_depth(depth)
    end if
    if(associated(self % right)) then
       call self % right % find_depth(depth)
    end if
    
  end subroutine find_depth

  subroutine print_nleafs(self)
    class (tree), intent(in) :: self
    integer :: leafs
    leafs = 0
    call self % root % leafs_enum(leafs)
    print*,""
    print*,"The number of the leafs of the tree is equal to ", leafs
    print*,""
  end subroutine print_nleafs

  recursive subroutine leafs_enum(self, nl)
    class (node), intent(in) :: self
    integer, intent(inout) :: nl

    if(associated(self % left)) then
       
       call self % left % leafs_enum(nl)
       if(.not. associated(self % right)) then
          nl = nl + 1
       end if
       
    end if

    if(associated(self % right)) then

       call self % right % leafs_enum(nl)

       if(.not. associated(self % left)) then
          nl = nl + 1
       end if
       
    end if
    
  end subroutine leafs_enum

  subroutine extract_sorted_array(self, array)
    class (tree), intent(in) :: self
    type (pair), dimension(:), intent(inout) :: array
    integer :: index

    index = 1    
    call self % root % get_all_nodes(array, index)
    
  end subroutine extract_sorted_array

  recursive subroutine get_all_nodes(self, array, index)
    class (node), intent(in) :: self
    type(pair), dimension(:), intent(inout) :: array
    integer, intent(inout) :: index

    if(associated(self % left)) then       
       call self % left % get_all_nodes(array, index)
    end if

    array(index) = self % value
    index = index + 1

    if(associated(self % right)) then
       call self % right % get_all_nodes(array, index)
    end if
    
  end subroutine get_all_nodes

  type (tree) function rebalance_tree(OldTree) result(NewTree)
    type (tree), intent(in) :: OldTree
    type (pair), dimension(:), pointer :: Arr
    integer :: num, middle, first_start, first_end, sec_start, sec_end
    integer :: real_middle
    logical :: first_call

    num = OldTree % get_nodes()
    allocate(Arr(num))

    call OldTree % extract_sorted_array(Arr)

    if(num > 1) then
 
       first_start = 1
       middle = (num + first_start)/2
       real_middle = middle
       NewTree = tree_init(Arr(middle))

       first_end = real_middle
       sec_start = real_middle
       sec_end = num + 1

       first_call = .true.
       
       middle = (first_end + first_start)/2
       
       call rebalance_add(NewTree, Arr, middle, first_start, first_end, real_middle, first_call)

       first_call = .false.
       middle = (sec_end + sec_start)/2

       call rebalance_add(NewTree, Arr, middle, sec_start, sec_end, real_middle, first_call)
       
    else if(num == 1) then
       print*,"Nothing to rebalance"
       print*,"Initialization for the new tree"
       NewTree = tree_init(Arr(num))
    else
       print*,"Bad input in rebalancing function"
       print*,"return"
       return
    end if
    
    deallocate(Arr)
  end function rebalance_tree

  recursive subroutine rebalance_add(MyTree, array, index, start, end, real_middle, first_call)
    type (tree), intent(inout) :: MyTree
    integer, intent(inout) :: index
    integer, intent(inout) :: start
    integer, intent(inout) :: end
    type (pair), dimension(:), intent(in) :: array
    integer, intent(in) :: real_middle
    logical, intent(inout) :: first_call
    integer :: midpoint_left, midpoint_right, first_end, sec_start

    ! all these if (first_call) are needed because otherwise
    ! the init value of the new tree is written twice
    ! I am pretty shure that there is more clever ways
    ! to avoid this, but I don't have enough time...
    ! I'm really sorry

    call MyTree % add_tree(array(index))

    ! if (first_call .eqv. .true.)then
    !    call MyTree % add_tree(array(index))
    ! else
    !    if(first_call .eqv. .false.) then
    !       ! if(index > real_middle) then
    !          call MyTree % add_tree(array(index))
    !       ! end if
    !    end if
    ! end if
    
    if(index > 1 .and. index >= start .and. index < end) then
       first_end = index
       sec_start = index+1
       midpoint_left = (index + start) / 2
       midpoint_right = (end + index) / 2

       call rebalance_add(MyTree, array, midpoint_left, start, first_end, real_middle, first_call)
       call rebalance_add(MyTree, array, midpoint_right, sec_start, end, real_middle, first_call)

    end if
    
  end subroutine
  
end module MyTree
