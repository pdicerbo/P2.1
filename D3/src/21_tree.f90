PROGRAM MyFirstTree
  use list_types
  use MyTree

  implicit none
  
  type (pair) :: pair_init, findpair
  type (tree), pointer :: FirstTree
  type (tree), pointer :: SecondTree
  integer :: key_to_find, nn, nnn
  type (pair), dimension(:), pointer :: to_extract, next_extract
  
  allocate(FirstTree)
  allocate(SecondTree)

  pair_init % key = 1
  pair_init % val = 20.
  print*,"Tree init"
  FirstTree = tree_init(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()
  
  print*, ""
  print*, "Filling Tree:"
  print*, ""

  pair_init % key = 2
  pair_init % val = 8.
  print*,""
  print*,"add val = ", pair_init % val, "with key = ", pair_init % key
  call FirstTree % add_tree(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()

  pair_init % key = 3
  pair_init % val = 16.
  print*,""
  print*,"add val = ", pair_init % val, "with key = ", pair_init % key
  call FirstTree % add_tree(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()
  
  pair_init % key = 4
  pair_init % val = 17.
  print*,""
  print*,"add val = ", pair_init % val, "with key = ", pair_init % key
  call FirstTree % add_tree(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()
  
  pair_init % key = 5
  pair_init % val = 40.
  print*,""
  print*,"add val = ", pair_init % val, "with key = ", pair_init % key
  call FirstTree % add_tree(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()
  
  pair_init % key = 6
  pair_init % val = 23.
  print*,""
  print*,"add val = ", pair_init % val, "with key = ", pair_init % key
  call FirstTree % add_tree(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()

  pair_init % key = 8
  pair_init % val = 15.
  print*,""
  print*,"add val = ", pair_init % val, "with key = ", pair_init % key
  call FirstTree % add_tree(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()

  pair_init % key = 0
  pair_init % val = 15.
  print*,""
  print*,"add val = ", pair_init % val, "with key = ", pair_init % key
  call FirstTree % add_tree(pair_init)
  print*,"now NNodes = ", FirstTree % get_nodes()

  key_to_find = 5
  findpair = FirstTree % find_in_tree(key_to_find) 


  call FirstTree % print_tree_depth()
  call FirstTree % print_nleafs()
  print*,""
  print*,"find key = ", key_to_find
  print*,"key found =", findpair % key,"val = ", findpair % val 
  print*,""

  nn = FirstTree % get_nodes()
  allocate(to_extract(nn))

  call FirstTree % print_tree_depth()
  print*,"FirstTree nodes = ", FirstTree % get_nodes()
  print*,""
  call FirstTree % extract_sorted_array(to_extract)
  print*,to_extract

  SecondTree = Rebalance_Tree(FirstTree)

  nnn = SecondTree % get_nodes()
  allocate(next_extract(nnn))

  call SecondTree % extract_sorted_array(next_extract)
  print*,next_extract

  call SecondTree % print_tree_depth()
  print*,"SecondTree nodes = ", SecondTree % get_nodes()

  call FirstTree % free_tree()
  call SecondTree % free_tree()
  deallocate(FirstTree)
  deallocate(to_extract)
END PROGRAM MyFirstTree
