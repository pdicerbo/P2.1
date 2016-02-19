PROGRAM MyFirstTree
  use list_types
  use MyTree

  implicit none
  
  type (pair) :: pair_init, findpair
  type (tree), pointer :: FirstTree
  integer :: key_to_find, nn
  type (pair), dimension(:), pointer :: to_extract
  
  allocate(FirstTree)

  pair_init % key = 4
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
  
  pair_init % key = 1
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

  call FirstTree % extract_sorted_array(to_extract)
  print*,to_extract
  call FirstTree % free_tree()
  deallocate(FirstTree)
  
END PROGRAM MyFirstTree
