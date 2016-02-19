PROGRAM MyFirstTree
  use list_types

  type (pair) :: pair_init
  type (tree), pointer :: MyTree

  allocate(MyTree)

  pair_init % key = 1
  pair_init % val = 20.
  print*,"Tree init"
  MyTree = tree_init(pair_init)
  print*,"now NNodes = ", MyTree % get_nodes()
  
  print*, ""
  print*, "Filling Tree:"
  print*, ""

  pair_init % key = 2
  pair_init % val = 8.
  print*,""
  print*,"add val = ", pair_init % val
  call MyTree % add_tree(pair_init)
  print*,"now NNodes = ", MyTree % get_nodes()

  pair_init % key = 3
  pair_init % val = 16.
  print*,""
  print*,"add val = ", pair_init % val
  call MyTree % add_tree(pair_init)
  print*,"now NNodes = ", MyTree % get_nodes()
  
  pair_init % key = 4
  pair_init % val = 17.
  print*,""
  print*,"add val = ", pair_init % val
  call MyTree % add_tree(pair_init)
  print*,"now NNodes = ", MyTree % get_nodes()
  
  pair_init % key = 5
  pair_init % val = 40.
  print*,""
  print*,"add val = ", pair_init % val
  call MyTree % add_tree(pair_init)
  print*,"now NNodes = ", MyTree % get_nodes()
  
  pair_init % key = 6
  pair_init % val = 23.
  print*,""
  print*,"add val = ", pair_init % val
  call MyTree % add_tree(pair_init)
  print*,"now NNodes = ", MyTree % get_nodes()

  call MyTree % free_tree()
  deallocate(MyTree)
  
END PROGRAM MyFirstTree
