PROGRAM TreeTest
  use list_types
  use MyTree
  
  implicit none
  
  integer :: num, i
  real :: checksum
  type (pair), allocatable, dimension(:) :: dat
  type (tree), pointer :: RealTree
  type (pair), dimension(:), pointer :: to_extract

  read(5,*) num
  allocate(dat(num))
  read(5,*) (dat(i),i=1,num)
  read(5,*) checksum

  allocate(RealTree)

  RealTree = tree_init(dat(1))

  do i=2, num
     call RealTree % add_tree(dat(i))
  end do

  call RealTree % print_tree_depth()
  call RealTree % print_nleafs()

  allocate(to_extract(num))

  call RealTree % extract_sorted_array(to_extract)
  
  call RealTree % free_tree()

  deallocate(to_extract)
  deallocate(RealTree)
  deallocate(dat)
  
END PROGRAM TreeTest
