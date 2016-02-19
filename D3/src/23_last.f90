PROGRAM TreeTest
  use list_types
  use MyTree
  
  implicit none
  
  integer :: num, i, j
  real :: checksum, rv, time1, time2
  type (pair), allocatable, dimension(:) :: dat
  type (tree), pointer :: RealTree
  type (tree), pointer :: SecondTree
  type (pair), dimension(:), pointer :: to_extract
  type (pair) :: p
  integer, parameter :: nlook = 5000
  integer, allocatable, dimension(:) :: idx
  
  read(5,*) num
  allocate(dat(num))
  read(5,*) (dat(i),i=1,num)
  read(5,*) checksum

  ! fill idx array with randomly selected keys
  CALL RANDOM_SEED()
  ALLOCATE(idx(nlook))
  DO i=1,nlook
     CALL RANDOM_NUMBER(rv)
     j = INT(rv*num)+1
     idx(i) = dat(j)%key
  END DO
  
  CALL CPU_TIME(time1)
  DO i=1,nlook
     DO j=1,num
        IF (dat(j)%key == idx(i)) THEN
           p = dat(j)
           EXIT
        END IF
     END DO
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'array value lookups', (time2-time1)*1000.0  
  
  allocate(RealTree)

  RealTree = tree_init(dat(1))

  do i=2, num
     call RealTree % add_tree(dat(i))
  end do

  call RealTree % print_tree_depth()
  call RealTree % print_nleafs()

  CALL CPU_TIME(time1)
  DO i=1,nlook
     ! XXX do linked list or hash table lookups here
     p = RealTree % find_in_tree(idx(i))
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'Linked List lookups', (time2-time1)*1000.0

  allocate(to_extract(num))

  call RealTree % extract_sorted_array(to_extract)
  
  allocate(SecondTree)
  
  SecondTree = Rebalance_Tree(RealTree)
  ! print*,"SecondTree nodes: ", SecondTree % get_nodes()
  ! call SecondTree % print_tree_depth()

  call SecondTree % free_tree()
  call RealTree % free_tree()
  
  deallocate(to_extract)
  deallocate(RealTree)
  deallocate(SecondTree)
  deallocate(dat)
  deallocate(idx)
  
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')       
END PROGRAM TreeTest
