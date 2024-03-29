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

  type HashTable
     type (LinkedList), allocatable, dimension(:) :: bucket
     integer :: nbuckets
     
   contains
     procedure :: hash_init
     procedure :: hash_func
     procedure :: add_hash
     procedure :: hash_find
     procedure :: hash_free
     
  end type HashTable

  type StackArray
     private
     integer, allocatable, dimension(:) :: StackArr
     integer :: len
     integer :: index

   contains

     procedure :: push
     procedure :: check_boundary
     procedure :: pop
     procedure :: length
     procedure :: free_stack
     procedure :: copy_constr
     
  end type StackArray

  type StackList
     private
     type (LinkedList), pointer :: StackLL => NULL()
     integer :: len

   contains

     procedure :: push_ll
     procedure :: get_length
     procedure :: pop_ll
     procedure :: free_stack_ll
     
  end type StackList
  
  interface Stack_Init
     module procedure real_init
     module procedure copy_constr
     module procedure init_stack_ll
     module procedure copy_constr_ll
  end interface Stack_Init

contains
  ! trivial implementation of the function
  ! that return the next prime number greater than
  ! the given argument n
  integer function next_prime(n)
    integer :: n, count, nprime
    logical :: is_prime

    if(n == 1) then
       next_prime = 2
    else
       
       nprime = n + 1
       
       do
          is_prime = .true.

          if(mod(nprime, 2) == 0) then
             do count=2,n
                if(mod(nprime, count) == 0)then
                   is_prime = .false.
                end if
             end do
          else
             do count=3,n,2
                if(mod(nprime, count) == 0)then
                   is_prime = .false.
                end if
             end do
          end if
          
          if(is_prime .eqv. .true.) then
             exit
          else
             nprime = nprime + 1
          end if
       end do
       next_prime = nprime
    endif
  end function next_prime

  
  ! the initialization is done by simply adding
  ! a new item in the list
  subroutine add_ll(self, a)
    class(LinkedList), intent(inout), target :: self
    type (pair), intent(in) :: a
    type (LinkedList), pointer :: tmp
    type (LinkedList), pointer :: new_cell
    
    tmp => self
    
    do while (associated(tmp % next))
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

  subroutine hash_init(self, n)
    class (HashTable), intent(inout) :: self
    integer, intent(in) :: n
    integer :: bucknum

    bucknum = next_prime(n)
    
    if(bucknum < 150) then
       print*, "the initialization value produces a number"
       print*, "of buckets to initialize equal to n = ", bucknum
       print*, "you should use at least n = 150"
       print*, "however I initialize your hash table.."
    end if
    
    self % nbuckets = bucknum
    allocate(self % bucket(bucknum))
    
  end subroutine hash_init

  integer function hash_func(self, mykey) result(hash)
    class (HashTable), intent(in) :: self
    integer, intent(in) :: mykey

    hash = mod(mykey, self % nbuckets) + 1
  end function hash_func

  subroutine add_hash(self, p)
    class (HashTable), intent(inout) :: self
    type (pair), intent(in) :: p
    integer :: index

    index = self % hash_func(p % key)

    call self % bucket(index) % add_ll(p)    
  end subroutine add_hash

  type (pair) function hash_find(self, mykey) result(FindPair)
    class (HashTable), intent(in) :: self
    integer, intent(in) :: mykey
    integer :: index

    index = self % hash_func(mykey)
    FindPair = self % bucket(index) % find_by_key(mykey)
  end function hash_find

  subroutine hash_free(self)
    class (HashTable), intent(inout) :: self
    integer :: i
    
    do i=1,self % nbuckets
       call self % bucket(i) % free_all()
    end do
  end subroutine hash_free
  
  type (StackArray) function real_init(n) result(self)
    integer, intent(inout) :: n

    ! if(n < 1) then
    !    write(*,*) "n must be at least 1"
    !    write(*,*) "exit"
    !    self % len = 0
    !    self % index = 0
    !    return
    ! end if

    self % len = n
    self % index = 1
    allocate(self % StackArr(n))

  end function real_init
  
  subroutine push(self, n)
    class (StackArray), intent(inout) :: self
    integer, intent(in) :: n

    call self % check_boundary()

    self % StackArr(self % index) = n
    self % index = self % index + 1
    
  end subroutine push

  subroutine check_boundary(self)
    class (StackArray), intent(inout) :: self
    integer, allocatable, dimension(:) :: tmp
    integer :: j
    
    if(self % index > self % len) then
       
       allocate(tmp(self % len + 10))

       do j=1, self % len
          tmp(j) = self % StackArr(j)
       end do

       deallocate(self % StackArr)
       self % StackArr = tmp
       self % len = self % len + 10       
    endif
  end subroutine check_boundary

  integer function pop(self) result(th)
    class (StackArray), intent(inout) :: self

    if(self % index .le. 1) then
       print*,self % index
       print*,"There isn't element to pop"
       print*,"exit"
       th = 0
       return
    end if

    self % index = self % index - 1
    th = self % StackArr(self % index)
  end function pop
  
  integer function length(self) result(l)
    class (StackArray), intent(in) :: self

    ! number of items within the StackArray
    l = self % index - 1

  end function length
  
  subroutine free_stack(self)
    class (StackArray), intent(inout) :: self

    deallocate(self % StackArr)
    
  end subroutine free_stack

  type (StackArray) function copy_constr(PrevStack) result(ret)
    class (StackArray), intent(in) :: PrevStack
    integer :: j
    
    ret % len = PrevStack % len
    ret % index = PrevStack % index
    allocate(ret % StackArr(ret % len))

    do j=1, ret%len
       ret % StackArr(j) = PrevStack % StackArr(j)
    end do
    
  end function copy_constr

  type (StackList) function init_stack_ll() result(ret)
    ret % len = 0
    allocate(ret % StackLL)
  end function init_stack_ll

  subroutine push_ll(self, n)
    class (StackList), intent(inout) :: self
    type (pair), intent(in) :: n

    call self % StackLL % add_ll(n)
    self % len = self % len + 1
  end subroutine push_ll

  integer function get_length(self) result(ret)
    class (StackList), intent(in) :: self

    ret = self % len
    
  end function get_length

  type (StackList) function copy_constr_ll(Prev) result(ret)
    type (StackList), intent(in) :: Prev
    type (LinkedList), pointer :: tmp

    ret % len = Prev % get_length()
    allocate(ret % StackLL)

    tmp => Prev % StackLL
    do while(associated(tmp % next))
       call ret % StackLL % add_ll(tmp % val)
       tmp => tmp % next
    end do
  end function copy_constr_ll

  subroutine free_stack_ll(self)
    class (StackList), intent(inout) :: self

    call self % StackLL % free_all()
    deallocate(self % StackLL)
    
  end subroutine free_stack_ll

  type (pair) function  pop_ll(self) result(ret)

    class (StackList), intent(inout) :: self
    type (LinkedList), pointer :: first
    type (LinkedList), pointer :: sec

    first => self % StackLL

    if(.not. associated(first % next)) then
       print*,"There isn't element to pop"
       print*,"exit"
       ret % key = 0
       ret % val = 0.
       return
    end if
    sec => first % next

    do while(associated(sec % next))
       first => first % next
       sec => sec % next
    end do
    
    deallocate(first % next)

    self % len = self % len - 1
    
  end function pop_ll
  
end module list_types
