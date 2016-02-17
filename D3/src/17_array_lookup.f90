

PROGRAM array_lookup
  USE list_types
  IMPLICIT NONE

  INTEGER :: num, i, j
  REAL :: chk, rv, time1, time2
  INTEGER, ALLOCATABLE, DIMENSION(:) :: idx
  TYPE(pair),ALLOCATABLE,DIMENSION(:) :: dat
  TYPE(pair) :: p
  INTEGER, PARAMETER :: nlook = 5000
  type(LinkedList), pointer :: List
  
  READ(5,*) num
  ALLOCATE(dat(num))
  READ(5,*) (dat(i),i=1,num)
  READ(5,*) chk

  ! XXX fill linked list or hash table with items from dat() here
  allocate(List)
  write(*,*) "allocation done"
  call List % init_ll(42.)
  ! call List % add_ll(42.)
  write(*,*) "init list with ", List % val
  call List % add_ll(3.14)
  write(*,*) "add element ", List % val
  call List % add_ll(6.28)
  write(*,*) "add element ", List % val
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

  CALL CPU_TIME(time1)
  DO i=1,nlook
      ! XXX do linked list or hash table lookups here
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'XXXX XXXX lookups', (time2-time1)*1000.0

  ! XXX free all allocated data
  DEALLOCATE(dat,idx)
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM array_lookup
