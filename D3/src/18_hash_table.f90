

PROGRAM array_lookup
  USE list_types
  IMPLICIT NONE

  INTEGER :: num, i, j
  REAL :: chk, rv, time1, time2
  INTEGER, ALLOCATABLE, DIMENSION(:) :: idx
  TYPE(pair),ALLOCATABLE,DIMENSION(:) :: dat
  TYPE(pair) :: p
  INTEGER, PARAMETER :: nlook = 5000
  type (HashTable), pointer :: HT
  
  READ(5,*) num
  ALLOCATE(dat(num))
  READ(5,*) (dat(i),i=1,num)
  READ(5,*) chk

  ! XXX fill linked list or hash table with items from dat() here
  allocate(HT)
  call HT % hash_init(150)
  do i=1,num
     call HT % add_hash(dat(i))
  end do
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
     p = HT % hash_find(idx(i))
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'Hash Table lookups', (time2-time1)*1000.0

  ! XXX free all allocated data
  DEALLOCATE(dat,idx)
  call HT % hash_free()
  deallocate(HT)
666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM array_lookup
