    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  Sort Module
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !-----------------------------------------------------------------------------
    MODULE SortMod
    !-----------------------------------------------------------------------------
    IMPLICIT NONE
    !-----------------------------------------------------------------------------
    PRIVATE
    PUBLIC :: qsort
    PUBLIC :: psort
    PUBLIC :: search
    !-----------------------------------------------------------------------------
    !
    !------------------------------------------------------------------------------

    !-----------------------------------------------------------------------------
    ! Quick sort, without changing the order of input elements
    !------------------------------------------------------------------------------
    INTERFACE qsort
    MODULE PROCEDURE sort_int
    MODULE PROCEDURE sort_real
    END INTERFACE
    !------------------------------------------------------------------------------

    !-----------------------------------------------------------------------------
    ! Quick sort, with changing the order of input elements
    !------------------------------------------------------------------------------
    INTERFACE psort
    MODULE PROCEDURE SortI
    MODULE PROCEDURE SortF
    MODULE PROCEDURE SortD
    MODULE PROCEDURE SortC
    END INTERFACE

    !-----------------------------------------------------------------------------
    ! Find the position of an element
    !------------------------------------------------------------------------------
    INTERFACE search
    MODULE PROCEDURE SearchI
    MODULE PROCEDURE SearchR
    END INTERFACE

    !------------------------------------------------------------------------------

    CONTAINS

    !------------------------------------------------------------------------------
    !> Sort an array of INTEGER values.
    !------------------------------------------------------------------------------
    PURE SUBROUTINE Sort( n, a )
    !-----------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN)  :: n
    INTEGER(KIND=4), INTENT(INOUT) :: a(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: i, j, l, ir, ra
    !------------------------------------------------------------------------------

    IF ( n <= 1 ) RETURN

    l = n / 2 + 1
    ir = n
    DO WHILE( .TRUE. )
        IF ( l > 1 ) THEN
            l = l - 1
            ra = a(l)
        ELSE
            ra = a(ir)
            a(ir) = a(1)
            ir = ir - 1
            IF ( ir == 1 ) THEN
                a(1) = ra
                RETURN
            END IF
        END IF
        i = l
        j = l + l
        DO WHILE( j <= ir )
            IF ( j < ir ) THEN
                IF ( a(j) < a(j+1) ) j = j + 1
            END IF

            IF ( ra < a(j) ) THEN
                a(i) = a(j)
                i = j
                j =  j + i
            ELSE
                j = ir + 1
            END IF
            a(i) = ra
        END DO
    END DO

    !------------------------------------------------------------------------------
    END SUBROUTINE Sort
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    !> Sort an interger array a, together with an another INTEGER array.
    !------------------------------------------------------------------------------
    PURE SUBROUTINE SortI( n, a, b )
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN) :: n
    INTEGER(KIND=4), INTENT(INOUT) :: a(:), b(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: i, j, l, ir, ra, rb
    !------------------------------------------------------------------------------

    IF ( n <= 1 ) RETURN

    l = n / 2 + 1
    ir = n
    DO WHILE( .TRUE. )
        IF ( l > 1 ) THEN
            l = l - 1
            ra = a(l)
            rb = b(l)
        ELSE
            ra = a(ir)
            rb = b(ir)
            a(ir) = a(1)
            b(ir) = b(1)
            ir = ir - 1
            IF ( ir == 1 ) THEN
                a(1) = ra
                b(1) = rb
                RETURN
            END IF
        END IF
        i = l
        j = l + l
        DO WHILE( j <= ir )
            IF ( j < ir  ) THEN
                IF ( a(j) < a(j+1) ) j = j + 1
            END IF
            IF ( ra < a(j) ) THEN
                a(i) = a(j)
                b(i) = b(j)
                i = j
                j =  j + i
            ELSE
                j = ir + 1
            END IF
            a(i) = ra
            b(i) = rb
        END DO
    END DO

    !------------------------------------------------------------------------------
    END SUBROUTINE SortI
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    !> Sort an index array, and change the order of an REAL array accordingly.
    !------------------------------------------------------------------------------
    PURE SUBROUTINE SortF( n, a, b )
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN) :: n
    INTEGER(KIND=4), INTENT(INOUT) :: a(:)
    REAL(KIND=8), INTENT(INOUT) :: b(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: i, j, l, ir, ra
    REAL(KIND=8) :: rb
    !------------------------------------------------------------------------------

    IF ( n <= 1 ) RETURN

    l = n / 2 + 1
    ir = n
    DO WHILE( .TRUE. )

        IF ( l > 1 ) THEN
            l = l - 1
            ra = a(l)
            rb = b(l)
        ELSE
            ra = a(ir)
            rb = b(ir)
            a(ir) = a(1)
            b(ir) = b(1)
            ir = ir - 1
            IF ( ir == 1 ) THEN
                a(1) = ra
                b(1) = rb
                RETURN
            END IF
        END IF
        i = l
        j = l + l
        DO WHILE( j <= ir )
            IF ( j<ir  ) THEN
                IF ( a(j) < a(j+1) ) j = j + 1
            END IF
            IF ( ra < a(j) ) THEN
                a(i) = a(j)
                b(i) = b(j)
                i = j
                j = j + i
            ELSE
                j = ir + 1
            END IF
            a(i) = ra
            b(i) = rb
        END DO
    END DO

    !------------------------------------------------------------------------------
    END SUBROUTINE SortF
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    !> Sort an REAL array, and change the order of an index array accordingly.
    !------------------------------------------------------------------------------
    PURE SUBROUTINE SortD( n, a, b )
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN) :: n
    INTEGER(KIND=4), INTENT(INOUT) :: b(:)
    REAL(KIND=8), INTENT(INOUT) :: a(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: i, j, l, ir, rb
    REAL(KIND=8) :: ra
    !------------------------------------------------------------------------------

    IF ( n <= 1 ) RETURN

    l = n / 2 + 1
    ir = n
    DO WHILE( .TRUE. )

        IF ( l > 1 ) THEN
            l = l - 1
            ra = a(l)
            rb = b(l)
        ELSE
            ra = a(ir)
            rb = b(ir)
            a(ir) = a(1)
            b(ir) = b(1)
            ir = ir - 1
            IF ( ir == 1 ) THEN
                a(1) = ra
                b(1) = rb
                RETURN
            END IF
        END IF
        i = l
        j = l + l
        DO WHILE( j <= ir )
            IF ( j < ir  ) THEN
                IF ( a(j) < a(j+1) ) j = j + 1
            END IF
            IF ( ra < a(j) ) THEN
                a(i) = a(j)
                b(i) = b(j)
                i = j
                j = j + i
            ELSE
                j = ir + 1
            END IF
            a(i) = ra
            b(i) = rb
        END DO
    END DO

    !------------------------------------------------------------------------------
    END SUBROUTINE SortD
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    !> Sort an COMPLEX array, and organize an index table accordingly.
    !------------------------------------------------------------------------------
    PURE SUBROUTINE SortC( n, a, b )
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN) :: n
    INTEGER(KIND=4), INTENT(INOUT) :: b(:)
    COMPLEX(KIND=8), INTENT(INOUT) :: a(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: i, j, l, ir, rb
    COMPLEX(KIND=8) :: ra
    !------------------------------------------------------------------------------

    IF ( n <= 1 ) RETURN

    l = n / 2 + 1
    ir = n
    DO WHILE( .TRUE. )
        IF ( l > 1 ) THEN
            l = l - 1
            ra = a(l)
            rb = b(l)
        ELSE
            ra = a(ir)
            rb = b(ir)
            a(ir) = a(1)
            b(ir) = b(1)
            ir = ir - 1
            IF ( ir == 1 ) THEN
                a(1) = ra
                b(1) = rb
                RETURN
            END IF
        END IF
        i = l
        j = l + l
        DO WHILE( j <= ir )
            IF ( j < ir ) THEN
                IF ( ABS(a(j)) < ABS(a(j+1)) ) j = j + 1
            END IF
            IF ( ABS(ra) < ABS(a(j)) ) THEN
                a(i) = a(j)
                b(i) = b(j)
                i = j
                j = j + i
            ELSE
                j = ir + 1
            END IF
            a(i) = ra
            b(i) = rb
        END DO
    END DO

    !------------------------------------------------------------------------------
    END SUBROUTINE SortC
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    !> Order REAL components in b in a decreasing order and RETURN the new order
    !> of indexes in a.
    !------------------------------------------------------------------------------
    PURE SUBROUTINE SortR( n, a, b )
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN) :: n
    INTEGER(KIND=4), INTENT(INOUT) :: a(:)
    REAL(KIND=8), INTENT(INOUT) :: b(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: i, j, l, ir, ra
    REAL(KIND=8) :: rb
    !------------------------------------------------------------------------------

    IF ( n <= 1 ) RETURN

    l = n / 2 + 1
    ir = n
    DO WHILE( .TRUE. )

        IF ( l > 1 ) THEN
            l = l - 1
            ra = a(l)
            rb = b(l)
        ELSE
            ra = a(ir)
            rb = b(ir)
            a(ir) = a(1)
            b(ir) = b(1)
            ir = ir - 1
            IF ( ir == 1 ) THEN
                a(1) = ra
                b(1) = rb
                RETURN
            END IF
        END IF
        i = l
        j = l + l
        DO WHILE( j <= ir )
            IF ( j < ir  ) THEN
                IF ( b(j) > b(j+1) ) j = j + 1
            END IF
            IF ( rb > b(j) ) THEN
                a(i) = a(j)
                b(i) = b(j)
                i = j
                j = j + i
            ELSE
                j = ir + 1
            END IF
            a(i) = ra
            b(i) = rb
        END DO
    END DO

    !------------------------------------------------------------------------------
    END SUBROUTINE SortR
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    !> Search an INTEGER value in an ordered array.
    !------------------------------------------------------------------------------
    PURE FUNCTION SearchI( N, Array, Val ) RESULT ( Idx )
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN) :: N, Val, Array(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: Lower, Upper, Lou, Idx
    !------------------------------------------------------------------------------

    Idx = 0
    Upper = N
    Lower = 1

    ! Handle the special case

    IF ( Upper == 0 ) RETURN

    DO WHILE( .TRUE. )
        IF ( Array(Lower) == Val) THEN
            Idx = Lower
            EXIT
        ELSE IF ( Array(Upper) == Val ) THEN
            Idx = Upper
            EXIT
        END IF

        IF ( (Upper-Lower) > 1 ) THEN
            Lou = ISHFT((Upper + Lower), -1)
            IF ( Array(Lou) < Val ) THEN
                Lower = Lou
            ELSE
                Upper = Lou
            END IF
        ELSE
            EXIT
        END IF
    END DO

    RETURN

    !------------------------------------------------------------------------------
    END FUNCTION SearchI
    !------------------------------------------------------------------------------

    !------------------------------------------------------------------------------
    !> Search a REAL value in an ordered array.
    !------------------------------------------------------------------------------
    PURE FUNCTION SearchR( N, Array, Val ) RESULT ( Idx )
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------------
    INTEGER(KIND=4), INTENT(IN) :: N
    INTEGER(KIND=4) :: Idx
    REAL(KIND=8), INTENT(IN) :: Val, Array(:)
    !------------------------------------------------------------------------------
    INTEGER(KIND=4) :: Lower, Upper, Lou
    !------------------------------------------------------------------------------

    Idx = 0
    Upper = N
    Lower = 1

    ! Handle the special case
    IF ( Upper == 0 ) RETURN

    DO WHILE( .TRUE. )
        IF ( ABS( Array(Lower) - Val) < TINY(Val)  ) THEN
            Idx = Lower
            EXIT
        ELSE IF ( ABS( Array(Upper) - Val ) < TINY(Val) ) THEN
            Idx = Upper
            EXIT
        END IF

        IF ( (Upper-Lower) > 1 ) THEN
            Lou = ISHFT((Upper + Lower), -1)
            IF ( Array(Lou) < Val ) THEN
                Lower = Lou
            ELSE
                Upper = Lou
            END IF
        ELSE
            EXIT
        END IF
    END DO

    RETURN

    !------------------------------------------------------------------------------
    END FUNCTION SearchR
    !------------------------------------------------------------------------------

    !-------------------------------
    !Sort the integer array (from small to large) and return the index
    !-------------------------------
    SUBROUTINE sort_int(n , a , indx)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    INTEGER(KIND=4) :: n
    INTEGER(KIND=4) , DIMENSION(n) :: a
    INTEGER(KIND=4) , DIMENSION(n) :: indx
    !-------------------------------
    INTEGER(KIND=4) :: i
    !-------------------------------
    INTEGER(KIND=4) , ALLOCATABLE , DIMENSION(:) :: b
    INTEGER(KIND=4) :: stat
    CHARACTER(LEN=256) :: errmsg
    !-------------------------------
    ALLOCATE(b(n) , stat=stat , errmsg=errmsg)
    b = a
    DO i = 1 , n
        indx(i) = i
    END DO
    CALL quick_sort_int(n , b , 1 , n , indx)
    DEALLOCATE(b , stat=stat , errmsg=errmsg)

    END SUBROUTINE
    !-------------------------------
    !Sort the real array (from small to large) and return the index
    !-------------------------------
    SUBROUTINE sort_real(n , a , indx)
    IMPLICIT NONE
    !-------------------------------
    INTEGER(KIND=4) :: n
    REAL(KIND=8) , DIMENSION(n) :: a
    INTEGER(KIND=4) , DIMENSION(n) :: indx
    !-------------------------------
    INTEGER(KIND=4) :: i
    REAL(KIND=8) , ALLOCATABLE , DIMENSION(:) :: b
    INTEGER(KIND=4) :: stat
    CHARACTER(LEN=256) :: errmsg
    !-------------------------------
    ALLOCATE(b(n) , stat=stat , errmsg=errmsg)
    b = a
    DO i = 1 , n
        indx(i) = i
    END DO
    CALL quick_sort_real(n , b , 1 , n , indx)
    DEALLOCATE(b , stat=stat , errmsg=errmsg)
    END SUBROUTINE
    !-------------------------------
    !Use the quick sort method to quickly sort an integer array
    !-------------------------------
    RECURSIVE SUBROUTINE quick_sort_int(n , a , s , e , indx)
    IMPLICIT NONE
    ! Represents the size of the type
    INTEGER(KIND=4) :: n
    ! 存放数据的类型
    INTEGER(KIND=4) , DIMENSION(n) :: a
    ! The parameters passed in, the starting position of the type of this group
    INTEGER(KIND=4) :: s
    ! The parameters passed in, the ending position of the type of this group
    INTEGER(KIND=4) :: e
    ! Index to store data
    INTEGER(KIND=4) , DIMENSION(n) :: indx
    !-------------------------------
    ! 
    INTEGER(KIND=4) :: l,r
    ! 
    INTEGER(KIND=4) :: k
    ! 
    INTEGER(KIND=4) :: temp
    ! 
    INTEGER(KIND=4) :: indxt
    !-------------------------------
    ! First, the initial values of l and r must be given. l must start from the beginning, and e must start from the end.
    l = s
    r = e + 1
    ! 
    IF ( r <= l ) RETURN
    k = a(s) 
    DO WHILE(.true.)
        ! 
        DO WHILE( .true. )
            l = l + 1
            IF(l >= e)EXIT
            IF ( (a(l) > k) ) EXIT
        END DO
        ! 
        DO WHILE( .true. )
            r = r - 1
            IF ( (a(r) < k) .or. (r <= s) ) EXIT
        END DO
        ! 
        IF ( r <= l ) EXIT
        ! 
        temp = a(l)
        a(l) = a(r)
        a(r) = temp
        ! 
        indxt = indx(l)
        indx(l) = indx(r)
        indx(r) = indxt
    END DO
    ! 
    temp = a(s)
    a(s) = a(r)
    a(r) = temp
    ! 
    indxt = indx(s)
    indx(s) = indx(r)
    indx(r) = indxt
    ! 
    CALL quick_sort_int(n , a , s , r-1 , indx)
    ! 
    CALL quick_sort_int(n , a , r+1 , e , indx)
    RETURN
    END SUBROUTINE quick_sort_int
    !-------------------------------
    !Quick sort real array using quick sort method
    !-------------------------------
    RECURSIVE SUBROUTINE quick_sort_real(n , a , s , e , indx)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    ! 
    INTEGER(KIND=4) :: n
    ! 
    REAL(KIND=8) , DIMENSION(n) :: a
    !
    INTEGER(KIND=4) , DIMENSION(n) :: indx
    ! 
    INTEGER(KIND=4) :: s
    ! 
    INTEGER(KIND=4) :: e
    !-------------------------------
    ! 
    INTEGER(KIND=4) :: l
    INTEGER(KIND=4) :: r
    ! 
    REAL(KIND=8) :: k
    ! 
    REAL(KIND=8) :: temp
    ! 
    INTEGER(KIND=4) :: indxt
    !-------------------------------
    
    l = s
    r = e + 1
    
    IF ( r <= l ) RETURN
    k = a(s) ! 
    DO WHILE(.true.)
        ! 
        DO WHILE( .true. )
            l = l + 1
            IF(l >= e)EXIT
            IF ( (a(l) > k) .or. (l >= e) ) EXIT
        END DO
        ! 
        DO WHILE( .true. )
            r = r-1
            IF ( (a(r) < k) .or. (r <= s) ) EXIT
        END DO
        ! 
        IF ( r <= l ) EXIT
        ! 
        temp = a(l)
        a(l) = a(r)
        a(r) = temp
        !
        indxt = indx(l)
        indx(l) = indx(r)
        indx(r) = indxt
    END DO
    ! 
    temp = a(s)
    a(s) = a(r)
    a(r) = temp
    ! 
    indxt = indx(s)
    indx(s) = indx(r)
    indx(r) = indxt
    ! 
    CALL quick_sort_real(n , a , s , r-1 , indx)
    ! 
    CALL quick_sort_real(n , a , r+1 , e , indx)
    RETURN

    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    END MODULE

