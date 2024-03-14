    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  Point Module
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    MODULE PointMod
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    PRIVATE
    !-------------------------------
    !
    PUBLIC :: Point_t
    !
    PUBLIC :: CrossProduct
    PUBLIC :: DotProduct
    PUBLIC :: MinusVector
    PUBLIC :: PlusVector
    PUBLIC :: DataVector
    PUBLIC :: MultValue
    PUBLIC :: Distance
    PUBLIC :: Length
    PUBLIC :: Same
    !---------------------------------------
    !
    !---------------------------------------
    TYPE Point_t
        REAL(KIND=8) :: X
        REAL(KIND=8) :: Y
        REAL(KIND=8) :: Z
    END TYPE
    !-------------------------------
    !
    !-------------------------------
    CONTAINS
    !-------------------------------
    !
    !-------------------------------
    TYPE(Point_t) FUNCTION CrossProduct(a , b)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: a
    TYPE(Point_t) , INTENT(IN) :: b
    !-------------------------------
    CrossProduct%x = +(a%y*b%z - a%z*b%y)
    CrossProduct%y = -(a%x*b%z - a%z*b%x)
    CrossProduct%z = +(a%x*b%y - a%y*b%x)
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    REAL(KIND=8) FUNCTION DotProduct(a , b)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: a
    TYPE(Point_t) , INTENT(IN) :: b
    !-------------------------------
    DotProduct = a%x*b%x + a%y*b%y + a%z*b%z
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    TYPE(Point_t) FUNCTION MultValue(value , vector)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    REAL(KIND=8) , INTENT(IN) :: value
    !
    TYPE(Point_t) , INTENT(IN) :: vector
    !-------------------------------
    MultValue%x = value*vector%x
    MultValue%y = value*vector%y
    MultValue%z = value*vector%z
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    TYPE(Point_t) FUNCTION MinusVector(a , b)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: a
    TYPE(Point_t) , INTENT(IN) :: b
    !-------------------------------
    MinusVector%x = a%x - b%x
    MinusVector%y = a%y - b%y
    MinusVector%z = a%z - b%z
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    TYPE(Point_t) FUNCTION PlusVector(a , b)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: a
    TYPE(Point_t) , INTENT(IN) :: b
    !-------------------------------
    PlusVector%x = a%x + b%x
    PlusVector%y = a%y + b%y
    PlusVector%z = a%z + b%z
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE DataVector(a , b)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: a
    !-------------------------------
    TYPE(Point_t) , INTENT(OUT) :: b
    !-------------------------------
    b%x = a%x
    b%y = a%y
    b%z = a%z
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    REAL(KIND=8) FUNCTION Distance(a , b)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    TYPE(Point_t) , INTENT(IN) :: a
    TYPE(Point_t) , INTENT(IN) :: b
    !-------------------------------
    Distance = SQRT((a%x - b%x)**2 + (a%y - b%y)**2 + (a%z - b%z)**2)
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    REAL(KIND=8) FUNCTION Length(a)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    TYPE(Point_t) , INTENT(IN) :: a
    !-------------------------------
    Length = SQRT(a%x**2 + a%y**2 + a%z**2)
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    INTEGER(KIND=4) FUNCTION Same(a , b)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    TYPE(Point_t) , INTENT(IN) :: a
    TYPE(Point_t) , INTENT(IN) :: b
    !-------------------------------
    REAL(KIND=8) :: r
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    r = Distance(a , b)
    IF(r < Residual)THEN
        Same = 1
    ELSE
        Same = 0
    END IF
    !-------------------------------
    END FUNCTION
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE CopyVector(b , a)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: a
    !-------------------------------
    TYPE(Point_t) , INTENT(OUT) :: b
    !-------------------------------
    b%x = a%x
    b%y = a%y
    b%z = a%z
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    END MODULE
