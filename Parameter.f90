    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  Parameter Module
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    MODULE ParameterMod
    IMPLICIT NONE
    !---------------------------------------
    !
    !
    REAL(KIND=8) , PARAMETER , PUBLIC :: pi = 3.14159265358979323846264d0
    !
    REAL(KIND=8) , PARAMETER , PUBLIC :: muz = 4.0d0*pi*1.0d-7
    !
    REAL(KIND=8) , PARAMETER , PUBLIC :: epsilonz = 8.854187817d-12

    ! REAL and COMPLEX precision constants / tolerance
    REAL (KIND=8) , PARAMETER :: LARGE_REAL = 1.0d13
    REAL (KIND=8) , PARAMETER :: TOL4= 0.0001d0
    REAL (KIND=8) , PARAMETER :: TOL6= 0.000001d0
    REAL (KIND=8) , PARAMETER :: TOL8= 0.00000001d0
    REAL (KIND=8) , PARAMETER :: EIGHT = 8.0d0
    REAL (KIND=8) , PARAMETER :: THREE = 3.0d0
    REAL (KIND=8) , PARAMETER :: TWO = 2.0d0
    REAL (KIND=8) , PARAMETER :: ONE = 1.0d0
    REAL (KIND=8) , PARAMETER :: R_ZERO = 0.0d0
    REAL (KIND=8) , PARAMETER :: MinusONE = -1.0d0
    REAL (KIND=8) , PARAMETER :: MinusTWO = -2.0d0

    COMPLEX(KIND=8) , PARAMETER :: C_ONE = (1.0d0,0.0d0)
    COMPLEX(KIND=8) , PARAMETER :: C_ONEI = (0.0d0,1.0d0)
    COMPLEX(KIND=8) , PARAMETER :: C_ZERO = (0.0d0, 0.0d0)
    COMPLEX(KIND=8) , PARAMETER :: C_MinusOne = (-1.0d0, 0.0d0)

    ! Important: sign convention used throughout the program
    INTEGER(KIND=4) , PARAMETER             :: ISIGN = 1

    CHARACTER(LEN=80) , PARAMETER :: FACE = 'FACE'
    CHARACTER(LEN=80) , PARAMETER :: EDGE = 'EDGE'
    CHARACTER(LEN=80) , PARAMETER :: NODE = 'NODE'
    CHARACTER(LEN=80) , PARAMETER :: ELEMENT = 'ELEMENT'

    CHARACTER(LEN=80) , PARAMETER :: MUMPS = 'MUMPS'
    CHARACTER(LEN=80) , PARAMETER :: MUMPS_VERSION = '5.1.1'
    CHARACTER(LEN=80) , PARAMETER :: ZMUMPS = 'ZMUMPS'
    CHARACTER(LEN=80) , PARAMETER :: ZMUMPS_VERSION = '5.1.1'
    CHARACTER(LEN=80) , PARAMETER :: DMUMPS = 'DMUMPS'
    CHARACTER(LEN=80) , PARAMETER :: DMUMPS_VERSION = '5.1.1'
    CHARACTER(LEN=80) , PARAMETER :: PARDISO = 'PARDISO'
    CHARACTER(LEN=80) , PARAMETER :: PARDISO_VERSION = '5.0.0'

    CHARACTER(LEN=80) , PARAMETER :: TETGEN ='TETGEN'
    CHARACTER(LEN=80) , PARAMETER :: TETGEN_VERSION = '1.5.1-beta1'
    CHARACTER(LEN=80) , PARAMETER :: VTK ='VTK'
    CHARACTER(LEN=80) , PARAMETER :: VTK_VERSION = '3.0'

    ! Useful conversion constants
    real (kind=8) , parameter     :: D2R = PI/180.0d0
    real (kind=8) , parameter     :: R2D = 180.d0/PI
    real (kind=8) , parameter     :: KM2M = 1000.0d0
    real (kind=8) , parameter     :: M2KM = 0.001d0

    END MODULE
