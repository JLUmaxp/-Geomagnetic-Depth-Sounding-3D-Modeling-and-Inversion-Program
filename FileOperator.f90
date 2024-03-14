    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  File Operator Module
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    MODULE FileOperatorMod
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    ! default PRIVATE
    PRIVATE
    !---------------------------------------
    PUBLIC :: copy_file
    PUBLIC :: rename_file
    PUBLIC :: delete_file
    PUBLIC :: make_dir
    PUBLIC :: get_dir
    !---------------------------------------
#IF defined (_WIN32) || defined (WIN32) || defined (_WIN64) || defined (WIN64)
    CHARACTER(LEN=*) , PARAMETER :: path_separator = '\'
    CHARACTER(LEN=*) , PARAMETER :: copy_cmd   = 'copy /y'
    CHARACTER(LEN=*) , PARAMETER :: rename_cmd = 'rename'
    CHARACTER(LEN=*) , PARAMETER :: delete_cmd = 'del'
    CHARACTER(LEN=*) , PARAMETER :: mkdir_cmd  = 'mkdir'
#ELSE 
    !---------------------------------------
    CHARACTER(LEN=*) , PARAMETER :: path_separator = '/'
    CHARACTER(LEN=*) , PARAMETER :: copy_cmd   = 'cp'
    CHARACTER(LEN=*) , PARAMETER :: rename_cmd = 'mv'
    CHARACTER(LEN=*) , PARAMETER :: delete_cmd = 'rm'
    CHARACTER(LEN=*) , PARAMETER :: mkdir_cmd  = 'mkdir -p'
#ENDIF 
    !---------------------------------------
    CONTAINS
    !---------------------------------------
    ! copy file from source to destination
    !---------------------------------------
    SUBROUTINE copy_file( src, dst )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: src, dst
    !---------------------------------------
    CALL system( copy_cmd // ' ' // TRIM(src) // ' ' // TRIM(dst) )
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    ! rename file
    !---------------------------------------
    SUBROUTINE rename_file( src, dst )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: src, dst
    !---------------------------------------
    CALL system( rename_cmd // ' ' // TRIM(src) // ' ' // TRIM(dst) )
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    ! delete file
    !---------------------------------------
    SUBROUTINE delete_file( file )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: file
    !---------------------------------------
    CALL system( delete_cmd // ' ' // TRIM(file) )
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    ! make dir
    !---------------------------------------
    SUBROUTINE make_dir( dir )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: dir
    !---------------------------------------
    CALL system( mkdir_cmd // ' ' // TRIM(dir) )
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    ! get dir
    !---------------------------------------
    SUBROUTINE get_dir ( dir )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    CHARACTER(LEN=256) , INTENT(OUT) :: dir
    !---------------------------------------
    CALL getcwd(dir)
    dir = TRIM(ADJUSTL(dir)) // path_separator
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    END MODULE