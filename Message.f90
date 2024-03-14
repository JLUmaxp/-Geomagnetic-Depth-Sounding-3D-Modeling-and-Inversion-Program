    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  Message Module
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    MODULE MessageMod
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    PRIVATE
    PUBLIC :: Message_t
    !-------------------------------
    !
    !-------------------------------
    TYPE Message_t
         !stat indicates running status
         !=0 means normal operation
         !=1 means an error occurred during operation
         !=2 means a warning occurs during operation
        INTEGER(KIND=4) :: stat = 0
        !
        CHARACTER(LEN=256) :: warning = ''
        !
        CHARACTER(LEN=256) :: error = ''
        !
        CHARACTER(LEN=256) :: caller = ''
        !
        CHARACTER(LEN=256) :: action = ''
        !
        CHARACTER(LEN=256) :: time = ''
    END TYPE
    !-------------------------------
    !
    !-------------------------------
    CONTAINS
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE ProcessMessage(Message, unit)
    !-------------------------------
    USE ISO_FORTRAN_ENV , ONLY : OUTPUT_UNIT
    IMPLICIT NONE
    !-------------------------------
    TYPE(Message_t) , INTENT(IN) :: Message
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !-------------------------------
    INTEGER(KIND=4) :: wunit
    LOGICAL(KIND=4) :: exist
    !-------------------------------
    IF(PRESENT(unit))THEN
        INQUIRE(unit=unit, exist=exist)
        IF(exist)THEN
            wunit = unit
        ELSE
            wunit = output_unit
        END IF
    ELSE
        wunit = output_unit
    END IF

    IF(Message%stat == 0)THEN
        CALL Info(Message%caller , Message%action , wunit)
    ELSE IF(Message%stat == 1)THEN
        CALL Warn(Message%caller , Message%warning , wunit)
    ELSE IF(Message%stat == 2)THEN
        CALL Error(Message%caller , Message%error , wunit)
    END IF

    RETURN
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE Info(caller , string , unit)
    !-------------------------------
    USE ISO_FORTRAN_ENV , ONLY : OUTPUT_UNIT
    IMPLICIT NONE
    !-------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: caller
    CHARACTER(LEN=*) , INTENT(IN) :: string
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !-------------------------------
    INTEGER(KIND=4) :: wunit
    LOGICAL(KIND=4) :: exist
    !-------------------------------
    IF(PRESENT(unit))THEN
        INQUIRE(unit=unit, exist=exist)
        IF(exist)THEN
            wunit = unit
        ELSE
            wunit = output_unit
        END IF
    ELSE
        wunit = output_unit
    END IF

    WRITE(wunit,'(a,1x,a)')'INFO::',TRIM(ADJUSTL(caller))
    WRITE(wunit,'(7x,a)')TRIM(ADJUSTL(string))
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE Warn(caller , string , unit)
    !-------------------------------
    USE ISO_FORTRAN_ENV , ONLY : OUTPUT_UNIT
    IMPLICIT NONE
    !-------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: caller
    CHARACTER(LEN=*) , INTENT(IN) :: string
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !-------------------------------
    INTEGER(KIND=4) :: wunit
    LOGICAL(KIND=4) :: exist
    !-------------------------------
    IF(PRESENT(unit))THEN
        INQUIRE(unit=unit, exist=exist)
        IF(exist)THEN
            wunit = unit
        ELSE
            wunit = output_unit
        END IF
    ELSE
        wunit = output_unit
    END IF

    WRITE(wunit,'(a,1x,a)')'WARN::',caller
    WRITE(wunit,'(7x,a)')string

    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE Error(caller , string , unit)
    !-------------------------------
    USE ISO_FORTRAN_ENV , ONLY : OUTPUT_UNIT
    IMPLICIT NONE
    !-------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: caller
    CHARACTER(LEN=*) , INTENT(IN) :: string
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !-------------------------------
    INTEGER(KIND=4) :: wunit
    LOGICAL(KIND=4) :: exist
    !-------------------------------
    IF(PRESENT(unit))THEN
        INQUIRE(unit=unit, exist=exist)
        IF(exist)THEN
            wunit = unit
        ELSE
            wunit = output_unit
        END IF
    ELSE
        wunit = output_unit
    END IF

    WRITE(wunit,'(a,1x,a)')'ERROR::',TRIM(ADJUSTL(caller))
    WRITE(wunit,'(8x,a)')TRIM(ADJUSTL(string))

    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE Fatal(caller , string , unit)
    !-------------------------------
    USE ISO_FORTRAN_ENV , ONLY : OUTPUT_UNIT
    IMPLICIT NONE
    !-------------------------------
    CHARACTER(LEN=*) , INTENT(IN) :: caller
    CHARACTER(LEN=*) , INTENT(IN) :: string
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !-------------------------------
    INTEGER(KIND=4) :: wunit
    LOGICAL(KIND=4) :: exist
    !-------------------------------
    IF(PRESENT(unit))THEN
        INQUIRE(unit=unit, exist=exist)
        IF(exist)THEN
            wunit = unit
        ELSE
            wunit = output_unit
        END IF
    ELSE
        wunit = output_unit
    END IF

    WRITE(wunit,'(a,1x,a)')'ERROR::',TRIM(ADJUSTL(caller))
    WRITE(wunit,'(8x,a)')TRIM(ADJUSTL(string))

    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    END MODULE
