    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  Timer Module
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    MODULE TimerMod
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    PRIVATE
    !---------------------------------------
    !
    !---------------------------------------
    PUBLIC :: timer_t
    !---------------------------------------
    !
    !---------------------------------------
    TYPE timer_t
        PRIVATE
        CHARACTER(LEN = 256) :: name = ''
        INTEGER(KIND=4) :: clock_start = 0
        INTEGER(KIND=4) :: clock_terminate = 0
        INTEGER(KIND=4) :: clock_max = 0
        INTEGER(KIND=4) :: clock_rate = 0
        INTEGER(KIND=4) , DIMENSION(8) :: values_start = 0
        INTEGER(KIND=4) , DIMENSION(8) :: values_terminate = 0
        INTEGER(KIND=4) , DIMENSION(8) :: values_current = 0
        LOGICAL(KIND=4) :: already_start = .false.
        LOGICAL(KIND=4) :: already_terminate = .false.
        CHARACTER(LEN=256) :: string_start_time = ''
        CHARACTER(LEN=256) :: string_current_time = ''
        CHARACTER(LEN=256) :: string_terminate_time = ''
        CHARACTER(LEN=256) :: string_run_time = ''
    CONTAINS
    PROCEDURE , PASS :: start
    PROCEDURE , PASS :: terminate
    PROCEDURE , PASS :: write_start_time
    PROCEDURE , PASS :: write_current_time
    PROCEDURE , PASS :: write_terminate_time
    PROCEDURE , PASS :: write_run_time
    END TYPE
    !---------------------------------------
    !
    !---------------------------------------
    CONTAINS
    !---------------------------------------
    !
    !---------------------------------------
    SUBROUTINE start( this , name )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    CLASS(timer_t) :: this
    !---------------------------------------
    CHARACTER(LEN=*) , INTENT(IN) , OPTIONAL :: name
    !---------------------------------------
    !IF( .not. this%already_start )THEN
    IF( PRESENT(name) )THEN
        this%name = TRIM(ADJUSTL(name))
    END IF
    CALL DATE_AND_TIME ( values = this%values_start )
    CALL SYSTEM_CLOCK ( this%clock_start, this%clock_rate, this%clock_max )
    this%already_start = .true.
    CALL change_time_to_string ( this%values_start , this%string_start_time )
    IF( LEN(this%name) /= 0 )THEN
        this%string_start_time = TRIM(ADJUSTL(this%string_start_time)) // ' ' // TRIM(ADJUSTL(this%name)) // ' ' // 'started.'
    END IF
    !END IF
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !½áÊø¼ÆÊ±
    !---------------------------------------
    SUBROUTINE terminate( this )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    CLASS(timer_t) :: this
    !---------------------------------------
    !IF( .not. this%already_start )THEN
    !    CALL this%start( )
    !END IF
    !IF( .not. this%already_terminate)THEN
    CALL DATE_AND_TIME ( values = this%values_terminate )
    CALL SYSTEM_CLOCK ( this%clock_terminate, this%clock_rate, this%clock_max )
    this%already_terminate = .true.
    CALL change_time_to_string ( this%values_terminate , this%string_terminate_time )
    IF( LEN(this%name) /= 0 )THEN
        this%string_terminate_time = &
            TRIM(ADJUSTL(this%string_terminate_time)) // ' ' // TRIM(ADJUSTL(this%name)) // ' ' // 'finished.'
    END IF
    !END IF
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    SUBROUTINE write_start_time( this , unit )
    !---------------------------------------
    USE iso_fortran_env , ONLY : output_unit
    IMPLICIT NONE
    !---------------------------------------
    CLASS(timer_t) :: this
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !---------------------------------------
    IF( .not. this%already_start )THEN
        CALL this%start( )
    END IF
    IF(.not. PRESENT(unit))THEN
        WRITE(unit=output_unit,fmt=*)TRIM(ADJUSTL(this%string_start_time))
    ELSE
        WRITE(unit=unit,fmt=*)TRIM(ADJUSTL(this%string_start_time))
    END IF
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    SUBROUTINE write_current_time( this , unit )
    !---------------------------------------
    USE iso_fortran_env , ONLY : output_unit
    IMPLICIT NONE
    !---------------------------------------
    CLASS(timer_t) :: this
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !---------------------------------------
    CALL DATE_AND_TIME ( values = this%values_current )
    CALL change_time_to_string ( this%values_current, this%string_current_time )
    IF(.not. PRESENT(unit))THEN
        WRITE(unit=output_unit,fmt=*)TRIM(ADJUSTL(this%string_current_time))
    ELSE
        WRITE(unit=unit,fmt=*)TRIM(ADJUSTL(this%string_current_time))
    END IF
    RETURN
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    SUBROUTINE write_terminate_time( this , unit )
    !---------------------------------------
    USE iso_fortran_env , ONLY : output_unit
    IMPLICIT NONE
    !---------------------------------------
    CLASS(timer_t) :: this
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !---------------------------------------
    IF( .not. this%already_terminate )THEN
        CALL this%terminate( )
    END IF
    IF(.not. PRESENT(unit))THEN
        WRITE(unit=output_unit,fmt=*)TRIM(ADJUSTL(this%string_terminate_time))
    ELSE
        WRITE(unit=unit,fmt=*)TRIM(ADJUSTL(this%string_terminate_time))
    END IF
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    SUBROUTINE write_run_time ( this , unit )
    !---------------------------------------
    USE iso_fortran_env , ONLY : output_unit
    IMPLICIT NONE
    !---------------------------------------
    CLASS(timer_t) :: this
    INTEGER(KIND=4) , INTENT(IN) , OPTIONAL :: unit
    !---------------------------------------
    INTEGER(KIND=4) :: h
    INTEGER(KIND=4) :: m
    INTEGER(KIND=4) :: s
    INTEGER(KIND=4) :: ms
    REAL(KIND=8) :: ts
    CHARACTER(LEN=256) :: temp
    !---------------------------------------
    !CALL this%terminate( )
    ! 
    ts = DBLE(this%clock_terminate - this%clock_start)/DBLE(this%clock_rate)
    ! 
    s = INT(ts)
    ! 
    ms = 1000.0d0 * (ts - s)
    ! 
    m = (s - MOD(s,60))/60
    ! 
    s = MOD(s,60)
    ! 
    h = (m - MOD(m,60))/60
    ! 
    m = MOD(m,60)

    WRITE(temp,'(i2)')h
    this%string_run_time =  'Delta Wallclock: '//TRIM(ADJUSTL(temp))//'h:'
    WRITE(temp,'(i2.2)')m
    this%string_run_time = TRIM(ADJUSTL(this%string_run_time))//TRIM(ADJUSTL(temp))//'m:'
    WRITE(temp,'(i2.2)')s
    this%string_run_time = TRIM(ADJUSTL(this%string_run_time))//TRIM(ADJUSTL(temp))//'s.'
    WRITE(temp,'(i3.3)')ms
    this%string_run_time = TRIM(ADJUSTL(this%string_run_time))//TRIM(ADJUSTL(temp))&
        //'ms '//'for the task: '//TRIM(ADJUSTL(this%name))//'.'

    IF(.not. PRESENT(unit))THEN
        WRITE(unit=output_unit,fmt='(1x,A)')TRIM(ADJUSTL(this%string_run_time))
    ELSE
        WRITE(unit=unit,fmt='(1x,A)')TRIM(ADJUSTL(this%string_run_time))
    END IF
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    SUBROUTINE change_time_to_string ( values , string )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    INTEGER(KIND=4) , DIMENSION(8) , INTENT(IN) :: values
    CHARACTER(LEN=*) , INTENT(OUT) :: string
    !---------------------------------------
    CHARACTER(LEN=8) :: ampm
    INTEGER(KIND=4) :: d
    INTEGER(KIND=4) :: h
    INTEGER(KIND=4) :: m
    INTEGER(KIND=4) :: mm
    CHARACTER(LEN=9) , PARAMETER , DIMENSION(12) :: month = (/ &
        'January  ', 'February ', 'March    ', 'April    ', &
        'May      ', 'June     ', 'July     ', 'August   ', &
        'September', 'October  ', 'November ', 'December ' /)
    INTEGER(KIND=4) :: n
    INTEGER(KIND=4) :: s
    INTEGER(KIND=4) :: y
    !---------------------------------------

    y = values(1)
    m = values(2)
    d = values(3)
    h = values(5)
    n = values(6)
    s = values(7)
    mm = values(8)

    IF ( h < 12 ) THEN
        ampm = 'AM'
    ELSE IF ( h == 12 ) THEN
        IF ( n == 0 .and. s == 0 ) THEN
            ampm = 'Noon'
        ELSE
            ampm = 'PM'
        END IF
    ELSE
        h = h - 12
        IF ( h < 12 ) THEN
            ampm = 'PM'
        ELSE IF ( h == 12 ) THEN
            IF ( n == 0 .and. s == 0 ) THEN
                ampm = 'Midnight'
            ELSE
                ampm = 'AM'
            END IF
        END IF
    END IF

    WRITE ( string, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
        d, TRIM ( month(m) ), y, h, ':', n, ':', s, '.', mm, TRIM ( ampm )

    string = 'Wallclock: '//TRIM(ADJUSTL(string))

    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    SUBROUTINE get_hms ( values , h , m , s , mm )
    !---------------------------------------
    IMPLICIT NONE
    !---------------------------------------
    INTEGER(KIND=4) , DIMENSION(8) , INTENT(IN) :: values
    INTEGER(KIND=4) , INTENT(OUT) :: h
    INTEGER(KIND=4) , INTENT(OUT) :: m
    INTEGER(KIND=4) , INTENT(OUT) :: s
    INTEGER(KIND=4) , INTENT(OUT) :: mm
    !---------------------------------------
    h = values(5)
    m = values(6)
    s = values(7)
    mm = values(8)
    RETURN
    !---------------------------------------
    END SUBROUTINE
    !---------------------------------------
    !
    !---------------------------------------
    END MODULE
