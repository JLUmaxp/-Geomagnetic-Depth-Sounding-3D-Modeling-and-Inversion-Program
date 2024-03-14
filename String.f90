    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !-----------------------------------------------------------------------
    !
    !    Copyright 2008-2013
    !    Kerry Key
    !    Scripps Institution of Oceanography
    !    kkey@ucsd.edu
    !
    !    This file is part of MARE2DEM.
    !
    !    MARE2DEM is free software: you can redistribute it and/or modify
    !    it under the terms of the GNU General PUBLIC License as published by
    !    the Free Software Foundation, either version 3 of the License, or
    !    (at your option) any later version.
    !
    !    MARE2DEM is distributed in the hope that it will be useful,
    !    but WITHOUT ANY WARRANTY; without even the implied warranty of
    !    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !    GNU General PUBLIC License for more details.
    !
    !    You should have received a copy of the GNU General PUBLIC License
    !    along with MARE2DEM.  IF not, see <http://www.gnu.org/licenses/>.
    !
    !-----------------------------------------------------------------------
    MODULE StringMod
    !
    ! THIS HAS A BUNCH OF USEFUL FUNCTIONS WRITTEN BY DAVID MYER AND MYSELF FOR DECOMPOSING INPUT STRINGS INTO COMPONENTS.
    !
    IMPLICIT NONE
    PRIVATE
    PUBLIC  :: PARSECODE
    PUBLIC  :: PARSEFIELDS
    PUBLIC  :: PARSELINE
    PUBLIC  :: FILEPARTS
    PUBLIC  :: LOWER
    PUBLIC  :: UPPER
    PUBLIC  :: CONVERTSTRTOINTEGER
    CONTAINS
    !------------------------------------------------------------------------
    !
    ! PARSE CODE
    !
    !------------------------------------------------------------------------
    SUBROUTINE PARSECODE(SLINE, SCODE, SVALUE, BCOMMENT)

    ! DAVID MYER IGPP/SIO LA JOLLA CA 92093-0225
    ! SUBROUTINE REVISION 3.0, NOVEMBER 2006
    ! DGM NOV 2006 - PARSE A LINE READ FROM A FILE INTO A CODE & VALUE.
    ! FORCE THE CODE TO BE ALL LOWERCASE WITH NO ENDING COLON.  TERMINATE
    ! THE LINE AT A '%' OR '!' SIGN (THESE ALLOW FOR USER COMMENTS!)
    !
    IMPLICIT NONE

    ! ARGS

    CHARACTER(LEN=*)            :: SLINE
    CHARACTER(LEN=:) , ALLOCATABLE , INTENT(OUT) :: SCODE, SVALUE
    LOGICAL(KIND=4) , INTENT(OUT) :: BCOMMENT

    ! LOCAL VARS
    INTEGER(KIND=4) :: IFROM, ITO, NLEN

    ! INIT RETURNS
    BCOMMENT = .FALSE.
    SCODE = ' '
    SVALUE = ' '

    NLEN = LEN_TRIM(SLINE)

    ! CONVERT ALL TAB CHARACTERS TO SPACES
    FORALL( ITO = 1:NLEN, ICHAR(SLINE(ITO:ITO)) == 9 ) SLINE(ITO:ITO) = ' '

    ! SKIP ANY BEGINNING BLANKS
    DO IFROM = 1,NLEN
        IF (SLINE(IFROM:IFROM) .NE. ' ') EXIT
    END DO

    ! IF THE FIRST CHAR IS A COMMENT CHAR, THEN THE WHOLE LINE IS A COMMENT.
    IF( SLINE(IFROM:IFROM) == '%' .OR. SLINE(IFROM:IFROM) == '!' ) THEN
        BCOMMENT = .TRUE.
        RETURN
    END IF

    ! PULL OFF THE CODE VALUE.  CVT TO LOWERCASE AS WE GO.
    ITO = INDEX(SLINE,'=') - 1
    IF (ITO < IFROM) THEN
        BCOMMENT = .TRUE. ! KWK FEB 2008 TREAT BLANK LINE LIKE A COMMENT
        RETURN
    END IF
    SCODE = SLINE(IFROM:ITO)
    CALL LOWER(SCODE)

    ! SKIP SPACES AFTER THE COLON
    DO IFROM = ITO+2,NLEN
        IF (SLINE(IFROM:IFROM) .NE. ' ') EXIT
    END DO

    ! GET THE REST, UP TO ANY COMMENT
    SVALUE = SLINE(IFROM:)
    ITO = LEN_TRIM(SVALUE)
    IFROM = INDEX(SVALUE,'%')
    IF (IFROM > 0 .AND. IFROM < ITO) THEN
        SVALUE(IFROM:ITO) = ' '
    END IF
    IFROM = INDEX(SVALUE,'!')
    IF (IFROM > 0 .AND. IFROM < ITO) THEN
        SVALUE(IFROM:ITO) = ' '
    END IF

    SCODE = TRIM(ADJUSTL(SCODE))
    CALL LOWER(SCODE)
    SVALUE = TRIM(ADJUSTL(SVALUE))

    END SUBROUTINE PARSECODE
    !------------------------------------------------------------------------
    !
    ! LOWER
    !
    !------------------------------------------------------------------------
    SUBROUTINE LOWER(S)

    ! DAVID MYER IGPP/SIO LA JOLLA CA 92093-0225
    ! DGM NOV 2006 - CONVERT STRING TO LOWER CASE
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(OUT)  :: S
    INTEGER(KIND=4) :: I

    DO  I = 1 , LEN_TRIM(S)
        IF  ( S(I:I) >= 'A' .AND. S(I:I) <= 'Z' ) THEN
            S(I:I) = CHAR(ICHAR(S(I:I)) + 32)
        END IF
    ENDDO

    END SUBROUTINE
    !------------------------------------------------------------------------
    !
    ! UPPER 小写转换成大写
    !
    !------------------------------------------------------------------------
    SUBROUTINE UPPER(STRING)
    !
    IMPLICIT NONE
    CHARACTER(LEN=*) , INTENT(INOUT) :: STRING
    INTEGER(KIND=4) :: I
    INTEGER(KIND=4) :: LENGTH
    LENGTH = LEN(STRING)
    DO I = 1 , LENGTH
        IF(LGE(STRING(I:I),'a') .AND. LLE(STRING(I:I),'z') )THEN
            STRING(I:I) = ACHAR(IACHAR(STRING(I:I))-32)
        END IF
    END DO
    END SUBROUTINE
    !------------------------------------------------------------------------
    ! PARSE FIELDS
    !------------------------------------------------------------------------
    SUBROUTINE PARSEFIELDS(SLINE, NFIELDS, SFIELDS)
    !
    ! ROUTINE TO PARSE OUT MIXED NUMERIC AND CHARACTER FIELDS FROM A LINE
    ! THIS IS USEFUL FOR MIXED FORMAT INPUT TABLES, WHICH ARE AWKWARD FOR FORTRAN
    !
    ! KERRY KEY
    ! SCRIPPS INSTITUTION OF OCEANOGRAPHY
    ! KKEY@UCSD.EDU
    !
    ! VERSION 1.0.   NOVEMBER 21, 2008.
    !
    IMPLICIT NONE

    ! ARGS

    CHARACTER(LEN=*)        :: SLINE
    INTEGER(KIND=4), INTENT(IN)    :: NFIELDS
    CHARACTER(LEN=*) , INTENT(OUT) :: SFIELDS(NFIELDS)


    ! LOCAL VARS
    INTEGER(KIND=4) :: IFROM, ITO, I, NLEN

    NLEN = LEN_TRIM(SLINE)

    ! CONVERT ALL TAB CHARACTERS TO SPACES
    FORALL( ITO = 1:NLEN, ICHAR(SLINE(ITO:ITO)) == 9 ) SLINE(ITO:ITO) = ' '

    IFROM = 1

    ! LOOP THROUGH THE LINE AND GET THE NFIELDS:
    DO I=1,NFIELDS

        ! SKIP ANY BEGINNING BLANKS
        DO IFROM = IFROM,NLEN
            IF (SLINE(IFROM:IFROM) .NE. ' ') EXIT
        ENDDO

        ! PULL OUT NONBLANK CHARACTER STRING:
        DO ITO = IFROM,NLEN
            IF (SLINE(ITO:ITO) .EQ. ' ') EXIT
        ENDDO
        SFIELDS(I) = TRIM(SLINE(IFROM:ITO-1))
        IFROM = ITO
    ENDDO

    END SUBROUTINE
    !------------------------------------------------------------------------
    !
    ! FILE PARTS
    !
    !------------------------------------------------------------------------
    SUBROUTINE FILEPARTS(SSTR, SROOT, SEXT1, SEXT2)
    !
    ! LIKE THE MATLAB FUNCTION FILEPARTS.
    !
    IMPLICIT NONE
    CHARACTER(LEN=*) , INTENT(IN)  :: SSTR
    CHARACTER(LEN=*) , INTENT(OUT) :: SROOT
    CHARACTER(LEN=*) , INTENT(OUT) , OPTIONAL :: SEXT1 , SEXT2

    INTEGER :: IDOT

    SROOT = ADJUSTL(SSTR)

    IDOT = INDEX(SROOT,'.',.TRUE.)

    SEXT1 = ' '
    SEXT2 = ' '

    IF (IDOT > 0) THEN

        SEXT1 = SROOT(IDOT+1:)
        SEXT1 = ADJUSTL(SEXT1)
        SROOT = SROOT(1:IDOT-1)
        IDOT = INDEX(SROOT,'.',.TRUE.)
        IF (IDOT == 0) THEN
            SEXT2 = ''
        ELSE
            SEXT2 = SROOT(IDOT+1:)
            SEXT2 = ADJUSTL(SEXT2)
            SROOT = SROOT(1:IDOT-1)
        END IF


    END IF

    END  SUBROUTINE
    !------------------------------------------------------------------------
    !
    ! PARSE LINE
    !
    !------------------------------------------------------------------------
    SUBROUTINE PARSELINE(NLEN, SLINE, BCOMMENT)
    !
    ! SUBROUTINE TO CHECK IF THE SLINE IS BLANK OR A COMMENT LINE, AND IF IT ISN'T
    ! THEN ANY COMMENT AT THE END OF THE LINE IS BLANKED.
    !
    ! THIS IS STYLED AFTER D. MYER'S PARSECODE FUNCTION.
    !
    ! THIS IS USEFUL FOR READING IN DATA TABLES THAT HAVE USER COMMENT LINES
    ! OR COMMENTS AT THE END OF THE LINE, DENOTED BY THE ! AND % SYMBOLS.
    !
    ! IF THE ENTIRE LINE IS A COMMENT, BCOMMENT = .TRUE.
    !
    ! KERRY KEY
    ! SCRIPPS INSTITUTION OF OCEANOGRAPHY
    ! KKEY@UCSD.EDU
    !
    ! VERSION 1.0.   APRIL, 2008.
    !
    ! ARGS
    INTEGER(KIND=4) , INTENT(IN)     :: NLEN
    CHARACTER(NLEN)         :: SLINE
    LOGICAL(KIND=4) , INTENT(OUT)    :: BCOMMENT
    ! LOCAL VARS
    INTEGER(KIND=4) :: IFROM, ITO
    ! INIT RETURNS
    BCOMMENT = .FALSE.

    ! CONVERT ALL TAB CHARACTERS TO SPACES
    FORALL( ITO = 1:NLEN, ICHAR(SLINE(ITO:ITO)) == 9 ) SLINE(ITO:ITO) = ' '

    ! SKIP ANY BEGINNING BLANKS
    DO IFROM = 1,NLEN
        IF (SLINE(IFROM:IFROM) .NE. ' ') EXIT
    ENDDO

    ! IF THE FIRST CHAR IS A COMMENT CHAR, THEN THE WHOLE LINE IS A COMMENT.
    ! DGM APRIL 2008 ALSO, IF THE LINE IS BLANK, CONSIDER IT A COMMENT.
    IF( IFROM > NLEN .OR. SLINE(IFROM:IFROM) == '%' &
        .OR. SLINE(IFROM:IFROM) == '!' ) THEN
    BCOMMENT = .TRUE.
    RETURN
    END IF

    ! NOW TRIM OFF ANY COMMENTS AT THE END OF THE LINE
    ITO = LEN_TRIM(SLINE)
    IFROM = INDEX(SLINE,'%')
    IF (IFROM > 0 .AND. IFROM < ITO) THEN
        SLINE(IFROM:ITO) = ' '
    END IF
    IFROM = INDEX(SLINE,'!')
    IF (IFROM > 0 .AND. IFROM < ITO) THEN
        SLINE(IFROM:ITO) = ' '
    END IF

    END SUBROUTINE
    !------------------------------------------------------------------------
    !
    ! CONVERT STRING TO INTEGER
    !
    !------------------------------------------------------------------------
    SUBROUTINE CONVERTSTRTOINTEGER(STRING , IVAL , LOKAY)
    IMPLICIT NONE
    CHARACTER(LEN=*)            :: STRING
    CHARACTER(LEN=LEN(STRING))  :: FIRST
    CHARACTER(LEN=20)           :: FORM
    INTEGER(KIND=4)             :: IERR
    INTEGER(KIND=4)             :: IVAL
    LOGICAL(KIND=4)             :: LOKAY

    LOKAY = .FALSE.
    READ(STRING, *, IOSTAT = IERR ) FIRST

    IF ( IERR == 0 ) THEN
        WRITE( FORM, '(A,I0,A)' ) '(I', LEN(STRING), ')'
        READ( FIRST, FORM, IOSTAT = IERR ) IVAL
    END IF

    LOKAY = IERR == 0

    END SUBROUTINE
    !------------------------------------------------------------------------
    !
    !
    !
    !------------------------------------------------------------------------
    END MODULE
