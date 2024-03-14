    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    MODULE SplineMod
    !------------------------------------------------------------------------
    !
    !
    !
    !------------------------------------------------------------------------
    IMPLICIT NONE
    !------------------------------------------------------------------------
    !
    !
    !
    !------------------------------------------------------------------------
    CONTAINS
    !------------------------------------------------------------------------
    !
    ! EVALUATES THE FIRST DERIVATIVE OF A FUNCTION FROM ITS CUBIC SPLINE
    !
    !------------------------------------------------------------------------
    REAL(KIND=8) FUNCTION CUBDER (KNOT , XKNOT, COEF, X1)
    !===== ===== ===== ===== ===== =====
    ! PURPOSE:
    ! EVALUATES THE FIRST DERIVATIVE OF A FUNCTION FROM ITS CUBIC SPLINE
    ! INTERPOLATION.
    !===== ===== ===== ===== ===== =====
    ! CALLED BY: FOLD_AND_CONVOLVE
    ! CALLS: INTERV.  ON EXIT FROM INTERV
    !===== ===== ===== ===== ===== =====
    IMPLICIT NONE
    ! INPUT PARAMETER:
    !===== ===== ===== ===== ===== =====
    ! KNOT - TOTAL NUMBER OF KNOTS INCLUDING ENDPOINTS.
    INTEGER(KIND=4) ,INTENT(IN) :: KNOT
    ! XKNOT(I), I = 1,KNOT - LOCATION OF THE KNOTS.  THE RIGHTMOST DATA
    !                        POINT USED TO CALCULATE COEFFICIENTS IS NOT
    !                        USED.
    REAL(KIND=8) , DIMENSION(KNOT) , INTENT(IN):: XKNOT
    ! COEF(J,I), J = 1,4; I = 1,KNOT = JTH DERIVATIVE AT H = 0
    !                                  WHERE  H = X - XKNOT(I)
    REAL(KIND=8) , DIMENSION(4,KNOT) , INTENT(IN) :: COEF
    REAL(KIND=8) , INTENT(IN) :: X1
    !===== ===== ===== ===== ===== =====
    INTEGER(KIND=4) :: I,MFLAG
    REAL(KIND=8) :: H
    !===== ===== ===== ===== ===== =====
    ! FIND INDEX I OF LARGEST BREAKPOINT TO THE LEFT OF X1.

    CALL INTERV ( XKNOT, KNOT-1, X1, I, MFLAG )
    H = X1 - XKNOT(I)
    IF (MFLAG == -1) H = 0.0D0

    CUBDER = (COEF(4,I)*H/2.0D0 + COEF(3,I) ) *H + COEF(2,I)
    !===== ===== ===== ===== ===== =====
    END FUNCTION CUBDER
    !------------------------------------------------------------------------
    !
    ! INTEGRATES A FUNCTION FROM X1 TO X2 USING ITS CUBIC SPLINE REPRESENTATION
    !
    !------------------------------------------------------------------------
    REAL(KIND=8) FUNCTION CUBINT (KNOT, XKNOT, COEF,  X1, X2)
    !===== ===== ===== ===== ===== =====
    ! PURPOSE:
    ! INTEGRATES A FUNCTION FROM X1 TO X2 USING ITS CUBIC SPLINE REPRESENTATION.
    !===== ===== ===== ===== ===== =====
    ! CALLED BY:  EGT_BOSS, TXCNVD, TXCNVL
    ! CALLS: INTERV.  ON EXIT FROM INTERV
    !===== ===== ===== ===== ===== =====
    ! THEORY:
    ! THE COEFFICIENTS OF THE CUBIC SPLINE REPRESENT THE
    ! INDEFINITE INTEGRAL OF F, ON THE I'TH INTERVAL, AS:
    !
    ! INTGR [ F(X) ] = COEF(4,I)/24 * H**4  +  COEF(3,I)/6 * H**3  +
    !                  COEF(2,I)/2 * H**2  +  COEF(1,I) * H
    !
    !                  WITH  H = X - XKNOT(K)
    !
    ! THIS IS A MODIFICATION OF THE FUNCTION PPVALU IN THE BOOK
    ! "A PRACTICAL GUIDE TO SPLINES"  BY C. DE BOOR
    !===== ===== ===== ===== ===== =====
    IMPLICIT NONE
    ! INPUT PARAMETER:
    !===== ===== ===== ===== ===== =====
    ! KNOT - TOTAL NUMBER OF KNOTS INCLUDING ENDPOINTS.
    INTEGER(KIND=4) , INTENT(IN) :: KNOT
    ! XKNOT(I), I = 1,KNOT - LOCATION OF THE KNOTS.  THE RIGHTMOST DATA
    !                        POINT USED TO CALCULATE COEFFICIENTS IS NOT
    !                        INCLUDED.
    REAL(KIND=8) , DIMENSION(KNOT) , INTENT(IN) :: XKNOT
    ! COEF(J,I), J = 1,4; I = 1,KNOT
    REAL(KIND=8) , DIMENSION(4,KNOT) , INTENT(IN) :: COEF
    REAL(KIND=8) ,INTENT(IN) :: X1,X2
    !===== ===== ===== ===== ===== =====
    INTEGER(KIND=4) :: I,I1,I2,MFLAG
    REAL(KIND=8) :: H,H1,H2
    !===== ===== ===== ===== ===== =====
    ! FIND THE INDICES I1 AND I2 OF LARGEST BREAKPOINTS TO THE LEFT OF X1
    ! AND X2 RESPECTIVELY.
    !
    CALL INTERV ( XKNOT, KNOT-1, X1, I1, MFLAG )
    CALL INTERV ( XKNOT, KNOT-1, X2, I2, MFLAG )
    H1 = X1 - XKNOT(I1)
    IF (MFLAG == -1) H1 = 0.0D0

    H2 = X2 - XKNOT(I2)
    CUBINT = (((COEF(4,I2)*H2/4.0D0 + COEF(3,I2) )*H2/3.0D0 + &
        COEF(2,I2) )*H2/2.0D0 + COEF(1,I2) )*H2 &
        - (((COEF(4,I1)*H1/4.0D0 + COEF(3,I1) )*H1/3.0D0 + &
        COEF(2,I1) )*H1/2.0D0 + COEF(1,I1) )*H1

    ! INCLUDE INTEGRALS OVER INTERVENING INTERVALS.

    IF (I2 > I1) THEN
        DO I = I1, I2-1
            H = XKNOT(I+1) - XKNOT(I)
            CUBINT = CUBINT + (((COEF(4,I)*H/4.0D0 + COEF(3,I) )*H/3.0D0 + &
                COEF(2,I) )*H/2.0D0 + COEF(1,I) )*H
        END DO
    END IF
    !===== ===== ===== ===== ===== =====
    END FUNCTION CUBINT
    !------------------------------------------------------------------------
    !
    ! CALCULATES COEFFICIENTS FOR CUBIC SPLINE INTERPOLATION
    !
    !------------------------------------------------------------------------
    SUBROUTINE CUBSPL (XNOT, C, N, IBCBEG, IBCEND)
    !===== ===== ===== ===== ===== =====
    ! PURPOSE:
    ! CALCULATES COEFFICIENTS FOR CUBIC SPLINE INTERPOLATION.
    ! CALL FUNCTION CUBVAL TO EVALUATE FUNCTION VALUES AFTER INTERPOLATION.
    !===== ===== ===== ===== ===== =====
    ! CALLED BY: EGT_CSPL, FOLD_AND_CONVOLVE, HSBOSS_TD, INTER_EGT_CSPL, MGTBS,
    !            PRM_BOSS, TDEM_3D, TQSTRIP, TXCNVD,
    !
    !===== ===== ===== ===== ===== =====
    ! THEORY:
    ! IN THE INTERVAL: (XNOT(I) - XNOT(I+1)), THE SPLINE F IS GIVEN BY:
    !
    ! F(X) = C(1,I) + H* (C(2,I) + H* (C(3,I) + H* C(4,I)/3.0) /2.0)
    !
    ! WHERE H = X - XNOT(I).  FUNCTION  *CUBVAL* MAY BE
    ! USED TO EVALUATE F OR ITS DERIVATIVES FROM XNOT,C, L = N-1,
    ! AND K=4.
    !===== ===== ===== ===== ===== =====
    IMPLICIT NONE
    ! INPUT PARAMETER:
    !===== ===== ===== ===== ===== =====
    ! N = NUMBER OF DATA POINTS. ASSUMED TO BE > 1.
    INTEGER(KIND=4) , INTENT(IN) :: N
    ! (XNOT(I), C(1,I), I=1,...,N) = ABSCISSAE AND ORDINATES OF THE DATA POINTS.
    !                                 XNOT IS ASSUMED TO BE STRICTLY INCREASING.
    REAL(KIND=8) , DIMENSION(N) , INTENT(IN) :: XNOT
    ! C(J,I), J=1,...,4; I=1,...,L (= N-1) = THE POLYNOMIAL COEFFICIENTS
    !         OF THE CUBIC INTERPOLATING SPLINE WITH INTERIOR KNOTS (OR JOINTS)
    !         XNOT(2), ..., XNOT(N-1).
    REAL(KIND=8) , DIMENSION(4,N) , INTENT(INOUT) :: C
    ! IBCBEG, IBCEND = BOUNDARY CONDITION INDICATORS, AND
    ! C(2,1), C(2,N) = BOUNDARY CONDITION INFORMATION. SPECIFICALLY,
    !
    ! IBCBEG = 0  NO BOUNDARY CONDITION AT XNOT(1) IS GIVEN.  IN THIS CASE,
    !             THE NOT-A-KNOT CONDITION IS USED, I.E. THE JUMP IN THE
    !             THIRD DERIVATIVE ACROSS XNOT(2) IS FORCED TO ZERO.  THUS
    !             FIRST AND THE SECOND CUBIC POLYNOMIAL PIECES ARE MADE TO
    !             COINCIDE.
    ! IBCBEG = 1  THE SLOPE AT XNOT(1) IS MADE TO EQUAL C(2,1),
    !             SUPPLIED BY INPUT.
    ! IBCBEG = 2  THE SECOND DERIVATIVE AT XNOT(1) IS MADE TO EQUAL C(2,1),
    !             SUPPLIED BY INPUT.
    !
    ! IBCEND = 0, 1, OR 2 HAS ANALOGOUS MEANING CONCERNING THE BOUNDARY
    !             CONDITION AT XNOT(N), WITH THE ADDITIONAL INFORMATION
    !             TAKEN FROM C(2,N).
    INTEGER(KIND=4) , INTENT(IN) :: IBCBEG,IBCEND
    !===== ===== ===== ===== ===== =====
    ! INTERNAL PARAMETER:
    !===== ===== ===== ===== ===== =====
    INTEGER(KIND=4) :: I,J,L,M
    REAL(KIND=8) :: DIVDF1,DIVDF3,DXNOT,G
    !SAVE THIS IS A BUG?
    !===== ===== ===== ===== ===== =====
    ! A TRIDIAGONAL LINEAR SYSTEM FOR THE UNKNOWN SLOPES S(I) OF F AT
    ! XNOT(I), I=1,...,N, IS GENERATED AND THEN SOLVED BY GAUSS ELIMINATION,
    ! WITH S(I) ENDING UP IN C(2,I), ALL I.
    ! C(3,.) AND C(4,.) ARE USED INITIALLY FOR TEMPORARY STORAGE.

    ! COMPUTE FIRST DIFFERENCES OF XNOT SEQUENCE AND STORE IN C(3,.).
    ! ALSO, COMPUTE FIRST DIVIDED DIFFERENCE OF DATA AND STORE IN C(4,.).
    !===== ===== ===== ===== ===== =====
    L = N - 1
    DO M = 2,N
        C(3,M) = XNOT(M) - XNOT(M-1)
        C(4,M) = (C(1,M) - C(1,M-1)) /C(3,M)
    END DO

    ! CONSTRUCT FIRST EQUATION FROM THE BOUNDARY CONDITION, OF THE FORM
    ! C(4,1)*S(1) + C(3,1)*S(2) = C(2,1)

    IF (IBCBEG < 1) THEN
        IF (N > 2) THEN

            ! NOT-A-KNOT CONDITION AT LEFT END AND N > 2.

            C(4,1) = C(3,3)
            C(3,1) = C(3,2) + C(3,3)
            C(2,1) = ((C(3,2) + 2.0* C(3,1)) * C(4,2)*C(3,3) &
                + C(3,2)**2 * C(4,3)) /C(3,1)
            GOTO 100
        ELSE

            ! NO CONDITION AT LEFT END AND N = 2.

            C(4,1) = 1.0
            C(3,1) = 1.0
            C(2,1) = 2.0 * C(4,2)
            GOTO 300
        END IF
    ELSE IF (IBCBEG == 1) THEN

        ! SLOPE PRESCRIBED AT LEFT END.

        C(4,1) = 1.0
        C(3,1) = 0.0
    ELSE

        ! SECOND DERIVATIVE PRESCRIBED AT LEFT END.

        C(4,1) = 2.0
        C(3,1) = 1.0
        C(2,1) = 3.0* C(4,2) - C(3,2) * C(2,1) /2.0
    END IF
    IF (N == 2) GOTO 300

    ! IF THERE ARE INTERIOR KNOTS, GENERATE THE CORRESPONDING EQUATIONS AND
    ! PERFORM THE FORWARD PASS OF GAUSS ELIMINATION, AFTER WHICH THE M-TH
    ! EQUATION READS    C(4,M)*S(M) + C(3,M)*S(M+1) = C(2,M).

100 DO M = 2,L
        G = -C(3,M+1) / C(4,M-1)
        C(2,M) = G*C(2,M-1) &
            + 3.0* (C(3,M)*C(4,M+1) + C(3,M+1)*C(4,M))
        C(4,M) = G* C(3,M-1) + 2.0* (C(3,M) + C(3,M+1))
    END DO

    ! CONSTRUCT LAST EQUATION FROM THE SECOND BOUNDARY CONDITION, OF THE FORM
    ! (-G*C(4,N-1))*S(N-1) + C(4,N)*S(N) = C(2,N)
    ! IF SLOPE IS PRESCRIBED AT RIGHT END, ONE CAN GO DIRECTLY TO BACK-
    ! SUBSTITUTION, SINCE C ARRAY HAPPENS TO BE SET UP JUST RIGHT FOR IT
    ! AT THIS POINT.

    IF (IBCEND < 1) THEN
        IF ( N /=3 .OR. IBCBEG /=0 ) THEN

            ! NOT-A-KNOT AND N > 2, AND EITHER N > 3 OR ALSO NOT-A-KNOT AT
            ! LEFT END POINT.

            G = C(3,N-1) + C(3,N)
            C(2,N) = ((C(3,N) + 2.0*G) *C(4,N)*C(3,N-1) + C(3,N)**2 &
                *(C(1,N-1) - C(1,N-2)) /C(3,N-1))/G
            G = -G / C(4,N-1)
            C(4,N) = C(3,N-1)
            GOTO 350
        END IF
    ELSE IF (IBCEND == 1) THEN
        GOTO 400
    ELSE
        GOTO 250
    END IF

    ! EITHER (N=3 AND NOT-A-KNOT ALSO AT LEFT) OR (N=2 AND NOT NOT-A-
    ! KNOT AT LEFT END POINT).

200 C(2,N) = 2.0 * C(4,N)
    C(4,N) = 1.0
    G = -1.0 / C(4,N-1)
    GOTO 350

    ! SECOND DERIVATIVE PRESCRIBED AT RIGHT ENDPOINT.

250 C(2,N) = 3.0*C(4,N) + C(3,N)*C(2,N)/2.0
    C(4,N) = 2.0
    G = -1.0 / C(4,N-1)
    GOTO 350
300 IF (IBCEND < 1) THEN
        IF (IBCBEG > 0) GOTO 200

        ! NOT-A-KNOT AT RIGHT ENDPOINT AND AT LEFT ENDPOINT AND N = 2.

        C(2,N) = C(4,N)
        GOTO 400
    ELSE IF (IBCEND == 1) THEN
        GOTO 400
    ELSE
        GOTO 250
    END IF

    ! COMPLETE FORWARD PASS OF GAUSS ELIMINATION.

350 C(4,N) = G*C(3,N-1) + C(4,N)
    C(2,N) = (G*C(2,N-1) + C(2,N)) /C(4,N)

    ! PERFORM BACK SUBSTITUTION.

400 J = L
450 C(2,J) = (C(2,J) - C(3,J) *C(2,J+1)) /C(4,J)
    J = J - 1
    IF (J > 0) GOTO 450

    ! GENERATE CUBIC COEFFICIENTS IN EACH INTERVAL, I.E., THE DERIVATIVES AT ITS
    ! LEFT ENDPOINT, FROM VALUE AND SLOPE AT ITS ENDPOINTS.

    DO I = 2,N
        DXNOT = C(3,I)
        DIVDF1 = (C(1,I) - C(1,I-1)) /DXNOT
        DIVDF3 = C(2,I - 1) + C(2,I) - 2.0*DIVDF1
        C(3,I-1) = 2.0* (DIVDF1 - C(2,I-1) - DIVDF3) /DXNOT
        C(4,I-1) = (DIVDF3/DXNOT) * (6.0/DXNOT)
    END DO
    !===== ===== ===== ===== ===== =====
    END SUBROUTINE CUBSPL
    !------------------------------------------------------------------------
    !
    ! EVALUATES A FUNCTION AT X1 FROM FROM ITS CUBIC SPLINE REPRESENTATION
    !
    !------------------------------------------------------------------------
    REAL(KIND=8) FUNCTION CUBVAL (KNOT, XKNOT, COEF,  X1)
    !===== ===== ===== ===== ===== =====
    ! PURPOSE:
    ! EVALUATES A FUNCTION AT X1 FROM FROM ITS CUBIC SPLINE REPRESENTATION.
    !===== ===== ===== ===== ===== =====
    ! CALLED BY: COSTRN, EGT_BOSS, FOLD_AND_CONVOLVE, INTER_EGT_BOSS,
    !                MGTV1, PRM_BOSS, TXCNVD, TXCNVL
    ! CALLS: INTERV.
    !===== ===== ===== ===== ===== =====
    ! THEORY:
    ! THE COEFFICIENTS OF THE CUBIC SPLINE ON THE I'TH INTERVAL REPRESENT F AS:
    !
    !                F(X) = COEF(4,I)/6 * H**3  +  COEF(3,I)/2 * H**2  +
    !                       COEF(2,I) * H  +  COEF(1,I)
    !
    !                          WITH  H = X - XKNOT(I)
    !
    ! THIS IS A MODIFICATION OF THE FUNCTION PPVALU IN THE BOOK
    ! "A PRACTICAL GUIDE TO SPLINES"  BY C. DE BOOR.
    ! THE INTERVAL INDEX I, APPROPRIATE FOR X, IS FOUND THROUGH A CALL TO INTERV.
    ! THE FORMULA FOR F IS EVALUATED USING NESTED MULTIPLICATION.
    !===== ===== ===== ===== ===== =====
    IMPLICIT NONE
    ! INPUT PARAMETER:
    !===== ===== ===== ===== ===== =====
    ! KNOT - TOTAL NUMBER OF KNOTS INCLUDING ENDPOINTS.
    INTEGER(KIND=4) , INTENT(IN) :: KNOT
    ! XKNOT(I), I = 1,KNOT - LOCATION OF THE KNOTS.  THE RIGHTMOST DATA
    !                        POINT USED TO CALCULATE COEFFICIENTS IS NOT
    !                        INCLUDED.
    REAL(KIND=8) , DIMENSION(KNOT) , INTENT(IN) :: XKNOT
    ! COEF(J,I), J = 1,4; I = 1,KNOT
    REAL(KIND=8) , DIMENSION(4,KNOT) , INTENT(IN) :: COEF
    REAL(KIND=8) , INTENT(IN) :: X1
    !===== ===== ===== ===== ===== =====
    ! INTERNAL PARAMETER:
    !===== ===== ===== ===== ===== =====
    REAL(KIND=8) :: H
    INTEGER(KIND=4) :: I,MFLAG
    !===== ===== ===== ===== ===== =====
    !
    ! FIND INDEX I OF LARGEST BREAKPOINT TO THE LEFT OF X1.
    !
    CALL INTERV ( XKNOT, KNOT-1, X1, I, MFLAG )
    H = X1 - XKNOT(I)
    IF (MFLAG == -1) H = 0.0D0

    CUBVAL = ((COEF(4,I)*H/3.0D0 + COEF(3,I) )*0.5D0*H + COEF(2,I) )*H + COEF(1,I)
    !===== ===== ===== ===== ===== =====
    END FUNCTION CUBVAL
    !------------------------------------------------------------------------
    !
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE INTERV (XT, LXT, X, LEFT, MFLAG)
    !===== ===== ===== ===== ===== =====
    ! PURPOSE:
    ! COMPUTES  LEFT = MAX( I , 1 <= I <= LXT  .AND.  XT(I) <= X )  .
    !===== ===== ===== ===== ===== =====
    ! CALLED BY: CUBVAL, CUBINT, CUBDER, LINVAL
    !===== ===== ===== ===== ===== =====
    ! RESTRUCTURED APRIL, 1997
    ! FROM  * A PRACTICAL GUIDE TO SPLINES *  BY C. DE BOOR
    !===== ===== ===== ===== ===== =====
    !
    ! INPUT:
    !
    ! XT  - A REAL(KIND=8) :: SEQUENCE, OF LENGTH  LXT, ASSUMED TO BE NON-DECREASING.
    ! LXT - NUMBER OF TERMS IN THE SEQUENCE  XT .
    ! X   - THE POINT WHOSE LOCATION WITH RESPECT TO THE SEQUENCE XT IS
    !       TO BE DETERMINED.
    !
    ! OUTPUT:
    !
    ! LEFT, MFLAG.....ARE BOTH INTEGER(KIND=4) ::S, WHOSE VALUE IS:
    !
    !        1     -1      IF        X <  XT(1)
    !        I      0      IF   XT(I)  <= X < XT(I+1)
    !       LXT     1      IF  XT(LXT) <= X
    !
    ! IN PARTICULAR, MFLAG = 0 IS THE 'USUAL' CASE.  MFLAG /= 0
    ! INDICATES THAT X  LIES OUTSIDE THE HALFOPEN INTERVAL
    ! XT(1) <= Y < XT(LXT) . THE ASYMMETRIC TREATMENT OF THE
    ! INTERVAL IS DUE TO THE DECISION TO MAKE ALL PP FUNCTIONS
    ! CONTINUOUS FROM THE RIGHT.
    !
    ! METHOD:
    !
    ! THE PROGRAM IS DESIGNED TO BE EFFICIENT IN THE COMMON SITUATION THAT
    ! IT IS CALLED REPEATEDLY, WITH  X  TAKEN FROM AN INCREASING OR DECREASING
    ! SEQUENCE. THIS WILL HAPPEN, E.G., WHEN A PP FUNCTION IS TO BE GRAPGED.
    ! THE FIRST GUESS FOR  LEFT  IS THEREFORE TAKEN TO BE THE VALUE RETURNED AT
    ! THE PREVIOUS CALL AND STORED IN THE LOCAL VARIABLE ILO. A FIRST
    ! CHECK ASCERTAINS THAT  ILO < LXT (THIS IS NECESSARY SINCE THE PRESENT
    ! CALL MAY HAVE NOTHING TO DO WITH THE PREVIOUS CALL).
    ! THEN, IF XT(ILO) <= XT(ILO+1),
    ! WE SET  LEFT = ILO  AND ARE DONE AFTER JUST THREE COMPARISONS.
    ! OTHERWISE, WE REPEATEDLY DOUBLE THE DIFFERENCE  ISTEP = IHI - ILO
    ! WHILE ALSO MOVING  ILO  AND  IHI  IN THE DIRECTION OF  X , UNTIL
    ! XT(ILO) <= X < XT(IHI) ,
    ! AFTER WHICH WE USE BISECTION TO GET, IN ADDITION, ILO+1 = IHI .
    ! LEFT = ILO  IS THEN RETURNED.
    !===== ===== ===== ===== ===== =====
    IMPLICIT NONE
    ! INPUT PARAMETER:
    !===== ===== ===== ===== ===== =====
    INTEGER(KIND=4) , INTENT(IN) :: LXT
    REAL(KIND=8) , DIMENSION(LXT) , INTENT(IN) :: XT
    REAL(KIND=8) , INTENT(IN) :: X
    !===== ===== ===== ===== ===== =====
    ! OUTPUT PARAMETER:
    INTEGER(KIND=4) , INTENT(OUT) :: LEFT,MFLAG
    !===== ===== ===== ===== ===== =====
    ! INTERNAL PARAMETER:
    INTEGER(KIND=4) :: IHI,ILO,ISTEP,MIDDLE,J1
    !SAVE ILO
    !DATA ILO /1/ THIS IS A BUG.
    ILO=1
    !===== ===== ===== ===== ===== =====
    ! TRIVIAL RETURNS WHEN X IS NOT IN THE RANGE.

    IF ( (X <= XT(1)) .OR. (LXT <= 1) ) THEN
        MFLAG = -1
        LEFT = 1
        RETURN
    END IF

    IF (X >= XT(LXT)) THEN
        MFLAG = 1
        LEFT = LXT
        RETURN
    END IF

    MFLAG = 0
    IF (ILO >= LXT) ILO = LXT-1
    IHI = ILO + 1

    ! TRIVIAL RETURN WHEN X IS ALREADY IN THE INTERVAL.

    IF ( (X <= XT(IHI)) .AND. (X >= XT(ILO)) ) THEN
        LEFT = ILO
        RETURN
    END IF
    !===== ===== ===== ===== ===== =====
    ! DECREASE ILO  TO CAPTURE X.
    IF (X <= XT(ILO)) THEN
        ISTEP = 1
        DO J1 = 1,LXT
            IHI = ILO
            ILO = IHI - ISTEP
            ILO = MAX(1, ILO)
            IF ( (X >= XT(ILO)) .OR. (ILO == 1) ) EXIT
            ISTEP = ISTEP*2
        END DO
        ! INCREASE IHI TO CAPTURE X
    ELSE IF ( X >= XT(IHI)) THEN

        ISTEP = 1
        DO J1 = 1,LXT
            ILO = IHI
            IHI = ILO + ISTEP
            IHI = MIN (IHI,LXT)
            IF ( (X <= XT(IHI)) .OR. (IHI == LXT) ) EXIT
            ISTEP = ISTEP*2
        END DO

    END IF

    ! NOW XT(ILO) <= X < XT(IHI) . NARROW THE INTERVAL.

    DO J1 = 1,LXT
        MIDDLE = (ILO + IHI)/2
        IF (MIDDLE == ILO) EXIT
        IF (X < XT(MIDDLE)) THEN
            IHI = MIDDLE
        ELSE
            ILO = MIDDLE
        END IF
    END DO

    ! TASK COMPLETE

    LEFT = ILO
    RETURN

    END SUBROUTINE INTERV
    !------------------------------------------------------------------------
    !
    !
    !
    !------------------------------------------------------------------------
    real(kind=8) function y_spl(x , y , n , x_spl)
    implicit none
    integer :: i
    !框架数组长度
    integer :: n
    !框架x和y
    real(kind=8) :: x(n) , y(n)
    !待插值的x和插值结果
    real(kind=8) :: x_spl
    real(kind=8) :: x1,y1,x2,y2

    !----------------------------
    if(x_spl < x(1))then             !如果待插值点在框架之外
        y_spl = y(1)
        return
    else if(x_spl > x(n))then
        y_spl = y(n)
        return
    end if
    !----------------------------
    do i = 1 , n
        if(x_spl < x(i))then
            x1 = x(i-1)
            y1 = y(i-1)
            x2 = x(i)
            y2 = y(i)
            y_spl = ((x_spl-x1)*y2+(x2-x_spl)*y1)/(x2-x1)
            exit
        end if
    end do
    end function
    !------------------------------------------------------------------------
    !
    !
    !
    !------------------------------------------------------------------------
    END MODULE