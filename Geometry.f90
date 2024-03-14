    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  Geometry module
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    MODULE Geometry
    USE SortMod , ONLY : qsort
    use PointMod , ONLY : Point_t
    use PointMod , ONLY : DotProduct
    use PointMod , ONLY : CrossProduct
    use PointMod , ONLY : MultValue
    use PointMod , ONLY : MinusVector
    use PointMod , ONLY : PlusVector
    use PointMod , ONLY : DataVector
    use PointMod , ONLY : Distance
    use PointMod , ONLY : Same
    !-------------------------------
    !
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    !-------------------------------
    CONTAINS
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE judge_point_segment_a(pa , pb , po , flag)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb
    !
    TYPE(Point_t) , INTENT(IN) :: po
    !
    INTEGER(KIND=4) , INTENT(OUT) :: flag
    !-------------------------------
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    IF((po%x-pa%x > Residual .and. po%x-pb%x > Residual) .or. (pa%x-po%x > Residual .and. pb%x-po%x > Residual))THEN
        flag = 0
        RETURN
    ELSE IF((po%y-pa%y > Residual .and. po%y-pb%y > Residual) .or. (pa%y-po%y > Residual .and. pb%y-po%y > Residual))THEN
        flag = 0
        RETURN
    ELSE IF((po%z-pa%z > Residual .and. po%z-pb%z > Residual) .or. (pa%z-po%z > Residual .and. pb%z-po%z > Residual))THEN
        flag = 0
        RETURN
    ELSE IF(ABS(po%x-pa%x) < Residual .and. ABS(po%y-pa%y) < Residual .and. ABS(po%z-pa%z) < Residual)THEN
        
        flag = 2
    ELSE IF(ABS(po%x-pb%x) < Residual .and. ABS(po%y-pb%y) < Residual .and. ABS(po%z-pb%z) < Residual)THEN
        
        flag = 2
    ELSE
        flag = 1
    END IF
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE judge_point_segment_b(pa , pb , po , flag)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------

    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb
    !
    TYPE(Point_t) , INTENT(IN) :: po
    !-------------------------------
    !
    INTEGER(KIND=4) , INTENT(OUT) :: flag
    !-------------------------------
    !
    REAL(KIND=8) :: dab
    !
    REAL(KIND=8) :: dao
    !
    REAL(KIND=8) :: dob
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    dab = distance(pa,pb)
    dao = distance(pa,po)
    dob = distance(po,pb)
    IF((ABS((dao+dob)-dab)) <= Residual)THEN
        IF(dao < Residual)THEN
            !
            flag = 2
        ELSE IF(dob < Residual)THEN
            !
            flag = 2
        ELSE
            flag = 1
            !
        END IF
    ELSE
        !
        flag = 0
    END IF
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE judge_point_triangle(pa , pb , pc , po , flag)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb
    TYPE(Point_t) , INTENT(IN) :: pc
    !
    TYPE(Point_t) , INTENT(IN) :: po
    !-------------------------------
    INTEGER(KIND=4) , INTENT(OUT) :: flag
    !-------------------------------
    TYPE(Point_t) :: v0,v1,v2
    REAL(KIND=8) :: u,v
    !
    REAL(KIND=8) :: v00,v01,v02,v11,v12
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------

    v0 = MinusVector(pc,pa)
    v1 = MinusVector(pb,pa)
    v2 = MinusVector(po,pa)
    v00 = DotProduct(v0,v0)
    v01 = DotProduct(v0,v1)
    v02 = DotProduct(v0,v2)
    v11 = DotProduct(v1,v1)
    v12 = DotProduct(v1,v2)
    IF(ABS(v00*v11-v01*v01) < Residual)THEN
        !
        flag = 0
        RETURN
    ELSE
        u = (v11*v02-v01*v12)/(v00*v11-v01*v01)
        IF(u < -Residual .or. u > 1.0d0+Residual)THEN
            flag = 0
            RETURN
        ELSE
            v = (v00*v12-v01*v02)/(v00*v11-v01*v01)
            IF(v < -Residual .or. v > 1.0d0+Residual)THEN
                flag = 0
                RETURN
            ELSE IF(v+u <= 1.0d0+Residual)THEN
                flag = 1
                RETURN
            ELSE
                flag = 0
                RETURN
            END IF
        END IF
    END IF
    END SUBROUTINE
    !-------------------------------
    SUBROUTINE judge_point_tetrahedron_do_not_use(pa , pb , pc , pd , po , flag)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb
    TYPE(Point_t) , INTENT(IN) :: pc
    TYPE(Point_t) , INTENT(IN) :: pd
    !
    TYPE(Point_t) , INTENT(IN) :: po
    !-------------------------------
    INTEGER(KIND=4) , INTENT(OUT) :: flag
    !-------------------------------
    REAL(KIND=8) :: v1,v2,v3,v4
    REAL(KIND=8) :: v
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-5
    REAL(KIND=8)  :: x(4),y(4),z(4),x_p,y_p,z_p
    !-------------------------------
    x(1) = pa%x
    x(2) = pb%x
    x(3) = pc%x
    x(4) = pd%x
    y(1) = pa%y
    y(2) = pb%y
    y(3) = pc%y
    y(4) = pd%y
    z(1) = pa%z
    z(2) = pb%z
    z(3) = pc%z
    z(4) = pd%z
    x_p = po%x
    y_p = po%y
    z_p = po%z
    !-----------------------------------
    !
    v1=(x(2)*y(3)*z(4)-x(2)*y(4)*z(3)+x(3)*y(4)*z(2)-x(3)*y(2)*z(4)+x(4)*y(2)*z(3)-x(4)*y(3)*z(2)-&
      x(3)*y(4)*z_p+x(3)*y_p*z(4)-x(4)*y_p*z(3)+x(4)*y(3)*z_p-x_p*y(3)*z(4)+x_p*y(4)*z(3)+&
      x(4)*y_p*z(2)-x(4)*y(2)*z_p+x_p*y(2)*z(4)-x_p*y(4)*z(2)+x(2)*y(4)*z_p-x(2)*y_p*z(4)-&
      x_p*y(2)*z(3)+x_p*y(3)*z(2)-x(2)*y(3)*z_p+x(2)*y_p*z(3)-x(3)*y_p*z(2)+x(3)*y(2)*z_p)/6.0
    v2=(x_p*y(3)*z(4)-x_p*y(4)*z(3)+x(3)*y(4)*z_p-x(3)*y_p*z(4)+x(4)*y_p*z(3)-x(4)*y(3)*z_p-&
      x(3)*y(4)*z(1)+x(3)*y(1)*z(4)-x(4)*y(1)*z(3)+x(4)*y(3)*z(1)-x(1)*y(3)*z(4)+x(1)*y(4)*z(3)+&
      x(4)*y(1)*z_p-x(4)*y_p*z(1)+x(1)*y_p*z(4)-x(1)*y(4)*z_p+x_p*y(4)*z(1)-x_p*y(1)*z(4)-&
      x(1)*y_p*z(3)+x(1)*y(3)*z_p-x_p*y(3)*z(1)+x_p*y(1)*z(3)-x(3)*y(1)*z_p+x(3)*y_p*z(1))/6.0
    v3=(x(2)*y_p*z(4)-x(2)*y(4)*z_p+x_p*y(4)*z(2)-x_p*y(2)*z(4)+x(4)*y(2)*z_p-x(4)*y_p*z(2)-&
      x_p*y(4)*z(1)+x_p*y(1)*z(4)-x(4)*y(1)*z_p+x(4)*y_p*z(1)-x(1)*y_p*z(4)+x(1)*y(4)*z_p+&
      x(4)*y(1)*z(2)-x(4)*y(2)*z(1)+x(1)*y(2)*z(4)-x(1)*y(4)*z(2)+x(2)*y(4)*z(1)-x(2)*y(1)*z(4)-&
      x(1)*y(2)*z_p+x(1)*y_p*z(2)-x(2)*y_p*z(1)+x(2)*y(1)*z_p-x_p*y(1)*z(2)+x_p*y(2)*z(1))/6.0
    v4=(x(2)*y(3)*z_p-x(2)*y_p*z(3)+x(3)*y_p*z(2)-x(3)*y(2)*z_p+x_p*y(2)*z(3)-x_p*y(3)*z(2)-&
      x(3)*y_p*z(1)+x(3)*y(1)*z_p-x_p*y(1)*z(3)+x_p*y(3)*z(1)-x(1)*y(3)*z_p+x(1)*y_p*z(3)+&
      x_p*y(1)*z(2)-x_p*y(2)*z(1)+x(1)*y(2)*z_p-x(1)*y_p*z(2)+x(2)*y_p*z(1)-x(2)*y(1)*z_p-&
      x(1)*y(2)*z(3)+x(1)*y(3)*z(2)-x(2)*y(3)*z(1)+x(2)*y(1)*z(3)-x(3)*y(1)*z(2)+x(3)*y(2)*z(1))/6.0    
    v =(x(2)*y(3)*z(4)-x(2)*y(4)*z(3)+x(3)*y(4)*z(2)-x(3)*y(2)*z(4)+x(4)*y(2)*z(3)-x(4)*y(3)*z(2)-&
        x(3)*y(4)*z(1)+x(3)*y(1)*z(4)-x(4)*y(1)*z(3)+x(4)*y(3)*z(1)-x(1)*y(3)*z(4)+x(1)*y(4)*z(3)+&
        x(4)*y(1)*z(2)-x(4)*y(2)*z(1)+x(1)*y(2)*z(4)-x(1)*y(4)*z(2)+x(2)*y(4)*z(1)-x(2)*y(1)*z(4)-&
        x(1)*y(2)*z(3)+x(1)*y(3)*z(2)-x(2)*y(3)*z(1)+x(2)*y(1)*z(3)-x(3)*y(1)*z(2)+x(3)*y(2)*z(1))/6.0
    v1 = abs(v1)
    v2 = abs(v2)
    v3 = abs(v3)
    v4 = abs(v4)
    v = abs(v)
    !-----------------------------------
    !
    IF(v1<residual .or. v2<residual .or. v3<residual .or. v4<residual)THEN
        flag = 2
    ELSE IF(abs(v - v1 - v2 - v3 - v4)<residual)THEN !
        flag = 1
    ELSE
        flag = 0
    END IF
    
    END SUBROUTINE
    SUBROUTINE judge_point_tetrahedron(pa , pb , pc , pd , po , flag)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb
    TYPE(Point_t) , INTENT(IN) :: pc
    TYPE(Point_t) , INTENT(IN) :: pd
    !
    TYPE(Point_t) , INTENT(IN) :: po
    !-------------------------------
    !
    INTEGER(KIND=4) , INTENT(OUT) :: flag
    !-------------------------------
    REAL(KIND=8) :: vabc,vacd,vadb,vcbd
    REAL(KIND=8) :: valu
    REAL(KIND=8) , PARAMETER :: sign = -1.0d0
    TYPE(Point_t) :: ab,ba,ac,ad,bc,cb,bd,db,cd
    TYPE(Point_t) :: oa,ob
    TYPE(Point_t) :: nabc,nacd,nadb,ncbd
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    ab = MinusVector(pb,pa)
    ba = MinusVector(pa,pb)
    ac = MinusVector(pc,pa)
    ad = MinusVector(pd,pa)
    bc = MinusVector(pc,pb)
    cb = MinusVector(pb,pc)
    bd = MinusVector(pd,pb)
    db = MinusVector(pb,pd)
    cd = MinusVector(pd,pc)
    oa = MinusVector(pa,po)
    ob = MinusVector(pb,po)
    !-----------------------------------
    !
    nabc = CrossProduct(ab,ac)
    !
    valu = DotProduct(nabc,ad)
    !
    IF(valu > 0.0d0)THEN
        nabc = MultValue(sign,nabc)
    END IF
    !-----------------------------------
    !
    nacd = CrossProduct(ac,ad)
    valu = DotProduct(nacd,ab)
    IF(valu > 0.0d0)THEN
        nacd = MultValue(sign,nacd)
    END IF
    !-----------------------------------
    !
    nadb = CrossProduct(ab,ad)
    valu = DotProduct(nadb,ac)
    IF(valu > 0.0d0)THEN
        nadb = MultValue(sign,nadb)
    END IF
    !-----------------------------------
    !
    ncbd = CrossProduct(bc,bd)
    valu = DotProduct(ncbd,ba)
    IF(valu > 0.0d0)THEN
        ncbd = MultValue(sign,ncbd)
    END IF
    !--------------------------------
    vabc = DotProduct(nabc,oa)
    vacd = DotProduct(nacd,oa)
    vadb = DotProduct(nadb,oa)
    vcbd = DotProduct(ncbd,ob)
    !
    IF(vabc > 0.0d0 .and. vacd > 0.0d0 .and. vadb > 0.0d0 .and. vcbd > 0.0d0)THEN
        !
        flag = 1
    ELSE IF(vabc >= -Residual .and. vacd >= -Residual .and. vadb >= -Residual .and. &
        vcbd >= -Residual .and. ABS(vabc*vacd*vadb*vcbd) <= Residual)THEN
    
    flag = 2
    ELSE
        flag = 0
    END IF
    
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE judge_seg_seg( aa , ab , ba , bb , ss)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: aa
    TYPE(Point_t) , INTENT(IN) :: ab
    !
    TYPE(Point_t) , INTENT(IN) :: ba
    TYPE(Point_t) , INTENT(IN) :: bb
    !
    INTEGER(KIND=4) , INTENT(OUT) :: ss
    !-------------------------------
    IF(&
        (same(aa,ba) == 1 .and. same(ab,bb) == 1) .or. &
        (same(aa,bb) == 1 .and. same(ab,ba) == 1))THEN
    ss = 1
    ELSE
        ss = 0
    END IF
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE judge_face_face(aa , ab , ac , ba , bb , bc , ff)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: aa
    TYPE(Point_t) , INTENT(IN) :: ab
    TYPE(Point_t) , INTENT(IN) :: ac
    !
    TYPE(Point_t) , INTENT(IN) :: ba
    TYPE(Point_t) , INTENT(IN) :: bb
    TYPE(Point_t) , INTENT(IN) :: bc
    !
    INTEGER(KIND=4) , INTENT(OUT) :: ff
    !-------------------------------
    !
    IF(&
        (same(aa,ba) == 1 .and. same(ab,bb) == 1 .and. same(ac,bc) == 1) .or.   &
        (same(aa,ba) == 1 .and. same(ab,bc) == 1 .and. same(ac,bb) == 1) .or.   &
        (same(aa,bb) == 1 .and. same(ab,ba) == 1 .and. same(ac,bc) == 1) .or.   &
        (same(aa,bb) == 1 .and. same(ab,bc) == 1 .and. same(ac,ba) == 1) .or.   &
        (same(aa,bc) == 1 .and. same(ab,ba) == 1 .and. same(ac,bb) == 1) .or.   &
        (same(aa,bc) == 1 .and. same(ab,bb) == 1 .and. same(ac,ba) == 1))THEN
    ff = 1
    ELSE
        ff = 0
    END IF
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE judge_face_ele(fa , fb , fc , ta , tb , tc , td , fe)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: fa
    TYPE(Point_t) , INTENT(IN) :: fb
    TYPE(Point_t) , INTENT(IN) :: fc
    !
    TYPE(Point_t) , INTENT(IN) :: ta
    TYPE(Point_t) , INTENT(IN) :: tb
    TYPE(Point_t) , INTENT(IN) :: tc
    TYPE(Point_t) , INTENT(IN) :: td
    !
    INTEGER(KIND=4) , INTENT(OUT) :: fe
    !-------------------------------
    !
    INTEGER(KIND=4) :: ff
    !-------------------------------
    !
    CALL judge_face_face(fa,fb,fc,ta,tb,tc,ff)
    IF(ff == 1)THEN
        fe = 1
        RETURN
    END IF
    !
    CALL judge_face_face(fa,fb,fc,ta,tb,td,ff)
    IF(ff == 1)THEN
        fe = 1
        RETURN
    END IF
    !
    CALL judge_face_face(fa,fb,fc,ta,tc,td,ff)
    IF(ff == 1)THEN
        fe = 1
        RETURN
    END IF
    !
    CALL judge_face_face(fa,fb,fc,tb,tc,td,ff)
    IF(ff == 1)THEN
        fe = 1
        RETURN
    END IF
    fe = 0
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE intersect_seg_seg_a(pa , pb , pc , pd , ll , pos)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb

    TYPE(Point_t) , INTENT(IN) :: pc
    TYPE(Point_t) , INTENT(IN) :: pd
    !-------------------------------

    INTEGER(KIND=4) , INTENT(OUT) :: ll

    TYPE(Point_t) , DIMENSION(2) , INTENT(OUT) :: pos
    !-------------------------------
    INTEGER(KIND=4) :: n

    INTEGER(KIND=4) , DIMENSION(4) :: indx
    
    REAL(KIND=8) , DIMENSION(4) :: datas

    TYPE(Point_t) , DIMENSION(4) :: coords
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------

    CALL DataVector(pa,coords(1))
    CALL DataVector(pb,coords(2))
    CALL DataVector(pc,coords(3))
    CALL DataVector(pd,coords(4))
    IF(ABS(pa%x-pb%x) < Residual)THEN

        IF(ABS(pa%y-pb%y) < Residual)THEN

            datas(1) = pa%z
            datas(2) = pb%z
            datas(3) = pc%z
            datas(4) = pd%z
            CALL qsort(n,datas,indx)
            IF( (indx(1) == 1 .and. indx(2) == 2) .or. &
                (indx(1) == 2 .and. indx(2) == 1) .or. &
                (indx(1) == 3 .and. indx(2) == 4) .or. &
                (indx(1) == 4 .and. indx(2) == 3))THEN

            ll = 0
            RETURN
            ELSE
                IF(ABS(coords(indx(2))%x-coords(indx(3))%x) < Residual)THEN

                    ll = 0
                    RETURN
                ELSE
                    
                    CALL DataVector(coords(indx(2)),pos(1))
                    CALL DataVector(coords(indx(3)),pos(2))
                    ll = 1
                END IF
            END IF
        ELSE
           
            datas(1) = pa%y
            datas(2) = pb%y
            datas(3) = pc%y
            datas(4) = pd%y
            CALL qsort(n,datas,indx)
            IF( (indx(1) == 1 .and. indx(2) == 2) .or. &
                (indx(1) == 2 .and. indx(2) == 1) .or. &
                (indx(1) == 3 .and. indx(2) == 4) .or. &
                (indx(1) == 4 .and. indx(2) == 3))THEN
            
            ll = 0
            RETURN
            ELSE
                IF(ABS(coords(indx(2))%x-coords(indx(3))%x) < Residual)THEN
                   
                    ll = 0
                    RETURN
                ELSE
                    
                    CALL DataVector(coords(indx(2)),pos(1))
                    CALL DataVector(coords(indx(3)),pos(2))
                    ll = 1
                END IF
            END IF
        END IF
    ELSE
        
        datas(1) = pa%x
        datas(2) = pb%x
        datas(3) = pc%x
        datas(4) = pd%x
        CALL qsort(n,datas,indx)
        IF((indx(1) == 1 .and. indx(2) == 2) .or. &
            (indx(1) == 2 .and. indx(2) == 1) .or. &
            (indx(1) == 3 .and. indx(2) == 4) .or. &
            (indx(1) == 4 .and. indx(2) == 3))THEN
        
        ll = 0
        RETURN
        ELSE
            IF(ABS(coords(indx(2))%x-coords(indx(3))%x) < Residual)THEN
                
                ll = 0
                RETURN
            ELSE
                
                CALL DataVector(coords(indx(2)),pos(1))
                CALL DataVector(coords(indx(3)),pos(2))
                ll = 1
            END IF
        END IF
    END IF
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    SUBROUTINE intersect_seg_seg_b(pa , pb , pc , pd , ll , pos)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb

    TYPE(Point_t) , INTENT(IN) :: pc
    TYPE(Point_t) , INTENT(IN) :: pd
    !-------------------------------

    INTEGER(KIND=4) , INTENT(OUT) :: ll
    
    TYPE(Point_t) , DIMENSION(2) , INTENT(OUT) :: pos
    !-------------------------------
    INTEGER(KIND=4) :: n
    !
    TYPE(Point_t) :: u
    !
    TYPE(Point_t) :: ab,ac,ad
    !
    REAL(KIND=8) :: abnorm
    !
    INTEGER(KIND=4) , DIMENSION(4) :: indx
    !
    REAL(KIND=8) , DIMENSION(4) :: datas
    !
    TYPE(Point_t) , DIMENSION(4) :: coords
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    !
    CALL DataVector(pa,coords(1))
    CALL DataVector(pb,coords(2))
    CALL DataVector(pc,coords(3))
    CALL DataVector(pd,coords(4))
    !
    datas(1) = 0.0d0
    ab = MinusVector(pb,pa)
    ac = MinusVector(pc,pa)
    ad = MinusVector(pd,pa)
    abnorm = distance(pa,pb)
    !
    datas(2) = abnorm
    u = MultValue(1.0d0/abnorm,ab)
    !
    datas(3) = DotProduct(u,ac)
    !
    datas(4) = DotProduct(u,ad)
    indx = 0
    n = 4
    CALL qsort(n,datas,indx)
    IF((indx(1) == 1 .and. indx(2) == 2) .or. &
        (indx(1) == 2 .and. indx(2) == 1) .or. &
        (indx(1) == 3 .and. indx(2) == 4) .or. &
        (indx(1) == 4 .and. indx(2) == 3))THEN
    !
    ll = 0
    RETURN
    ELSE
        IF(ABS(datas(indx(2)) - datas(indx(3))) < Residual)THEN
            !
            ll = 0
            RETURN
        ELSE
            !
            CALL DataVector(coords(indx(2)),pos(1))
            CALL DataVector(coords(indx(3)),pos(2))
            ll = 1
        END IF
    END IF
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE intersect_seg_triangle(sa , sb , pa , pb , pc , ps , pt , po , pos , coincide)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: sa
    TYPE(Point_t) , INTENT(IN) :: sb
    !
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb
    TYPE(Point_t) , INTENT(IN) :: pc

    INTEGER(KIND=4) , INTENT(OUT) :: ps

    INTEGER(KIND=4) , INTENT(OUT) :: pt
    !
    TYPE(Point_t) , INTENT(OUT) :: po
    !
    TYPE(Point_t) , DIMENSION(2) , INTENT(OUT) :: pos
    !
    TYPE(Point_t) , DIMENSION(2) , INTENT(OUT) :: coincide
    !-------------------------------
    INTEGER(KIND=4) :: cross

    INTEGER(KIND=4) :: ll
    INTEGER(KIND=4) :: ss
    INTEGER(KIND=4) :: nss
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    CALL intersect_line_plane(sa,sb,pa,pb,pc,cross,po)
    IF(cross == 0)THEN
        !
        ps = 0
        pt = 0
        RETURN
    ELSE IF(cross == 1)THEN

        CALL judge_point_segment_b(sa,sb,po,ps)
        IF(ps == 0)THEN
            RETURN
        ELSE
            !
            CALL judge_point_triangle(pa,pb,pc,po,pt)
        END IF
    ELSE IF(cross == 3)THEN
        !
        ps = 2
        !
        CALL judge_point_triangle(pa,pb,pc,po,pt)
    ELSE IF(cross == 2)THEN
        !
        nss = 0
        !
        CALL intersect_line_line(sa,sb,pa,pb,ll,po)
        IF(ll == 2)THEN

            CALL intersect_seg_seg_b(sa,sb,pa,pb,ss,pos)
            IF(ss == 1)THEN
                !
                ps = 5
                pt = 5
                !
                CALL DataVector(pa,coincide(1))
                CALL DataVector(pb,coincide(2))
                RETURN
            ELSE
                !
                ps = 4
                pt = 4
                RETURN
            END IF
        ELSE IF(ll == 1)THEN
            nss = nss + 1
            !
            CALL DataVector(po,pos(nss))
        END IF
        !
        CALL intersect_line_line(sa,sb,pa,pc,ll,po)
        IF(ll == 2)THEN
            CALL intersect_seg_seg_b(sa,sb,pa,pc,ss,pos)
            IF(ss == 1)THEN
                !
                ps = 5
                pt = 5
                CALL DataVector(pa,coincide(1))
                CALL DataVector(pc,coincide(2))
                RETURN
            ELSE
                !
                ps = 4
                pt = 4
                RETURN
            END IF
        ELSE IF(ll == 1)THEN

            nss = nss + 1
            IF(nss == 2)THEN
                IF(distance(po,pos(1)) < Residual)THEN

                    nss = 1
                ELSE
                    
                    CALL DataVector(po,pos(nss))
                    ps = 3
                    pt = 3
                END IF
            ELSE
               
                CALL DataVector(po,pos(nss))
            END IF
        END IF
        !
        CALL intersect_line_line(sa,sb,pb,pc,ll,po)
        IF(ll == 2)THEN
            !
            CALL intersect_seg_seg_b(sa,sb,pb,pc,ss,pos)
            IF(ss == 1)THEN
                !
                ps = 5
                pt = 5
                CALL DataVector(pb,coincide(1))
                CALL DataVector(pc,coincide(2))
                RETURN
            ELSE
                !
                ps = 4
                pt = 4
                RETURN
            END IF
        ELSE IF(ll == 1)THEN
            !
            nss = nss + 1
            IF(nss == 2)THEN
                IF(distance(po,pos(1)) < Residual)THEN
                    nss = 1
                ELSE
                    !
                    CALL DataVector(po,pos(2))
                    ps = 3
                    pt = 3
                    RETURN
                END IF
            ELSE
                !
                IF(nss <= 2)THEN
                    CALL DataVector(po,pos(nss))
                END IF
            END IF
        END IF
        !------------------------------------
        IF(nss == 0)THEN
            !
            ps = 4
            pt = 4
            RETURN
        ELSE IF(nss == 1)THEN
            IF(distance(pos(1),sa) < Residual .or. distance(pos(1),sb) < Residual)THEN
                !
                ps = 4
                pt = 4
                RETURN
            ELSE
                CALL judge_point_triangle(pa,pb,pc,sa,pt)
                IF(pt == 0)THEN
                    !
                    CALL judge_point_triangle(pa,pb,pc,sb,pt)
                    IF(pt == 0)THEN
                        !
                        ps = 4
                        pt = 4
                        RETURN
                    ELSE IF(pt == 1)THEN
                        !
                        CALL DataVector(sb,pos(2))
                        ps = 3
                        pt = 3
                        RETURN
                    END IF
                ELSE IF(pt==1)THEN
                    !
                    CALL DataVector(sa,pos(2))
                    ps = 3
                    pt = 3
                    RETURN
                END IF
            END IF
        END IF
    END IF
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE intersect_line_line(la , lb , lc , ld , ll , po)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: la
    TYPE(Point_t) , INTENT(IN) :: lb
    !
    TYPE(Point_t) , INTENT(IN) :: lc
    TYPE(Point_t) , INTENT(IN) :: ld
    !-------------------------------
    INTEGER(KIND=4) , INTENT(OUT) :: ll
    !
    TYPE(Point_t) , INTENT(OUT) :: po
    !-------------------------------
    INTEGER(KIND=4) :: ps
    TYPE(Point_t) :: ab,ac,cd,tab
    !ac*cd
    TYPE(Point_t) :: accd
    !lb*cd
    TYPE(Point_t) :: abcd
    REAL(KIND=8) :: t
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    ab = MinusVector(lb,la)
    ac = MinusVector(lc,la)
    cd = MinusVector(ld,lc)
    accd = CrossProduct(ac,cd)
    abcd = CrossProduct(ab,cd)
    IF(SQRT(abcd%x**2+abcd%y**2+abcd%z**2) < Residual)THEN
        !If lb*cd is a 0 vector, the two line segments are parallel or overlapping
        IF(SQRT(accd%x**2+accd%y**2+accd%z**2) < Residual)THEN
            !If ac*cd is also a 0 vector, the two line segments overlap
            ll = 2
        ELSE
            !Otherwise, the two line segments are parallel and have no intersection.
            ll = 0
        END IF
    ELSE
        !Two line segments intersect
        IF(ABS(abcd%x) > Residual)THEN
            t = accd%x/abcd%x
        ELSE IF(ABS(abcd%y) > Residual)THEN
            t = accd%y/abcd%y
        ELSE IF(ABS(abcd%z) > Residual)THEN
            t = accd%z/abcd%z
        ELSE
            t = 0.0d0
        END IF
        IF(t >= 0.0d0 .and. t <= 1.0d0)THEN
            !The intersection point is on line segment lb
            tab = MultValue(t,ab)
            po = PlusVector(la,tab)
            CALL judge_point_segment_b(lc,ld,po,ps)
            IF(ps > 0)THEN
                !Determine the intersection point on line segment cd
                ll = 1
            ELSE
                !
                ll = 0
            END IF
        ELSE
            ll = 0
        END IF
    END IF
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE intersect_line_plane(la , lb , pa , pb , pc , cross , pt)
    !-------------------------------
    IMPLICIT NONE
    !-------------------------------
    !
    TYPE(Point_t) , INTENT(IN) :: la
    TYPE(Point_t) , INTENT(IN) :: lb
    !
    TYPE(Point_t) , INTENT(IN) :: pa
    TYPE(Point_t) , INTENT(IN) :: pb
    TYPE(Point_t) , INTENT(IN) :: pc
    !-------------------------------
    !
    INTEGER(KIND=4) , INTENT(OUT) :: cross
    !
    TYPE(Point_t) , INTENT(OUT) :: pt
    !-------------------------------
    !
    TYPE(Point_t) :: normal
    TYPE(Point_t) :: va,vb
    TYPE(Point_t) :: u,v,w,tu
    REAL(KIND=8) :: un,vn,wn
    REAL(KIND=8) :: t
    REAL(KIND=8) , PARAMETER :: Residual = 1.0d-8
    !-------------------------------
    !
    va = MinusVector(pb,pa)
    vb = MinusVector(pb,pc)
    normal = CrossProduct(va,vb)
    !-------------------------------
    !p2-p1
    u = MinusVector(lb,la)
    !p1-pon
    v = MinusVector(la,pc)
    !(p2-p1)¡¤n
    un = DotProduct(u,normal)
    !(p1-pon)¡¤n
    vn = DotProduct(v,normal)
    IF(ABS(vn) < Residual)THEN
        w = MinusVector(lb,pc)
        !(p2-pon)¡¤n
        wn = DotProduct(w,normal)
        IF(ABS(wn) < Residual)THEN
            cross = 2
            RETURN
        ELSE
            cross = 3
            CALL DataVector(la,pt)
            RETURN
        END IF
    ELSE IF(ABS(un) < Residual) THEN
        cross = 0
        RETURN
    END IF
    !t = ©\(P1 ©\Pon)¡¤N /(P2 ©\P1)¡¤N
    t = -vn / un
    !t(P2 ©\P1)
    tu = MultValue(t,u)
    !Pt = P1 + t(P2 ©\P1)
    pt = PlusVector(la,tu)
    cross = 1
    !-------------------------------
    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    SUBROUTINE unit_normal_vector(na , nb , nc , n)
    !-------------------------------
    use PointMod , ONLY : Point_t
    !-------------------------------
    IMPLICIT NONE
    !
    TYPE(Point_t) , INTENT(IN) :: na
    TYPE(Point_t) , INTENT(IN) :: nb
    TYPE(Point_t) , INTENT(IN) :: nc
    !
    TYPE(Point_t) , INTENT(OUT):: n
    !-------------------------------
    REAL(KIND=8) :: length
    !-------------------------------

    !
    IF(na%x == nb%x .and. nb%x == nc%x)THEN
        n%x = 1.0d0
        n%y = 0.0d0
        n%z = 0.0d0
    ELSE IF(na%y == nb%y .and. nb%y == nc%y)THEN
        n%x = 0.0d0
        n%y = 1.0d0
        n%z = 0.0d0
    ELSE IF(na%z == nb%z .and. nb%z == nc%z)THEN
        n%x = 0.0d0
        n%y = 0.0d0
        n%z = 1.0d0
    ELSE
        n%z = 1.0d0
        IF((nb%y-na%y)*(nc%x-na%x)-(nc%y-na%y)*(nb%x-na%x) == 0.0d0)THEN
            n%y = 0.0d0
        ELSE
            n%y = ((na%z-nb%z)*(nc%x-na%x)-(na%z-nc%z)*(nb%x-na%x))/&
                ((nb%y-na%y)*(nc%x-na%x)-(nc%y-na%y)*(nb%x-na%x))
        END IF
        IF(nb%x-na%x == 0.0d0)THEN
            n%x = 0.0d0
        ELSE
            n%x = ((na%z-nb%z)-(na%y-nb%y)*n%y)/(nb%x-na%x)
        END IF
        length = SQRT(n%x**2+n%y**2+n%z**2)
        n%x = n%x / length
        n%y = n%y / length
        n%z = n%z / length
    END IF

    END SUBROUTINE
    !-------------------------------
    !
    !-------------------------------
    END MODULE
