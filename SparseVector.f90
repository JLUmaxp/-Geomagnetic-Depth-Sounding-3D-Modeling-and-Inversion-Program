    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  
    !  Modified from ModEM by Gary Egbert and Anna Kelbert.
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    module SparseVectorMod
    !------------------------------------------------------------------------
    ! Sparse complex and real vector operations. At present, sparse vectors are used for
    ! representation of data functionals. Both real and complex vectors are supported, and
    ! only those routines needed for modeling and initial work on inversion have
    ! been developed.
    ! Not specific to EM problem, no dependency on outside (from other classes) modules.
    !------------------------------------------------------------------------
    use ParameterMod
    use MeshDefMod
    use VectorMod
    !------------------------------------------------------------------------
    implicit none
    !------------------------------------------------------------------------
    INTERFACE create
    module procedure create_sparsevecc
    module procedure create_sparsevecr
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE deall
    module procedure deall_sparsevecc
    module procedure deall_sparsevecr
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE reall
    module procedure reall_sparsevecc
    module procedure reall_sparsevecr
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE scMult
    module procedure scMult_sparsevecc
    module procedure scMult_sparsevecr
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE linComb
    module procedure linComb_sparsevecc
    module procedure linComb_sparsevecr
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE add
    module procedure add_scvector
    module procedure add_srvector
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE dotProd
    module procedure dotProd_scvector_f
    module procedure dotProd_srvector_f
    !module procedure dotProd_csvector_f
    !module procedure dotProd_rsvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE dotProd_noConj
    module procedure dotProd_noConj_scvector_f
    !module procedure dotProd_noConj_csvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE conjg
    module procedure conjg_sparsevecc_f
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE newValue
    module procedure newValueC_sparsevecc
    module procedure newValueR_sparsevecc
    module procedure newValueR_sparsevecr
    module procedure copyValue_csvector
    module procedure copyValue_rsvector
    END INTERFACE
    !------------------------------------------------------------------------
    interface assignment (=)
    module PROCEDURE copy_sparsevecc
    module PROCEDURE copy_sparsevecr
    module PROCEDURE copy_csvector
    end interface
    !------------------------------------------------------------------------
    public :: sparsevecc_t, sparsevecr_t
    public :: create_sparsevecc, deall_sparsevecc
    public :: create_sparsevecr, deall_sparsevecr
    public :: read_sparsevecc, write_sparsevecc
    public :: read_sparsevecr, write_sparsevecr
    public :: scMult_sparsevecc
    public :: scMult_sparsevecr
    public :: newValueC_sparsevecc, newValueR_sparsevecc
    public :: newValueR_sparsevecr
    public :: copyValue_csvector, conjg_sparsevecc_f
    public :: copyValue_rsvector
    public :: copy_sparsevecc, linComb_sparsevecc
    public :: copy_sparsevecr, linComb_sparsevecr
    public :: dotProd_scvector_f, dotProd_csvector_f
    public :: dotProd_srvector_f, dotProd_rsvector_f
    public :: add_scvector
    public :: add_srvector
    !------------------------------------------------------------------------
    type :: sparsevecc_t

        ! complex vector defined on edges;
        ! store the intention of the use in a character string defined
        ! as a parameter: EDGE or NODE
        character (len=80)                                  :: FEType=''
        ! nCoeff is number of non-zero nodes
        integer                         :: nCoeff  = 0
        ! xyz = 1,2,3 refers to x, y or z components,
        ! i is the array of indices that defines grid location
        integer , pointer, dimension(:)         :: i,xyz
        ! c is complex array of coefficients
        complex (kind=8), pointer, dimension(:)     :: c
        ! has sparse vector been allocated?
        logical                 :: allocated = .false.
        ! temporary:  .true. for function outputs only; necessary to avoid memory leaks
        ! (probably will not be needed in the future when compilers will support
        ! ISO/IEC 15581 - the "allocatable array extension")
        logical                 :: temporary = .false.
        ! pointer to the parent grid not needed or set: we can
        ! make full use of the sparse vector without it...
        ! - should be passed explicitly if it is ever required!
        !type (Mesh_t), pointer                     :: grid

    end type sparsevecc_t
    !------------------------------------------------------------------------
    type :: sparsevecr_t

        ! real vector defined on edges or nodes;
        ! store the intention of the use in a character string defined
        ! as a parameter: EDGE or NODE
        character (len=80)                                  :: FEType=''
        ! nCoeff is number of non-zero nodes
        integer                         :: nCoeff  = 0
        ! xyz = 1,2,3 refers to x, y or z components,
        ! i is the array of indices that defines grid location
        integer , pointer, dimension(:)         :: i,xyz
        ! c is real array of coefficients
        real (kind=8), pointer, dimension(:)    :: c
        ! has sparse vector been allocated?
        logical                 :: allocated = .false.
        ! temporary:  .true. for function outputs only; necessary to avoid memory leaks
        ! (probably will not be needed in the future when compilers will support
        ! ISO/IEC 15581 - the "allocatable array extension")
        logical                 :: temporary = .false.
        ! pointer to the parent grid not needed or set: we can
        ! make full use of the sparse vector without it...
        ! - should be passed explicitly if it is ever required!
        !type (Mesh_t), pointer                     :: grid

    end type sparsevecr_t
    !------------------------------------------------------------------------
    Contains

    !------------------------------------------------------------------------
    ! delete/ deallocate the sparse vector
    !------------------------------------------------------------------------
    subroutine deall_sparsevecc(oldLC)

    implicit none
    type (sparsevecc_t)                :: oldLC
    integer                          :: status

    if(oldLC%allocated) then
        deallocate(oldLC%i,STAT=status)
        deallocate(oldLC%xyz, STAT=status)
        deallocate(oldLC%c, STAT=status)
        oldLC%FEType = ''
        oldLC%allocated = .false.
    end if

    !=================================================================
    !==================================================== Added by Oat
    !=================================================================
    oldLC%nCoeff = 0

    end subroutine deall_sparsevecc

    !------------------------------------------------------------------------
    ! delete/ deallocate the sparse vector
    !------------------------------------------------------------------------
    subroutine deall_sparsevecr(oldLC)

    implicit none
    type (sparsevecr_t)                :: oldLC
    integer                          :: status

    if(oldLC%allocated) then
        deallocate(oldLC%i,STAT=status)
        deallocate(oldLC%xyz, STAT=status)
        deallocate(oldLC%c, STAT=status)
        oldLC%FEType = ''
        oldLC%allocated = .false.
    end if

    !=================================================================
    !==================================================== Added by Oat
    !=================================================================
    oldLC%nCoeff = 0

    end subroutine deall_sparsevecr

    !------------------------------------------------------------------------
    ! create an object of type sparsevecc_t of length nCoeff
    ! pointer to the grid not needed (and in some cases
    ! when sparse vectors are set up, we don't even have it).
    !------------------------------------------------------------------------
    subroutine create_sparsevecc(nCoeff,newLC,FEType)

    implicit none
    integer, intent(in)                     :: nCoeff
    type (sparsevecc_t), intent(inout)        :: newLC
    character (len=80), intent(in)          :: FEType
    integer                 :: status

    ! the old baggage is out of the door
    if(newLC%allocated) then
        deallocate(newLC%i, STAT = status)
        deallocate(newLC%xyz, STAT = status)
        deallocate(newLC%c, STAT = status)
        newLC%FEType = ''
        newLC%allocated = .false.
    endif

    newLC%allocated = .true.
    allocate(newLC%i(nCoeff),STAT=status)
    newLC%allocated = newLC%allocated .and. (status .eq. 0)
    allocate(newLC%xyz(nCoeff),STAT=status)
    newLC%allocated = newLC%allocated .and. (status .eq. 0)
    allocate(newLC%c(nCoeff),STAT=status)
    newLC%allocated = newLC%allocated .and. (status .eq. 0)

    newLC%nCoeff = nCoeff
    newLC%i = 0
    newLC%xyz = 0
    newLC%c = C_ZERO
    newLC%FEType = FEType

    end subroutine create_sparsevecc

    !------------------------------------------------------------------------
    ! create an object of type sparsevecr_t of length nCoeff
    ! pointer to the grid not needed (and in some cases
    ! when sparse vectors are set up, we don't even have it).
    !------------------------------------------------------------------------
    subroutine create_sparsevecr(nCoeff,newLC,FEType)

    implicit none
    integer, intent(in)                     :: nCoeff
    type (sparsevecr_t), intent(inout)        :: newLC
    character (len=80), intent(in)          :: FEType
    integer                 :: status

    ! the old baggage is out of the door
    if(newLC%allocated) then
        deallocate(newLC%i, STAT = status)
        deallocate(newLC%xyz, STAT = status)
        deallocate(newLC%c, STAT = status)
        newLC%FEType = ''
        newLC%allocated = .false.
    endif

    newLC%allocated = .true.
    allocate(newLC%i(nCoeff),STAT=status)
    newLC%allocated = newLC%allocated .and. (status .eq. 0)
    allocate(newLC%xyz(nCoeff),STAT=status)
    newLC%allocated = newLC%allocated .and. (status .eq. 0)
    allocate(newLC%c(nCoeff),STAT=status)
    newLC%allocated = newLC%allocated .and. (status .eq. 0)

    newLC%nCoeff = nCoeff
    newLC%i = 0
    newLC%xyz = 0
    newLC%c = R_ZERO
    newLC%FEType = FEType

    end subroutine create_sparsevecr

    !------------------------------------------------------------------------
    ! Reallocates an object of type sparsevecc_t. The object has to already be
    ! allocated. If allocated and shorter than nCoeff, more memory is
    ! allocated at the end and the contents are preserved.
    ! If allocated and longer than nCoeff, truncates to the first nCoeff values.
    ! This is useful when we need to store the information somewhere, but do
    ! not yet know the final length of the vector. Once it is fully read and
    ! the number of coefficients is known, use this routine to truncate
    ! to the correct length preserving all the values already stored.
    !------------------------------------------------------------------------
    subroutine reall_sparsevecc(nCoeff,newLC)

    implicit none
    integer, intent(in)                         :: nCoeff
    type (sparsevecc_t), intent(inout)            :: newLC
    type (sparsevecc_t)                           :: tempLC
    integer                                     :: n, status

    ! the old baggage is out of the door
    if(.not. newLC%allocated) then
        write(0, *) 'The input sparsevecc_t has to be allocated in reall_sparsevecc'
        return
    end if

    tempLC = newLC

    if (tempLC%nCoeff .eq. nCoeff) then
        ! do nothing
    else
        call deall_sparsevecc(newLC)
        newLC%allocated = .true.
        allocate(newLC%i(nCoeff),STAT=status)
        newLC%allocated = newLC%allocated .and. (status .eq. 0)
        allocate(newLC%xyz(nCoeff),STAT=status)
        newLC%allocated = newLC%allocated .and. (status .eq. 0)
        allocate(newLC%c(nCoeff),STAT=status)
        newLC%allocated = newLC%allocated .and. (status .eq. 0)
        newLC%FEType = tempLC%FEType
        newLC%nCoeff = nCoeff
    end if

    if (tempLC%nCoeff > nCoeff) then
        ! new vector will be shorter
        do n = 1,nCoeff
            newLC%i(n) = tempLC%i(n)
            newLC%xyz(n) = tempLC%xyz(n)
            newLC%c(n) = tempLC%c(n)
        end do
    else if (tempLC%nCoeff < nCoeff) then
        ! new vector will be longer; copy the old values
        do n = 1,tempLC%nCoeff
            newLC%i(n) = tempLC%i(n)
            newLC%xyz(n) = tempLC%xyz(n)
            newLC%c(n) = tempLC%c(n)
        end do
        ! ... then pad with zeroes
        do n = tempLC%nCoeff+1,nCoeff
            newLC%i(n) = 0
            newLC%xyz(n) = 0
            newLC%c(n) = C_ZERO
        end do
    end if

    call deall_sparsevecc(tempLC)

    end subroutine reall_sparsevecc

    !------------------------------------------------------------------------
    ! Reallocates an object of type sparsevecr_t. The object has to already be
    ! allocated. If allocated and shorter than nCoeff, more memory is
    ! allocated at the end and the contents are preserved.
    ! If allocated and longer than nCoeff, truncates to the first nCoeff values.
    ! This is useful when we need to store the information somewhere, but do
    ! not yet know the final length of the vector. Once it is fully read and
    ! the number of coefficients is known, use this routine to truncate
    ! to the correct length preserving all the values already stored.
    !------------------------------------------------------------------------
    subroutine reall_sparsevecr(nCoeff,newLC)

    implicit none
    integer, intent(in)                         :: nCoeff
    type (sparsevecr_t), intent(inout)            :: newLC
    type (sparsevecr_t)                           :: tempLC
    integer                                     :: n, status

    ! the old baggage is out of the door
    if(.not. newLC%allocated) then
        write(0, *) 'The input sparsevecr_t has to be allocated in reall_sparsevecr'
        return
    end if

    tempLC = newLC

    if (tempLC%nCoeff .eq. nCoeff) then
        ! do nothing
    else
        call deall_sparsevecr(newLC)
        newLC%allocated = .true.
        allocate(newLC%i(nCoeff),STAT=status)
        newLC%allocated = newLC%allocated .and. (status .eq. 0)
        allocate(newLC%xyz(nCoeff),STAT=status)
        newLC%allocated = newLC%allocated .and. (status .eq. 0)
        allocate(newLC%c(nCoeff),STAT=status)
        newLC%allocated = newLC%allocated .and. (status .eq. 0)
        newLC%FEType = tempLC%FEType
        newLC%nCoeff = nCoeff
    end if

    if (tempLC%nCoeff > nCoeff) then
        ! new vector will be shorter
        do n = 1,nCoeff
            newLC%i(n) = tempLC%i(n)
            newLC%xyz(n) = tempLC%xyz(n)
            newLC%c(n) = tempLC%c(n)
        end do
    else if (tempLC%nCoeff < nCoeff) then
        ! new vector will be longer; copy the old values
        do n = 1,tempLC%nCoeff
            newLC%i(n) = tempLC%i(n)
            newLC%xyz(n) = tempLC%xyz(n)
            newLC%c(n) = tempLC%c(n)
        end do
        ! ... then pad with zeroes
        do n = tempLC%nCoeff+1,nCoeff
            newLC%i(n) = 0
            newLC%xyz(n) = 0
            newLC%c(n) = R_ZERO
        end do
    end if

    call deall_sparsevecr(tempLC)

    end subroutine reall_sparsevecr

    !------------------------------------------------------------------------
    ! write_sparsevecc writes a sparse vector  in a simple ASCII format; vector has
    ! to exist and be allocated before calling this routine, and the file unit
    ! must already be available for writing.
    !------------------------------------------------------------------------
    subroutine write_sparsevecc(fid, SV)

    integer,        intent(in)        :: fid
    type (sparsevecc_t), intent(in)     :: SV

    !  local variables
    integer                           :: ii, istat

    if(.not. SV%allocated) then
        write(0, *) 'sparse vector must be allocated before call to write_sparsevecc'
        return
    endif

    write(fid,'(i12,a10)',iostat=istat) SV%nCoeff,trim(SV%FEType)

    do ii = 1,SV%nCoeff
        write(fid,'(4i5,2es14.6)',iostat=istat) SV%i(ii),SV%xyz(ii),real(SV%c(ii)),aimag(SV%c(ii))
    end do

    end subroutine write_sparsevecc

    !------------------------------------------------------------------------
    ! write_sparsevecr writes a sparse vector  in a simple ASCII format; vector has
    ! to exist and be allocated before calling this routine, and the file unit
    ! must already be available for writing.
    !------------------------------------------------------------------------
    subroutine write_sparsevecr(fid, SV)

    integer,        intent(in)        :: fid
    type (sparsevecr_t), intent(in)     :: SV

    !  local variables
    integer                           :: ii, istat

    if(.not. SV%allocated) then
        write(0, *) 'sparse vector must be allocated before call to write_sparsevecc'
        return
    endif

    write(fid,'(i12,a10)',iostat=istat) SV%nCoeff,trim(SV%FEType)

    do ii = 1,SV%nCoeff
        write(fid,'(4i5,2es14.6)',iostat=istat) SV%i(ii),SV%xyz(ii),SV%c(ii)
    end do

    end subroutine write_sparsevecr

    !------------------------------------------------------------------------
    ! read_sparsevecc reads a sparse vector in a simple ASCII format; vector must match
    ! the input grid; file unit must already be available for reading.
    !------------------------------------------------------------------------
    subroutine read_sparsevecc(fid, SV, grid)

    integer,        intent(in)        :: fid
    type (sparsevecc_t), intent(inout)  :: SV
    type (Mesh_t), intent(in), optional :: grid

    !  local variables
    integer                           :: nCoeff
    character(80)                     :: FEType
    integer                           :: ii, istat
    real (kind(SV%c))                  :: cr, ci

    read(fid,*,iostat=istat) nCoeff,FEType

    if(.not. SV%allocated) then
        call create_sparsevecc(nCoeff,SV,FEType)
    else
        call reall_sparsevecc(nCoeff,SV)
        SV%FEType = FEType
    endif

    do ii = 1,nCoeff
        ! makes the acceptable formatting more flexible
        read(fid,*,iostat=istat) SV%i(ii),SV%xyz(ii),cr,ci
        SV%c(ii) = cmplx(cr,ci)
    end do

    ! if grid is available, make sure the two are consistent
    if (present(grid)) then
        if (maxval(SV%i) > grid%NumberOfEdges) then
            write(0, *) 'sparse vector size does not match the grid in read_sparsevecc'
            return
        endif
    endif

    end subroutine read_sparsevecc

    !------------------------------------------------------------------------
    ! read_sparsevecr reads a sparse vector in a simple ASCII format; vector must match
    ! the input grid; file unit must already be available for reading.
    !------------------------------------------------------------------------
    subroutine read_sparsevecr(fid, SV, grid)

    integer,        intent(in)        :: fid
    type (sparsevecr_t), intent(inout)  :: SV
    type (Mesh_t), intent(in), optional :: grid

    !  local variables
    integer                           :: nCoeff
    character(80)                     :: FEType
    integer                           :: ii, istat
    real (kind(SV%c))                  :: cr

    read(fid,*,iostat=istat) nCoeff,FEType

    if(.not. SV%allocated) then
        call create_sparsevecr(nCoeff,SV,FEType)
    else
        call reall_sparsevecr(nCoeff,SV)
        SV%FEType = FEType
    endif

    do ii = 1,nCoeff
        ! makes the acceptable formatting more flexible
        read(fid,*,iostat=istat) SV%i(ii),SV%xyz(ii),cr
        SV%c(ii) = cr
    end do

    ! if grid is available, make sure the two are consistent
    if (present(grid)) then
        if(FEType == EDGE)then
            if (maxval(SV%i) > grid%NumberOfEdges) then
                write(0, *) 'sparse vector size does not match the grid in read_sparsevecr'
                return
            endif
        else if(FEType == Node)then
            if (maxval(SV%i) > grid%NumberOfNodes) then
                write(0, *) 'sparse vector size does not match the grid in read_sparsevecr'
                return
            endif
        end if
    endif

    end subroutine read_sparsevecr

    !------------------------------------------------------------------------
    ! this will copy a sparse complex vector from SV1 to SV2 ...
    ! interface to =
    ! basically like copy commands for vectors, scalars, BC
    ! check for size consistency (nCoeff), reallocate output if needed
    ! note that before allocation nCoeff = 0
    ! Remember, SV2 = SV1
    !------------------------------------------------------------------------
    subroutine copy_sparsevecc(SV2,SV1)

    implicit none
    type (sparsevecc_t), intent(in)       :: SV1
    type (sparsevecc_t), intent(inout)        :: SV2

    ! check to see if RHS (SV1) is active (allocated)
    if(.not. SV1%allocated) then
        write(0,*) 'RHS not allocated yet for copy_sparsevecc'
        return
    end if

    ! allocate output if needed, otherwise check for consistency
    if(.not. SV2%allocated) then
        call create_sparsevecc(SV1%nCoeff, SV2, SV1%FEType)
    elseif(SV1%nCoeff .ne. SV2%nCoeff) then
        ! internal memory allocation is strongly discouraged. But this
        ! is an exception
        call deall_sparsevecc(SV2)
        ! ... now allocate for correct number of components
        call create_sparsevecc(SV1%nCoeff, SV2, SV1%FEType)
    end if

    ! happen to have the same specs
    if (SV1%FEType == SV2%FEType) then

        ! just copy the components
        SV2%i = SV1%i
        SV2%xyz = SV1%xyz
        SV2%c = SV1%c

    else
        write (0, *) 'not compatible usage for copy_sparsevecc'

    end if

    if(SV1%temporary) then
        call deall_sparsevecc(SV1)
    end if

    end subroutine copy_sparsevecc

    !------------------------------------------------------------------------
    ! this will copy a sparse real vector from SV1 to SV2 ...
    ! interface to =
    ! basically like copy commands for vectors, scalars, BC
    ! check for size consistency (nCoeff), reallocate output if needed
    ! note that before allocation nCoeff = 0
    ! Remember, SV2 = SV1
    !------------------------------------------------------------------------
    subroutine copy_sparsevecr(SV2,SV1)

    implicit none
    type (sparsevecr_t), intent(in)       :: SV1
    type (sparsevecr_t), intent(inout)        :: SV2

    ! check to see if RHS (SV1) is active (allocated)
    if(.not. SV1%allocated) then
        write(0,*) 'RHS not allocated yet for copy_sparsevecr'
        return
    end if

    ! allocate output if needed, otherwise check for consistency
    if(.not. SV2%allocated) then
        call create_sparsevecr(SV1%nCoeff, SV2, SV1%FEType)
    elseif(SV1%nCoeff .ne. SV2%nCoeff) then
        ! internal memory allocation is strongly discouraged. But this
        ! is an exception
        call deall_sparsevecr(SV2)
        ! ... now allocate for correct number of components
        call create_sparsevecr(SV1%nCoeff, SV2, SV1%FEType)
    end if

    ! happen to have the same specs
    if (SV1%FEType == SV2%FEType) then

        ! just copy the components
        SV2%i = SV1%i
        SV2%xyz = SV1%xyz
        SV2%c = SV1%c

    else
        write (0, *) 'not compatible usage for copy_sparsevecc'

    end if

    if(SV1%temporary) then
        call deall_sparsevecr(SV1)
    end if

    end subroutine copy_sparsevecr

    !------------------------------------------------------------------------
    ! linear combination of two sparse vectors, output as a sparse vector
    ! allocates (or reallocates) output sparse vector Loc3
    !------------------------------------------------------------------------
    subroutine linComb_sparsevecc(Lic1,ic1,Lic2,ic2,Loc3)

    implicit none
    type (sparsevecc_t), intent(in)       :: Lic1,Lic2
    type (sparsevecc_t), intent(inout)        :: Loc3
    complex (kind=8), intent(in)        :: ic1,ic2

    integer                 :: n,m,nm
    integer                 :: nCoeffSum
    integer, allocatable, dimension(:)      :: Lic1oldIndex
    integer                             :: status

    allocate(Lic1oldIndex(Lic2%nCoeff), STAT = status)

    ! it all depends on how many nodes are common
    if(Loc3%allocated) then
        deallocate(Loc3%i, STAT = status)
        deallocate(Loc3%xyz, STAT = status)
        deallocate(Loc3%c, STAT = status)
        Loc3%FEType = ''
        Loc3%allocated = .false.
    endif

    if (Lic1%FEType == Lic2%FEType) then

        ! count common indices
        nCoeffSum = Lic1%nCoeff+Lic2%nCoeff
        do m = 1,Lic2%nCoeff
            Lic1oldIndex(m) = 0
            do n = 1,Lic1%nCoeff

                if((Lic1%xyz(n).eq.Lic2%xyz(m)).and.(Lic1%i(n).eq.Lic2%i(m))) then
                    nCoeffSum = nCoeffSum-1
                    Lic1oldIndex(m) = n
                    exit
                endif

            enddo
        enddo

    else
        write (0, *) 'not compatible usage for LinCompSparseVecC'
    end if

    Call create_sparsevecc(nCoeffsum, Loc3, Lic1%FEType)
    nm = Lic1%nCoeff
    Loc3%i(1:nm) = Lic1%i
    Loc3%xyz(1:nm) = Lic1%xyz
    Loc3%c(1:nm) = ic1*Lic1%c

    do m = 1,Lic2%nCoeff
        ! if none of them are common, just concatenate
        if(Lic1oldIndex(m).eq.0) then
            nm = nm+1
            Loc3%i(nm) = Lic2%i(m)
            Loc3%xyz(nm) = Lic2%xyz(m)
            Loc3%c(nm) = ic2*Lic2%c(m)
        else
            Loc3%c(Lic1oldIndex(m)) =  Loc3%c(Lic1oldIndex(m))+ic2*Lic2%c(m)
        endif
    enddo

    deallocate(Lic1oldIndex, STAT = status)

    end subroutine linComb_sparsevecc

    !------------------------------------------------------------------------
    ! linear combination of two sparse vectors, output as a sparse vector
    ! allocates (or reallocates) output sparse vector Loc3
    !------------------------------------------------------------------------
    subroutine linComb_sparsevecr(Lic1,ic1,Lic2,ic2,Loc3)

    implicit none
    type (sparsevecr_t), intent(in)       :: Lic1,Lic2
    type (sparsevecr_t), intent(inout)        :: Loc3
    real (kind=8), intent(in)        :: ic1,ic2

    integer                 :: n,m,nm
    integer                 :: nCoeffSum
    integer, allocatable, dimension(:)      :: Lic1oldIndex
    integer                             :: status

    allocate(Lic1oldIndex(Lic2%nCoeff), STAT = status)

    ! it all depends on how many nodes are common
    if(Loc3%allocated) then
        deallocate(Loc3%i, STAT = status)
        deallocate(Loc3%xyz, STAT = status)
        deallocate(Loc3%c, STAT = status)
        Loc3%FEType = ''
        Loc3%allocated = .false.
    endif

    if (Lic1%FEType == Lic2%FEType) then

        ! count common indices
        nCoeffSum = Lic1%nCoeff+Lic2%nCoeff
        do m = 1,Lic2%nCoeff
            Lic1oldIndex(m) = 0
            do n = 1,Lic1%nCoeff

                if((Lic1%xyz(n).eq.Lic2%xyz(m)).and.(Lic1%i(n).eq.Lic2%i(m))) then
                    nCoeffSum = nCoeffSum-1
                    Lic1oldIndex(m) = n
                    exit
                endif

            enddo
        enddo

    else
        write (0, *) 'not compatible usage for LinCompSparseVecC'
    end if

    Call create_sparsevecr(nCoeffsum, Loc3, Lic1%FEType)
    nm = Lic1%nCoeff
    Loc3%i(1:nm) = Lic1%i
    Loc3%xyz(1:nm) = Lic1%xyz
    Loc3%c(1:nm) = ic1*Lic1%c

    do m = 1,Lic2%nCoeff
        ! if none of them are common, just concatenate
        if(Lic1oldIndex(m).eq.0) then
            nm = nm+1
            Loc3%i(nm) = Lic2%i(m)
            Loc3%xyz(nm) = Lic2%xyz(m)
            Loc3%c(nm) = ic2*Lic2%c(m)
        else
            Loc3%c(Lic1oldIndex(m)) =  Loc3%c(Lic1oldIndex(m))+ic2*Lic2%c(m)
        endif
    enddo

    deallocate(Lic1oldIndex, STAT = status)

    end subroutine linComb_sparsevecr

    !------------------------------------------------------------------------
    ! multiply a sparse complex vector SV1 by a complex value to get SV2.
    ! Sometimes computing a full linear combination by linComb_sparsevecc
    ! is too much work for this simple need... SV2 = ic * SV1
    ! output SV2 can overwrite input SV1
    ! allocates for output if necessary
    !------------------------------------------------------------------------
    subroutine scMult_sparsevecc(cs,SV1,SV2)

    implicit none
    complex(kind=8), intent(in)     :: cs
    type (sparsevecc_t), intent(in)       :: SV1
    type (sparsevecc_t), intent(inout)    :: SV2

    !  make sure SV2 is allocated and of the correct size
    if(SV2%allocated) then
        if(SV2%FEType .ne. SV1%FEType) then
            write(0,*) 'not compatible usage for scMult_sparsevecc'
            return
        elseif(SV2%nCoeff .ne. SV1%nCoeff) then
            call deall_sparsevecc(SV2)
            call create_sparsevecc(SV1%nCoeff,SV2,SV1%FEType)
        endif
    else
        call create_sparsevecc(SV1%nCoeff,SV2,SV1%FEType)
    endif

    SV2%i = SV1%i
    SV2%xyz = SV1%xyz
    SV2%c = cs * SV1%c

    end subroutine scMult_sparsevecc

    !------------------------------------------------------------------------
    ! multiply a sparse real vector SV1 by a real value to get SV2.
    ! Sometimes computing a full linear combination by linComb_sparsevecr
    ! is too much work for this simple need... SV2 = ic * SV1
    ! output SV2 can overwrite input SV1
    ! allocates for output if necessary
    !------------------------------------------------------------------------
    subroutine scMult_sparsevecr(cs,SV1,SV2)

    implicit none
    real(kind=8), intent(in)     :: cs
    type (sparsevecr_t), intent(in)       :: SV1
    type (sparsevecr_t), intent(inout)    :: SV2

    !  make sure SV2 is allocated and of the correct size
    if(SV2%allocated) then
        if(SV2%FEType .ne. SV1%FEType) then
            write(0,*) 'not compatible usage for scMult_sparsevecc'
            return
        elseif(SV2%nCoeff .ne. SV1%nCoeff) then
            call deall_sparsevecr(SV2)
            call create_sparsevecr(SV1%nCoeff,SV2,SV1%FEType)
        endif
    else
        call create_sparsevecr(SV1%nCoeff,SV2,SV1%FEType)
    endif

    SV2%i = SV1%i
    SV2%xyz = SV1%xyz
    SV2%c = cs * SV1%c

    end subroutine scMult_sparsevecr

    !------------------------------------------------------------------------
    ! compute complex dot product between a sparse vector SV and a vector of
    ! type cvector_t ... result in c
    !------------------------------------------------------------------------
    function dotProd_scvector_f(SV,V) result(c)

    implicit none
    type (sparsevecc_t), intent(in)       :: SV
    type (cvector_t), intent(in)          :: V
    complex(kind=8)             :: c
    integer                 :: i
    integer                 :: xi

    c = C_ZERO

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProd_scvector_f'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for dotProd_scvector_f'
        return
    endif

    ! sum over  non-zero terms in sparse vector (conjugate sparse)
    ! (need to check xyz the component)
    ! Remember, xyz = 1,2,3 refers to x, y or z components
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with datum
            xi = SV%i(i)

            c = c + conjg(SV%c(i)) * V%datum(xi)

        else
            write(0,*) 'I out of bounds for dotProd_scvector_f'
            return
        endif

    enddo

    end function dotProd_scvector_f

    !------------------------------------------------------------------------
    ! compute real dot product between a sparse vector SV and a vector of
    ! type rvector_t ... result in c
    !------------------------------------------------------------------------
    function dotProd_srvector_f(SV,V) result(c)

    implicit none
    type (sparsevecr_t), intent(in)       :: SV
    type (rvector_t), intent(in)          :: V
    real(kind=8)             :: c
    integer                 :: i
    integer                 :: xi

    c = R_ZERO

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProd_srvector_f'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for dotProd_srvector_f'
        return
    endif

    ! sum over  non-zero terms in sparse vector (conjugate sparse)
    ! (need to check xyz the component)
    ! Remember, xyz = 1,2,3 refers to x, y or z components
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with datum
            xi = SV%i(i)

            c = c + SV%c(i) * V%datum(xi)

        else
            write(0,*) 'I out of bounds for dotProd_srvector_f'
            return
        endif

    enddo

    end function dotProd_srvector_f

    !------------------------------------------------------------------------
    ! compute complex dot product between a sparse vector SV and a vector of
    ! type cvector_t ... result in c
    !------------------------------------------------------------------------
    function dotProd_csvector_f(V,SV) result(c)

    implicit none
    type (sparsevecc_t), intent(in)       :: SV
    type (cvector_t), intent(in)          :: V
    complex(kind=8)             :: c
    integer                 :: i
    integer                 :: xi

    c = C_ZERO

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProd_csvector_f'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for dotProd_csvector_f'
        return
    endif

    ! sum over  non-zero terms in sparse vector (conjugate full)
    ! (need to check xyz the component)
    ! Remember, xyz = 1,2,3 refers to x, y or z components
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with datum
            xi = SV%i(i)
            c = c + SV%c(i) * conjg(V%datum(xi))

        else
            write(0,*) 'IJK out of bounds for dotProd_csvector_f'
            return
        endif

    enddo

    end function dotProd_csvector_f

    !------------------------------------------------------------------------
    ! compute real dot product between a sparse vector SV and a vector of
    ! type rvector_t ... result in c
    !------------------------------------------------------------------------
    function dotProd_rsvector_f(V,SV) result(c)

    implicit none
    type (sparsevecr_t), intent(in)       :: SV
    type (rvector_t), intent(in)          :: V
    real(kind=8)             :: c
    integer                 :: i
    integer                 :: xi

    c = R_ZERO

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProd_rsvector_f'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for dotProd_rsvector_f'
        return
    endif

    ! sum over  non-zero terms in sparse vector (conjugate full)
    ! (need to check xyz the component)
    ! Remember, xyz = 1,2,3 refers to x, y or z components
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with datum
            xi = SV%i(i)
            c = c + SV%c(i) * V%datum(xi)

        else
            write(0,*) 'IJK out of bounds for dotProd_rsvector_f'
            return
        endif

    enddo

    end function dotProd_rsvector_f

    !------------------------------------------------------------------------
    ! compute complex dot product between a sparse vector SV and a vector of
    ! type cvector_t ... result in c
    !   FOR THIS VERSION FIRST VECTOR IS NOT CONJUGATED
    !------------------------------------------------------------------------
    function dotProd_noConj_scvector_f(SV,V) result(c)

    implicit none
    type (sparsevecc_t), intent(in)       :: SV
    type (cvector_t), intent(in)          :: V
    complex(kind=8)             :: c
    integer                 :: i
    integer                 :: xi

    c = C_ZERO

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProd_scvector_f'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for dotProd_scvector_f'
        return
    endif

    ! sum over  non-zero terms in sparse vector
    ! (need to check xyz the component)
    ! Remember, xyz = 1,2,3 refers to x, y or z components
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with datum
            xi = SV%i(i)
            c = c + SV%c(i) * V%datum(xi)


        else
            write(0,*) 'IJK out of bounds for dotProd_scvector_f'
            return
        endif

    enddo

    end function dotProd_noConj_scvector_f

    !------------------------------------------------------------------------
    ! compute complex dot product between a sparse vector SV and a vector of
    ! type cvector_t ... result in c
    !   FOR THIS VERSION FIRST VECTOR IS NOT CONJUGATED
    !------------------------------------------------------------------------
    function dotProd_noConj_csvector_f(V,SV) result(c)

    implicit none
    type (sparsevecc_t), intent(in)       :: SV
    type (cvector_t), intent(in)          :: V
    complex(kind=8)             :: c
    integer                 :: i
    integer                 :: xi

    c = C_ZERO

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProd_csvector_f'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for dotProd_csvector_f'
        return
    endif

    ! sum over  non-zero terms in sparse vector
    ! (need to check xyz the component)
    ! Remember, xyz = 1,2,3 refers to x, y or z components
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with datum
            xi = SV%i(i)
            c = c + SV%c(i) * V%datum(xi)

        else
            write(0,*) 'IJK out of bounds for dotProd_csvector_f'
            return
        endif

    enddo

    end function dotProd_noConj_csvector_f

    !------------------------------------------------------------------------
    ! compute sum cs*SV + V where SV is a complex sparse vector. V is a full
    ! complex vector of type cvector_t, and cs is a complex scalar; result
    ! overwrites V. Can also be used to construct a complex full vector
    ! from a sparse complex vector if cs = (1.0, 0.0) and V = 0 (initially)
    ! or copy from a sparse vector to a full vector
    !------------------------------------------------------------------------
    subroutine add_scvector(cs,SV,V)

    implicit none
    type (sparsevecc_t), intent(in)   :: SV
    type (cvector_t), intent(inout)   :: V
    complex(kind=8), intent(in)     :: cs
    integer             :: i
    integer             :: xi

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for add_scvector'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for add_scvector'
        return
    endif

    ! loop over non-zero terms in sparse vector, adding to
    ! corresponding terms in full vector
    ! (need to check component xyz ...)
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with x-components
            xi = SV%i(i)
            V%datum(xi) = cs*SV%c(i) + V%datum(xi)

        else
            write(0,*) 'IJK out of bounds for add_scvector'
            return
        endif

    enddo

    end subroutine add_scvector

    !------------------------------------------------------------------------
    ! compute sum cs*SV + V where SV is a real sparse vector. V is a full
    ! real vector of type rvector_t, and cs is a real scalar; result
    ! overwrites V. Can also be used to construct a real full vector
    ! from a sparse real vector if cs = 1.0 and V = 0 (initially)
    ! or copy from a sparse vector to a full vector
    !------------------------------------------------------------------------
    subroutine add_srvector(cs,SV,V)

    implicit none
    type (sparsevecr_t), intent(in)   :: SV
    type (rvector_t), intent(inout)   :: V
    real(kind=8), intent(in)     :: cs
    integer             :: i
    integer             :: xi

    if((.not.SV%allocated).or.(.not.V%allocated)) then
        write(0,*) 'RHS not allocated yet for add_scvector'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for add_scvector'
        return
    endif

    ! loop over non-zero terms in sparse vector, adding to
    ! corresponding terms in full vector
    ! (need to check component xyz ...)
    do i = 1,SV%nCoeff

        ! generic test for both edge and face (all the components)
        if (SV%i(i).le.V%nd) then

            ! dealing with x-components
            xi = SV%i(i)
            V%datum(xi) = cs*SV%c(i) + V%datum(xi)

        else
            write(0,*) 'IJK out of bounds for add_srvector'
            return
        endif

    enddo

    end subroutine add_srvector

    !------------------------------------------------------------------------
    ! this will conjugate a sparse complex vector SV1 (result SV2)
    ! A.K.
    !------------------------------------------------------------------------
    function conjg_sparsevecc_f(SV1) result (SV2)

    type (sparsevecc_t), intent(in)       :: SV1
    type (sparsevecc_t)                   :: SV2

    ! check to see if SV1 is active (allocated)
    if(.not.SV1%allocated) then
        write(0,*) 'Input sparse vector not allocated yet for conjg_sparsevecc_f'
        return
    endif

    !  make sure SV2 is allocated and of the correct size
    if(SV2%allocated) then
        if ((SV2%FEType .ne. SV1%FEType) .or. (SV2%nCoeff .ne. SV1%nCoeff)) then
            call deall_sparsevecc(SV2)
            call create_sparsevecc(SV1%nCoeff,SV2,SV1%FEType)
        endif
    else
        call create_sparsevecc(SV1%nCoeff,SV2,SV1%FEType)
    endif

    SV2%i = SV1%i
    SV2%xyz = SV1%xyz
    SV2%c = conjg(SV1%c)

    SV2%temporary = .true.

    end function conjg_sparsevecc_f

    !------------------------------------------------------------------------
    ! subroutine to fill a single entry in the sparse vector with a value
    ! from full vector; sparsevecc_t has to be pre-allocated
    ! A.K.
    !------------------------------------------------------------------------
    subroutine newValueC_sparsevecc(SV,index,c,i,xyz)

    type (sparsevecc_t), intent(inout)                :: SV
    complex(kind=8), intent(in)                  :: c
    integer, intent(in)                             :: i,xyz,index

    if (.not.SV%allocated) then
        write (0, *) 'Sparse vector in newValueC_sparsevecc is not allocated yet'
        return
    end if

    if (index.gt.SV%nCoeff) then !ubound(SV%c)
        write (0, *) 'The chosen index in newValueC_sparsevecc is not allocated yet'
        return
    end if

    SV%i(index) = i
    SV%xyz(index) = xyz
    SV%c(index) = c

    end subroutine newValueC_sparsevecc

    !------------------------------------------------------------------------
    ! A subroutine to fill a single entry in the sparse vector with a value
    ! from full vector; sparsevecc_t has to be pre-allocated
    ! A.K.
    !------------------------------------------------------------------------
    subroutine newValueR_sparsevecc(SV,index,r,i,xyz)

    type (sparsevecc_t), intent(inout)                :: SV
    real(kind=8), intent(in)                        :: r
    integer, intent(in)                             :: i,xyz,index

    if (.not.SV%allocated) then
        write (0, *) 'Sparse vector in newValueR_sparsevecc is not allocated yet'
        return
    end if

    if (index.gt.SV%nCoeff) then
        write (0, *) 'The chosen index in newValueR_sparsevecc is not allocated yet'
        return
    end if

    SV%i(index) = i
    SV%xyz(index) = xyz
    SV%c(index) = dcmplx(r,0.0d0)

    end subroutine newValueR_sparsevecc

    !------------------------------------------------------------------------
    ! subroutine to fill a single entry in the sparse vector with a value
    ! from full vector; sparsevecr_t has to be pre-allocated
    ! A.K.
    !------------------------------------------------------------------------
    subroutine newValueR_sparsevecr(SV,index,c,i,xyz)

    type (sparsevecr_t), intent(inout)                :: SV
    real(kind=8), intent(in)                  :: c
    integer, intent(in)                             :: i,xyz,index

    if (.not.SV%allocated) then
        write (0, *) 'Sparse vector in newValueR_sparsevecr is not allocated yet'
        return
    end if

    if (index.gt.SV%nCoeff) then !ubound(SV%c)
        write (0, *) 'The chosen index in newValueR_sparsevecr is not allocated yet'
        return
    end if

    SV%i(index) = i
    SV%xyz(index) = xyz
    SV%c(index) = c

    end subroutine newValueR_sparsevecr

    !------------------------------------------------------------------------
    ! A subroutine to fill a single entry in the sparse vector with a value
    ! from full vector; sparsevecc_t has to be pre-allocated
    ! A.K.
    !------------------------------------------------------------------------
    subroutine copyValue_csvector(SV,index,V,i,xyz)

    type (sparsevecc_t), intent(inout)                :: SV
    type (cvector_t), intent(in)                      :: V
    integer, intent(in)             :: i,xyz,index

    if (.not.SV%allocated) then
        write (0, *) 'Sparse vector in copyValue_csvector is not allocated yet'
        return
    end if

    if (index.gt.SV%nCoeff) then
        write (0, *) 'The chosen index in copyValue_csvector is not allocated yet'
        return
    end if

    SV%i(index) = i
    SV%xyz(index) = xyz

    SV%c(index) = V%datum(i)


    end subroutine copyValue_csvector

    !------------------------------------------------------------------------
    ! A subroutine to fill a single entry in the sparse vector with a value
    ! from full vector; sparsevecc_t has to be pre-allocated
    ! A.K.
    !------------------------------------------------------------------------
    subroutine copyValue_rsvector(SV,index,V,i,xyz)

    type (sparsevecr_t), intent(inout)                :: SV
    type (rvector_t), intent(in)                      :: V
    integer, intent(in)             :: i,xyz,index

    if (.not.SV%allocated) then
        write (0, *) 'Sparse vector in copyValue_rsvector is not allocated yet'
        return
    end if

    if (index.gt.SV%nCoeff) then
        write (0, *) 'The chosen index in copyValue_rsvector is not allocated yet'
        return
    end if

    SV%i(index) = i
    SV%xyz(index) = xyz

    SV%c(index) = V%datum(i)


    end subroutine copyValue_rsvector

    !------------------------------------------------------------------------
    ! copy from a full vector to a sparse vector, this routine has quite a
    ! limited functionality as it assumes one knows the total number of
    ! non-zero vectors in full vector description
    !------------------------------------------------------------------------
    subroutine copy_csvector(SV,V)

    implicit none
    type (sparsevecc_t), intent(inout)    :: SV
    type (cvector_t), target, intent(in)  :: V
    integer             :: i
    integer             :: xi

    if(.not.V%allocated) then
        write(0,*) 'RHS not allocated yet for copy_csvector'
        return
    endif

    if(.not.SV%allocated) then
        write(0,*) 'LHS not allocated yet for copy_csvector'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for copy_csvector'
        return
    endif

    if (V%FEType == EDGE .or. V%FEType == FACE) then
        i = 0
        ! for x - component
        do xi = 1, V%nd

            if (V%datum(xi) /= 0.0d0) then
                i = i + 1
                if (i > SV%nCoeff) then
                    write(0, *) 'outside sparse vector nCoeff: copy_csvector'
                    return
                end if
                SV%xyz(i) = 1
                SV%i(i) = xi
                SV%c(i) = V%datum(xi)
            end if

        end do

    else

        write (0, *) 'Vector (full) use not proper in copy_csvector'

    end if

    end subroutine copy_csvector

    !------------------------------------------------------------------------
    ! copy from a full vector to a sparse vector, this routine has quite a
    ! limited functionality as it assumes one knows the total number of
    ! non-zero vectors in full vector description
    !------------------------------------------------------------------------
    subroutine copy_rsvector(SV,V)

    implicit none
    type (sparsevecr_t), intent(inout)    :: SV
    type (rvector_t), target, intent(in)  :: V
    integer             :: i
    integer             :: xi

    if(.not.V%allocated) then
        write(0,*) 'RHS not allocated yet for copy_rsvector'
        return
    endif

    if(.not.SV%allocated) then
        write(0,*) 'LHS not allocated yet for copy_rsvector'
        return
    endif

    if (SV%FEType /= V%FEType) then
        write(0,*) 'not compatible usage for copy_rsvector'
        return
    endif

    if (V%FEType == EDGE .or. V%FEType == FACE) then
        i = 0
        ! for x - component
        do xi = 1, V%nd

            if (V%datum(xi) /= 0.0d0) then
                i = i + 1
                if (i > SV%nCoeff) then
                    write(0, *) 'outside sparse vector nCoeff: copy_rsvector'
                    return
                end if
                SV%xyz(i) = 1
                SV%i(i) = xi
                SV%c(i) = V%datum(xi)
            end if

        end do

    else

        write (0, *) 'Vector (full) use not proper in copy_rsvector'

    end if

    end subroutine copy_rsvector

    !------------------------------------------------------------------------
    end module
