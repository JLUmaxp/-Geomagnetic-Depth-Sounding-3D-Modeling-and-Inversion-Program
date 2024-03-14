    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    !  Modified from ModEM by Gary Egbert and Anna Kelbert.
    !***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****!
    module VectorMod
    !------------------------------------------------------------------------
    ! This module creates data types for vector or scalar fields defined on
    ! the edges or nodes of a tetrahedron grid, along with basic
    ! algebraic (vector space) operations. Example of these basic operations
    ! are allocation, deallocation, intialization, copying, and algebriac
    ! operations (linear combinations, scalar products, dot products).
    ! Not specific to EM problem, no dependency on outside (from other classes) modules.
    !------------------------------------------------------------------------
    use ParameterMod        ! math/ physics constants
    use UtilitiesMod        ! for error and warning messages
    !------------------------------------------------------------------------
    implicit none
    !------------------------------------------------------------------------
    ! Important - overloading the '=' assignment
    !------------------------------------------------------------------------
    INTERFACE ASSIGNMENT (=)
    MODULE PROCEDURE copy_cvector
    MODULE PROCEDURE copy_rvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! Generic interfaces are done through subroutines
    ! creates edge/node nodes
    !------------------------------------------------------------------------
    INTERFACE create
    module procedure create_rvector
    module procedure create_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! deallocates the edge/node nodes
    !------------------------------------------------------------------------
    INTERFACE deall
    module procedure deall_rvector
    module procedure deall_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! set the values to zero
    !------------------------------------------------------------------------
    INTERFACE zero
    module procedure zero_rvector
    module procedure zero_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! compatibility test
    !------------------------------------------------------------------------
    INTERFACE compare
    module procedure compare_rvector_f
    module procedure compare_cvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    ! scalar value multiplies the edge/node nodes
    !------------------------------------------------------------------------
    INTERFACE scMult
    module procedure scMult_rvector
    module procedure scMult_cvector
    module procedure scMultReal_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE scMultAdd
    module procedure scMultAdd_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE linComb
    module procedure linComb_rvector
    module procedure linComb_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! adds the edge/node nodes
    !------------------------------------------------------------------------
    INTERFACE add
    module procedure add_rvector
    module procedure add_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! subtracts the edge/node nodes
    !------------------------------------------------------------------------
    INTERFACE subtract
    module procedure subtract_rvector
    module procedure subtract_cvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! pointwise vector (two vector data types) multiplication of edge/node
    ! nodes
    ! and pointwise real-complex (mixed) multiplication of edge/node nodes
    ! Both are vector data types
    !------------------------------------------------------------------------
    INTERFACE diagMult
    module procedure diagMult_rvector
    module procedure diagMult_cvector
    module procedure diagMult_rcvector
    module procedure diagMult_crvector
    END INTERFACE
    !------------------------------------------------------------------------
    ! pointwise real-complex (mixed) division of edge/node nodes
    ! Both are vector data types
    !------------------------------------------------------------------------
    INTERFACE diagDiv
    module procedure diagDiv_rvector
    module procedure diagDiv_rcvector
    module procedure diagDiv_crvector
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE dotProd
    MODULE PROCEDURE dotProd_rvector_f
    MODULE PROCEDURE dotProd_cvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE dotProd_noConj
    MODULE PROCEDURE dotProd_noConj_cvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    ! overload some intrinsic functions for complex numbers
    !------------------------------------------------------------------------
    INTERFACE conjg
    MODULE PROCEDURE conjg_cvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE cmplx
    MODULE PROCEDURE cmplx_rvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE real
    MODULE PROCEDURE real_cvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    INTERFACE imag
    MODULE PROCEDURE imag_cvector_f
    END INTERFACE
    !------------------------------------------------------------------------
    ! Interface operators are done through functions
    !  INTERFACE OPERATOR (+)
    !     MODULE PROCEDURE add_rvector_f
    !     MODULE PROCEDURE add_cvector_f
    !  END INTERFACE
    !
    !  INTERFACE OPERATOR (-)
    !     MODULE PROCEDURE subtract_rvector_f
    !     MODULE PROCEDURE subtract_cvector_f
    !  END INTERFACE
    !
    !  INTERFACE OPERATOR (*)
    !     MODULE PROCEDURE scMult_rvector_f
    !     MODULE PROCEDURE scMult_cvector_f
    !     MODULE PROCEDURE scMultReal_cvector_f
    !     MODULE PROCEDURE diagMult_rvector_f
    !     MODULE PROCEDURE diagMult_cvector_f
    !     MODULE PROCEDURE diagMult_rcvector_f
    !     MODULE PROCEDURE diagMult_crvector_f
    !  END INTERFACE
    !
    !  INTERFACE OPERATOR (/)
    !     MODULE PROCEDURE diagDiv_rcvector_f
    !     MODULE PROCEDURE diagDiv_crvector_f
    !  END INTERFACE
    !------------------------------------------------------------------------
    public :: create_rvector,  create_cvector
    public :: deall_rvector, deall_cvector
    public :: write_rvector, write_cvector
    public :: read_rvector, read_cvector
    public :: copy_rvector, copy_cvector
    public :: zero_rvector, zero_cvector
    public :: compare_rvector_f, compare_cvector_f
    public :: scMult_rvector, scMult_cvector, scMultReal_cvector
    public :: scMult_rvector_f, scMult_cvector_f, scMultReal_cvector_f
    public :: add_rvector, add_cvector
    public :: add_rvector_f, add_cvector_f
    public :: subtract_rvector, subtract_cvector
    public :: subtract_rvector_f, subtract_cvector_f
    public :: diagMult_rvector, diagMult_cvector
    public :: diagMult_rvector_f, diagMult_cvector_f
    public :: diagMult_rcvector, diagMult_crvector
    public :: diagMult_rcvector_f, diagMult_crvector_f
    public :: diagDiv_rcvector, diagDiv_crvector
    public :: diagDiv_rcvector_f, diagDiv_crvector_f
    public :: dotProd_rvector_f, dotProd_cvector_f
    public :: linComb_cvector, linComb_rvector, scMultAdd_cvector, conjg_cvector_f
    public :: cmplx_rvector_f, real_cvector_f, imag_cvector_f


    !------------------------------------------------------------------------
    ! type vector defines vector for edge in a tetrahedron grid as
    ! a complex field
    !------------------------------------------------------------------------
    type :: cvector_t

        ! store the intention of the use in a character string defined
        ! in GridDef as a parameter: EDGE or NODE are two possibilities
        character (len=80)                               :: FEType

        ! Typical usage:  electrical fields on cell edges of
        ! tetrahedron grid
        ! For example, in an EDGE, the dimensions would be
        ! x: edge nodes in x-direction: dimension Nx, Ny+1, Nz+1
        ! y: edge nodes in y-direction: dimension Nx+1, Ny, Nz+1
        ! z: edge nodes in z-direction: dimension Nx+1, Ny+1, Nz
        ! Note that the arrays are defined through dynamic memory allocation
        complex(kind=8), allocatable, dimension(:)  :: datum

        ! Grid Dimensions:
        ! nx is grid dimension (number of cells) in the x-direction
        ! ny is grid dimension (number of cells) in the y-direction
        ! nz is grid dimension (number of cells) in the z-direction:
        integer                                          :: nd = 0

        ! allocated:  .true.  x, y, z arrays have been allocated
        logical                                   :: allocated = .false.

        ! temporary:  .true. for function outputs only; necessary to avoid memory leaks
        ! (probably will not be needed in the future when compilers will support
        ! ISO/IEC 15581 - the "allocatable array extension")
        logical                                     :: temporary = .false.

    end type cvector_t

    !------------------------------------------------------------------------
    ! type vector defines vector for either edge or node in a tetrahedron grid as
    ! a real field
    !------------------------------------------------------------------------
    type :: rvector_t

        ! store the intention of the use in a character string defined
        ! in GridDef as a parameter: EDGE or NODE are two possibilities
        character (len=80)                                :: FEType

        ! Typical usage:  conductivity averaged on cell edges of
        ! tetrahedron grid
        ! x: edge nodes in x-direction: dimension Nx, Ny+1, Nz+1
        ! y: edge nodes in y-direction: dimension Nx+1, Ny, Nz+1
        ! z: edge nodes in z-direction: dimension Nx+1, Ny+1, Nz
        ! Note that the arrays are defined through dynamic memory allocation
        real(kind=8), allocatable, dimension(:)      :: datum

        ! Grid Dimensions:
        ! nx is grid dimension (number of cells) in the x-direction
        ! ny is grid dimension (number of cells) in the y-direction
        ! nz is grid dimension (number of cells) in the z-direction:
        integer                                          :: nd = 0


        ! allocated:  .true.  x, y, z arrays have been allocated
        logical                                   :: allocated = .false.

        ! temporary:  .true. for function outputs only; necessary to avoid memory leaks
        ! (probably will not be needed in the future when compilers will support
        ! ISO/IEC 15581 - the "allocatable array extension")
        logical                                     :: temporary = .false.


    end type rvector_t

    Contains
    ! CREATE GRID_edge/node VECTORS
    ! * subroutine create_rvector(igrid, E, FEType)
    ! * subroutine create_cvector(igrid, E, FEType)

    ! DEALLOCATE GRID_edge/node VECTORS
    ! * subroutine deall_rvector(E)
    ! * subroutine deall_cvector(E)

    ! COPY GRID_edge/node VECTORS :  (=)
    ! * subroutine copy_rvector(E2,E1)
    ! * subroutine copy_cvector(E2,E1)

    ! ZERO GRID_edge/node VECTORS
    ! * subroutine zero_rvector(E)
    ! * subroutine zero_cvector(E)

    ! SCALAR MULTIPLICATION:
    ! * subroutine scMult_rvector(c, E1, E2)
    ! * subroutine scMult_cvector(c, E1, E2)
    ! * subroutine scMultReal_cvector(c, E1, E2)

    ! SCALAR MULTIPLICATION: FUNCTION VERSION (c*E)
    ! * function scMult_rvector_f(c, E1) result(E2)
    ! * function scMult_cvector_f(c, E1) result(E2)
    ! * function scMultReal_cvector_f(c, E1) result(E2)

    ! VECTOR SUM:
    ! * subroutine add_rvector(E1, E2, E3)
    ! * subroutine add_cvector(E1, E2, E3)

    ! VECTOR SUM: FUNCTION VERSION (E3 = E1+E2)
    ! * function add_rvector_f(E1, E2) result(E3)
    ! * function add_cvector_f(E1, E2) result(E3)

    ! VECTOR SUBTRACT:
    ! * subroutine subtract_rvector(E1, E2, E3)
    ! * subroutine subtract_cvector(E1, E2, E3)

    ! VECTOR SUBTRACT: FUNCTION VERSION (E3 = E1-E2)
    ! * function subtract_rvector_f(E1, E2) result(E3)
    ! * function subtract_cvector_f(E1, E2) result(E3)

    ! POINTWISE MULTIPLICATION OF VECTORS:
    ! * subroutine diagMult_rvector(E1, E2, E3)
    ! * subroutine diagMult_cvector(E1, E2, E3)

    ! POINTWISE MULTIPLICATION OF VECTORS: FUNCTION VERSION (c*E)
    ! * function diagMult_rvector_f(E1, E2) result(E3)
    ! * function diagMult_cvector_f(E1, E2) result(E3)

    ! POINTWISE MULTIPLICATION OF VECTORS and SCALARS:
    ! one combination is real and complex vectors and another is complex
    ! and real vectors as the sequence for inputs
    ! * subroutine diagMult_crvector(E1, E2, E3)
    ! * subroutine diagMult_rcvector(E1, E2, E3)

    ! POINTWISE MULTIPLICATION OF VECTORS and SCALARS: FUNCTION VERSION (c*E)
    ! one combination is real and complex vectors and another is complex
    ! and real vectors as the sequence for inputs
    ! * function diagMult_crvector_f(E1, E2) result(E3)
    ! * function diagMult_rcvector_f(E1, E2) result(E3)

    ! POINTWISE DIVISION OF VECTORS and SCALARS:
    ! one combination is real and complex vectors and another is complex
    ! and real vectors as the sequence for inputs
    ! * subroutine diagDiv_crvector(E1, E2, E3)
    ! * subroutine diagDiv_rcvector(E1, E2, E3)

    ! POINTWISE DIVISION OF VECTORS and SCALARS: FUNCTION VERSION (c*E)
    ! one combination is real and complex vectors and another is complex
    ! and real vectors as the sequence for inputs
    ! * function diagDiv_crvector_f(E1, E2) result(E3)
    ! * function diagDiv_rcvector_f(E1, E2) result(E3)

    ! VECTOR DOT PRODUCT
    ! * function dotProd_rvector(E1, E2) result(r)
    ! * function dotProd_cvector(E1, E2) result(c)

    ! COMPLEX LINEAR COMBINATION ... formally redundent with above
    ! only doing ones that we are sure to want
    ! * subroutine linCom_cvector(inc1, E1, inc2, E2, E3)
    ! * subroutine scMultAdd_V_node(c, E1, E2)  (E2 = E2+c*E1)

    ! COMBINE REAL VECTORS TO PRODUCE A COMPLEX VECTOR, AND THE CONVERSE
    ! * function conjg_cvector(E1) result (E2)
    ! * function cmplx_rvector(E1, E2) result (E3)
    ! * function real_cvector(E1) result (E2)
    ! * function imag_cvector(E1) result (E2)

    ! The algebraic routines expect all input and output
    ! variables to be of the correct type, already allocated,
    ! and of the correct size.

    !------------------------------------------------------------------------
    ! create_rvector creates variable of derived type rvector_t,
    ! using grid definition in structure "grid" ;
    ! allocates memory in x,y,z component arrays
    ! FEType is a character string to describe intended usage
    !------------------------------------------------------------------------
    subroutine create_rvector(n, E, FEType)

    implicit none
    integer(kind=4), intent(in)    :: n
    ! the grid for which an edge/node node field is being initialized
    type (rvector_t), intent(inout)      :: E

    integer                            :: status,nd

    character (len=*), intent(in)     :: FEType

    ! First deallocate anything, that's allocated
    call deall_rvector(E)


    ! Grid dimensions
    nd = n

    E%nd = nd


    ! FEType
    E%FEType = FEType

    ! allocate memory for datum
    ! E%allocated will be true if all allocations succeed
    E%allocated = .true.
    if (E%FEType == EDGE .or. E%FEType == NODE .or. E%FEType == ELEMENT) then
        allocate(E%datum(nd), STAT=status)
        E%allocated = E%allocated .and. (status .EQ. 0)
    else
        write (0, *) 'not a known tag'
    end if

    if (E%allocated) then
        E%datum = R_ZERO
    else
        write (0, *) 'Warning: unable to allocate rvector_t - invalid grid supplied'
    end if

    end subroutine create_rvector  ! create_rvector

    !------------------------------------------------------------------------
    ! create_cvector creates variable of derived type cvector_t,
    ! using grid definition in structure "grid" ;
    ! allocates memory in x,y,z component arrays
    ! FEType is a character string to describe intended usage
    !------------------------------------------------------------------------
    subroutine create_cvector(n, E, FEType)

    implicit none
    integer(kind=4), intent(in)     :: n
    ! the grid for which an edge/node node field is being initialized
    type (cvector_t), intent(inout)       :: E

    integer                             :: status,nd

    character (len=*), intent(in)      :: FEType

    ! First deallocate anything, that's allocated
    call deall_cvector(E)


    ! Grid dimensions
    nd = n

    E%nd = nd

    ! print *, 'nx, ny, nz', nx, ny, nz

    ! FEType
    E%FEType = FEType

    ! allocate memory for x,y,z ;
    ! E%allocated will be true if all allocations succeed
    E%allocated = .true.
    if (E%FEType == EDGE .or. E%FEType == NODE.or. E%FEType == ELEMENT) then
        allocate(E%datum(nd), STAT=status)
        E%allocated = E%allocated .and. (status .EQ. 0)
    else
        write (0, *) 'not a known tag'
    end if

    if (E%allocated) then
        E%datum = C_ZERO
    else
        write (0, *) 'Warning: unable to allocate cvector_t - invalid grid supplied'
    end if

    end subroutine create_cvector  ! create_cvector

    !------------------------------------------------------------------------
    ! deall_rvector destoys variable of derived type rvector_t,
    ! deallocating memory
    !------------------------------------------------------------------------
    subroutine deall_rvector(E)

    implicit none
    type (rvector_t)  :: E
    integer     :: status

    ! deallocate memory for datum
    if (E%allocated) then
        deallocate(E%datum, STAT=status)
    end if


    E%nd = 0

    E%FEType = ''
    E%allocated = .false.

    end subroutine deall_rvector  ! deall_rvector

    !------------------------------------------------------------------------
    ! deall_cvector destoys variable of derived type cvector_t,
    ! deallocating memory
    !------------------------------------------------------------------------
    subroutine deall_cvector(E)

    implicit none
    type (cvector_t)  :: E
    integer     :: status

    ! deallocate memory for datum
    if (E%allocated) then
        deallocate(E%datum, STAT=status)
    end if


    E%nd = 0
    E%FEType = ''
    E%allocated = .false.

    end subroutine deall_cvector  ! deall_cvector

    !------------------------------------------------------------------------
    ! write_rvector writes an rvector_t in a simple format; rvector_t has
    ! to exist and be allocated before calling this routine, and the file unit
    ! must already be available for writing.
    ! optional file type may be 'ascii' or 'binary', shortcuts allowed;
    ! defaults to 'ascii'. use binary for better accuracy.
    !------------------------------------------------------------------------
    subroutine write_rvector(fid, E, ftype)

    integer,        intent(in)      :: fid
    type (rvector_t), intent(in)      :: E
    character(*), optional, intent(in):: ftype

    !  local variables
    integer                             :: Nd
    integer                           :: i, istat
    real (kind=8), allocatable, dimension(:)  :: datum
    logical                           :: ok, hasname, binary
    character(80)                     :: fname, isbinary

    if(.not. E%allocated) then
        write(0, *) 'rvector_t must be allocated before call to write_rvector'
        return
    endif

    if (.not. present(ftype)) then
        binary = .false.
    elseif (index(ftype,'b')>0) then
        binary = .true.
    else
        binary = .false.
    endif

    inquire(fid, opened=ok, named=hasname, name=fname, unformatted=isbinary)

    ! check that the file is unformatted if binary, formatted if ascii
    if ((index(isbinary,'yes')>0 .or. index(isbinary,'YES')>0) .and. .not. binary) then
        write(0,*) 'Warning: Unable to write rvector_t to unformatted file ',trim(fname)
    elseif ((index(isbinary,'no')>0 .or. index(isbinary,'NO')>0) .and. binary) then
        write(0,*) 'Warning: Unable to write rvector_t to formatted file ',trim(fname)
    endif

    ! write binary to unformatted files
    if (binary) then
        write(fid) E%nd,E%FEType
        write(fid) E%datum
        return
    end if

    ! otherwise, write ascii
    write(fid,'(3i5,a10)',iostat=istat) E%nd,trim(E%FEType)

    Nd = E%nd

    allocate(datum(Nd),STAT=istat)
    datum = R_ZERO

    if (E%FEType == EDGE .or. E%FEType == NODE.or. E%FEType == ELEMENT) then
        datum = E%datum
    else
        write (0, *) 'not a known tag'
    end if


    do i = 1,Nd
        write(fid,'(i5,es14.6)',iostat=istat) i,datum(i)
    end do


    deallocate(datum,STAT=istat)

    end subroutine write_rvector

    !------------------------------------------------------------------------
    ! write_cvector writes a cvector_t in a simple format; cvector_t has
    ! to exist and be allocated before calling this routine, and the file unit
    ! must already be available for writing.
    ! optional file type may be 'ascii' or 'binary', shortcuts allowed;
    ! defaults to 'ascii'. use binary for better accuracy.
    !------------------------------------------------------------------------
    subroutine write_cvector(fid, E, ftype)

    integer,        intent(in)      :: fid
    type (cvector_t), intent(in)      :: E
    character(*), optional, intent(in):: ftype

    !  local variables
    integer                             :: Nd
    integer                           :: i, istat
    complex (kind=8), allocatable, dimension(:)  :: datum
    logical                           :: ok, hasname, binary
    character(80)                     :: fname, isbinary

    if(.not. E%allocated) then
        write(0, *) 'cvector_t must be allocated before call to write_cvector'
        return
    endif

    if (.not. present(ftype)) then
        binary = .false.
    elseif (index(ftype,'b')>0) then
        binary = .true.
    else
        binary = .false.
    endif

    inquire(fid, opened=ok, named=hasname, name=fname, unformatted=isbinary)

    ! check that the file is unformatted if binary, formatted if ascii
    if ((index(isbinary,'yes')>0 .or. index(isbinary,'YES')>0) .and. .not. binary) then
        write(0,*) 'Warning: Unable to write cvector_t to unformatted file ',trim(fname)
    elseif ((index(isbinary,'no')>0 .or. index(isbinary,'NO')>0) .and. binary) then
        write(0,*) 'Warning: Unable to write cvector_t to formatted file ',trim(fname)
    endif

    ! write binary to unformatted files
    if (binary) then
        write(fid) E%nd,E%FEType
        write(fid) E%datum
        return
    end if

    ! otherwise, write ascii
    write(fid,'(3i5,a10)',iostat=istat) E%nd,trim(E%FEType)

    Nd = E%nd

    allocate(datum(Nd),STAT=istat)
    datum = C_ZERO

    if (E%FEType == EDGE .or. E%FEType == NODE.or. E%FEType == ELEMENT) then
        datum = E%datum
    else
        write (0, *) 'not a known tag'
    end if

    do i = 1,Nd
        write(fid,'(3i5,6es14.6)',iostat=istat) i,datum(i)
    end do

    deallocate(datum,STAT=istat)

    end subroutine write_cvector

    !------------------------------------------------------------------------
    ! read_rvector reads an rvector_t in a simple format; rvector_t must match
    ! the input grid; file unit must already be available for reading.
    ! optional file type may be 'ascii' or 'binary', shortcuts allowed;
    ! defaults to 'ascii'. use binary for better accuracy.
    !------------------------------------------------------------------------
    subroutine read_rvector(fid, E, ftype)

    integer,        intent(in)      :: fid
    type (rvector_t), intent(inout)       :: E
    character(*), optional, intent(in):: ftype

    !  local variables
    integer                             :: Nd
    character(80)                       :: FEType
    integer                           :: i, ii, istat
    real (kind=8), allocatable, dimension(:)  :: datum
    logical                           :: ok, hasname, binary
    character(80)                     :: fname, isbinary

    if (.not. present(ftype)) then
        binary = .false.
    elseif (index(ftype,'b')>0) then
        binary = .true.
    else
        binary = .false.
    endif

    inquire(fid, opened=ok, named=hasname, name=fname, unformatted=isbinary)

    ! check that the file is unformatted if binary, formatted if ascii
    if ((index(isbinary,'yes')>0 .or. index(isbinary,'YES')>0) .and. .not. binary) then
        write(0,*) 'Warning: Unable to read rvector_t from unformatted file ',trim(fname)
    elseif ((index(isbinary,'no')>0 .or. index(isbinary,'NO')>0) .and. binary) then
        write(0,*) 'Warning: Unable to read rvector_t from formatted file ',trim(fname)
    endif

    if (binary) then
        ! read binary from unformatted files
        read(fid) Nd,FEType
    else
        ! otherwise, read ascii
        read(fid,*,iostat=istat) Nd,FEType
    end if

    if(.not. E%allocated) then
        write(0, *) 'rvector_t must be allocated before reading from ',trim(fname)
        stop
    elseif (E%FEType .ne. FEType) then
        write(0, *) 'rvector_t must be of type ',FEType,' before reading from ',trim(fname)
        stop
    elseif (E%nd .ne. Nd) then
        write(0, *) 'wrong size of rvector_t on input from ',trim(fname)
        stop
    endif

    if (binary) then
        ! read binary from unformatted files
        read(fid) E%datum
        return
    end if

    ! otherwise, read ascii
    allocate(datum(Nd),STAT=istat)
    datum = R_ZERO

    do i = 1,Nd
        read(fid,*,iostat=istat) ii,datum(i)
    end do

    if (E%FEType == EDGE .or. E%FEType == NODE.or. E%FEType == ELEMENT) then
        E%datum = datum
    else
        write (0, *) 'not a known tag'
    end if

    deallocate(datum,STAT=istat)

    end subroutine read_rvector

    !------------------------------------------------------------------------
    ! read_cvector reads a cvector_t in a simple ASCII format; cvector_t must match
    ! the input grid; file unit must already be available for reading.
    ! optional file type may be 'ascii' or 'binary', shortcuts allowed;
    ! defaults to 'ascii'. use binary for better accuracy.
    !------------------------------------------------------------------------
    subroutine read_cvector(fid, E, ftype)

    integer,        intent(in)      :: fid
    type (cvector_t), intent(inout)       :: E
    character(*), optional, intent(in):: ftype

    !  local variables
    integer                             :: Nd
    character(80)                       :: FEType
    integer                           :: i, ii, istat
    real (kind=8)                  :: datumr, datumi
    complex (kind=8), allocatable, dimension(:)  :: datum
    logical                           :: ok, hasname, binary
    character(80)                     :: fname, isbinary

    if (.not. present(ftype)) then
        binary = .false.
    elseif (index(ftype,'b')>0) then
        binary = .true.
    else
        binary = .false.
    endif

    inquire(fid, opened=ok, named=hasname, name=fname, unformatted=isbinary)

    ! check that the file is unformatted if binary, formatted if ascii
    if ((index(isbinary,'yes')>0 .or. index(isbinary,'YES')>0) .and. .not. binary) then
        write(0,*) 'Warning: Unable to read cvector_t from unformatted file ',trim(fname)
    elseif ((index(isbinary,'no')>0 .or. index(isbinary,'NO')>0) .and. binary) then
        write(0,*) 'Warning: Unable to read cvector_t from formatted file ',trim(fname)
    endif

    if (binary) then
        ! read binary from unformatted files
        read(fid) Nd,FEType
    else
        ! otherwise, read ascii
        read(fid,*,iostat=istat) Nd,FEType
    end if

    if(.not. E%allocated) then
        write(0, *) 'cvector_t must be allocated before reading from ',trim(fname)
        stop
    elseif (E%FEType .ne. FEType) then
        write(0, *) 'cvector_t must be of type ',FEType,' before reading from ',trim(fname)
        stop
    elseif (E%nd .ne. Nd) then
        write(0, *) 'wrong size of cvector_t on input from ',trim(fname)
        stop
    endif

    if (binary) then
        ! read binary from unformatted files
        read(fid) E%datum
        return
    end if

    ! otherwise, read ascii
    allocate(datum(Nd),STAT=istat)
    datum = C_ZERO

    do i = 1,Nd
        ! makes the acceptable formatting more flexible
        read(fid,*,iostat=istat) ii,datumr,datumi
        datum(i) = cmplx(datumr,datumi)
    end do

    if (E%FEType == EDGE .or. E%FEType == NODE.or. E%FEType == ELEMENT) then
        E%datum = datum
    else
        write (0, *) 'not a known tag'
    end if

    deallocate(datum,STAT=istat)

    end subroutine read_cvector

    !------------------------------------------------------------------------
    ! copy_rvector makes an exact copy of derived data type
    ! rvector_t;   NOTE: first argument is output
    !------------------------------------------------------------------------
    subroutine copy_rvector(E2, E1)

    implicit none
    type (rvector_t), intent(in)       :: E1
    type (rvector_t), intent(inout)    :: E2
    integer                      :: status

    ! check to see if RHS (E1) is active (allocated)
    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for copy_rvector'
    else

        if(E1%nd == E2%nd) then

            if (E1%FEType == E2%FEType) then

                ! just copy components
                E2%datum = E1%datum
                E2%FEType = E1%FEType

            else
                write (0, *) 'not compatible usage for copy_rvector'
            end if

        else

            if(E2%allocated) then
                ! first deallocate memory for datum
                deallocate(E2%datum,STAT=status)
            end if

            !  then allocate E2 as correct size ...
            Call create_rvector(E1%nd, E2, E1%FEType)
            !   .... and copy E1
            E2%datum = E1%datum
            E2%FEType = E1%FEType

        end if

    end if

    ! if the input was a temporary function output, deallocate
    if (E1%temporary) then
        call deall_rvector(E1)
    end if

    end subroutine copy_rvector  ! copy_rvector

    !------------------------------------------------------------------------
    ! copy_cvector makes an exact copy of derived data type
    ! cvector_t;
    !------------------------------------------------------------------------
    subroutine copy_cvector(E2, E1)

    implicit none
    type (cvector_t), intent(in)            :: E1
    type (cvector_t), intent(inout)         :: E2
    integer                               :: status

    ! check to see if RHS (E1) is active (allocated)
    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for copy_cvector'
    else

        if(E2%nd == E1%nd) then

            if  (E1%FEType == E2%FEType) then

                ! just copy components
                E2%datum = E1%datum
                E2%FEType = E1%FEType

            else
                write (0, *) 'not compatible usage for copy_cvector'
            end if

        else

            if(E2%allocated) then
                ! first deallocate memory for datum
                deallocate(E2%datum,STAT=status)
            end if

            !  then allocate E2 as correct size ...
            Call create_cvector(E1%nd, E2, E1%FEType)
            !   .... and copy E1
            E2%datum = E1%datum
            E2%FEType = E1%FEType

        end if

    end if

    ! if the input was a temporary function output, deallocate
    if (E1%temporary) then
        call deall_cvector(E1)
    end if

    end subroutine copy_cvector  ! copy_cvector

    !------------------------------------------------------------------------
    ! zero_rvector zeros variable of derived data type
    ! rvector_t;
    !------------------------------------------------------------------------
    subroutine zero_rvector(E)

    implicit none
    type (rvector_t), intent(inout)   :: E

    ! check to see if E is active (allocated)
    if(.not.E%allocated) then
        write(0,*) 'Error in zero_rvector: E not allocated'
    else

        E%datum = R_ZERO

    end if

    end subroutine zero_rvector

    !------------------------------------------------------------------------
    ! zero_cvector zeros variable of derived data type
    ! cvector_t;
    !------------------------------------------------------------------------
    subroutine zero_cvector(E)

    implicit none
    type (cvector_t), intent(inout) :: E

    ! check to see if E is active (allocated)
    if(.not.E%allocated) then
        write(0,*) 'Error in zero_cvector: E not allocated'
    else

        E%datum = C_ZERO

    end if

    end subroutine zero_cvector ! zero_cvector

    !------------------------------------------------------------------------
    ! * Creates a random perturbation in rvector_t - used for testing
    !------------------------------------------------------------------------
    subroutine random_rvector(E,eps)

    implicit none
    type (rvector_t), intent(inout)                    :: E
    real(8), intent(in), optional                    :: eps

    if (.not. E%allocated) then
        call warning('cvector_t not allocated in random_rvector')
        return
    end if

    call zero_rvector(E)

    call random_number(E%datum)

    if (present(eps)) then
        E%datum = E%datum * eps
    else
        E%datum = E%datum * 0.05d0
    end if

    end subroutine random_rvector

    !------------------------------------------------------------------------
    ! * Creates a random perturbation in cvector_t - used for testing
    !------------------------------------------------------------------------
    subroutine random_cvector(E,eps)

    implicit none
    type (cvector_t), intent(inout)                    :: E
    real(8), intent(in), optional                    :: eps
    ! local
    real (kind=8), allocatable, dimension(:)  :: datumr, datumi
    integer              :: Nd,istat

    if (.not. E%allocated) then
        call warning('cvector_t not allocated in random_cvector')
        return
    end if

    call zero_cvector(E)

    ! make some random vectors
    Nd = E%nd

    allocate(datumr(Nd),datumi(Nd),STAT=istat)

    call random_number(datumr)
    call random_number(datumi)

    if (present(eps)) then
        datumr = datumr * eps
        datumi = datumi * eps
    else
        datumr = datumr * 0.05d0
        datumi = datumi * 0.05d0
    end if

    if (E%FEType == EDGE .or. E%FEType == NODE.or. E%FEType == ELEMENT) then
        E%datum(:) = cmplx(datumr(:),datumi(:))
    else
        write (0, *) 'not a known tag'
    end if

    deallocate(datumr,datumi,STAT=istat)

    end subroutine random_cvector

    !------------------------------------------------------------------------
    ! * check two vectors for compatibility for linear operator purposes
    !------------------------------------------------------------------------
    function compare_cvector_f(E1,E2) result (status)

    type (cvector_t), intent(in)                  :: E1
    type (cvector_t), intent(in)                  :: E2
    logical                                     :: status

    status = .FALSE.

    ! Check whether all the vector nodes are of the same size
    if(E1%nd == E2%nd) then
        if (E1%FEType == E2%FEType) then
            status = .TRUE.
        end if
    end if


    end function compare_cvector_f

    !------------------------------------------------------------------------
    ! * check two vectors for compatibility for linear operator purposes
    !------------------------------------------------------------------------
    function compare_rvector_f(E1,E2) result (status)

    type (rvector_t), intent(in)                  :: E1
    type (rvector_t), intent(in)                  :: E2
    logical                                     :: status

    status = .FALSE.

    ! Check whether all the vector nodes are of the same size
    if(E1%nd == E2%nd) then
        if (E1%FEType == E2%FEType) then
            status = .TRUE.
        end if
    end if


    end function compare_rvector_f

    !------------------------------------------------------------------------
    ! scMult_cvector multiplies vector stored as derived data type
    ! cvector_t with a complex scalar; subroutine version
    ! E2 can overwrite E1
    !------------------------------------------------------------------------
    subroutine scMult_cvector(c, E1, E2)

    implicit none
    complex(kind=8), intent(in)                      :: c
    ! a complex scalar to be multiplied with
    type (cvector_t), intent(in)                       :: E1
    type (cvector_t), intent(inout)                    :: E2

    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for scMult_cvector'
        return
    endif

    ! check to see if LHS (E2) is active (allocated)
    if(.not.E2%allocated) then
        write(0,*) 'LHS was not allocated yet for scMult_cvector'
    else

        ! Check whether all the vector nodes are of the same size
        if(E1%nd == E2%nd) then

            if (E1%FEType == E2%FEType) then

                ! complex scalar multiplication for datum
                E2%datum = E1%datum * c

            else
                write (0, *) 'not compatible usage for scMult_cvector'
            end if

        else
            write(0, *) 'Error:scMult_cvector: vectors not same size'

        end if
    end if

    end subroutine scMult_cvector ! scMult_cvector

    !------------------------------------------------------------------------
    ! scMult_cvector_f multiplies vector stored as derived data type
    ! cvector_t with a complex scalar; function version
    !------------------------------------------------------------------------
    function scMult_cvector_f(c, E1) result(E2)

    implicit none
    complex(kind=8), intent(in)                      :: c
    ! a complex scalar to be multiplied with
    type (cvector_t), intent(in)                       :: E1
    type (cvector_t)                                   :: E2

    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for scMult_cvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E2, E1%FEType)
    ! check to see if LHS (E2) is active (allocated)
    if(.not.E2%allocated) then
        write(0,*) 'LHS was not allocated for scMult_cvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if(E1%nd == E2%nd) then

            if (E1%FEType == E2%FEType) then

                ! complex scalar multiplication for datum
                E2%datum = E1%datum * c

            else
                write (0, *) 'not compatible usage for scMult_cvector_f'
            end if

        else

            write(0, *) 'Error:scMult_cvector_f: vectors not same size'

        end if
    end if

    E2%temporary = .true.

    end function scMult_cvector_f ! scMult_cvector_f

    !------------------------------------------------------------------------
    ! scMultReal_cvector multiplies vector stored as derived data type
    ! cvector_t with a real scalar; subroutine version
    ! E2 can overwrite E1
    !------------------------------------------------------------------------
    subroutine scMultReal_cvector(c, E1, E2)

    implicit none
    real (kind=8), intent(in)                         :: c
    ! a real scalar to be multiplied with
    type (cvector_t), intent(in)                       :: E1
    type (cvector_t), intent(inout)                    :: E2

    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for scMultReal_cvector'
        return
    endif

    ! check to see if LHS (E2) is active (allocated)
    if(.not.E2%allocated) then
        write(0,*) 'LHS was not allocated yet for scMultReal_cvector'
    else

        ! Check whether all the vector nodes are of the same size
        if(E1%nd == E2%nd) then

            if (E1%FEType == E2%FEType) then

                ! complex scalar multiplication for datum
                E2%datum = E1%datum * c

            else
                write (0, *) 'not compatible usage for scMultReal_cvector'
            end if

        else
            write(0, *) 'Error:scMultReal_cvector: vectors not same size'

        end if
    end if

    end subroutine scMultReal_cvector ! scMultReal_cvector

    !------------------------------------------------------------------------
    ! scMult_cvector_f multiplies vector stored as derived data type
    ! cvector_t with a real scalar; function version
    !------------------------------------------------------------------------
    function scMultReal_cvector_f(c, E1) result(E2)

    implicit none
    real (kind=8), intent(in)                :: c
    ! a real scalar to be multiplied with
    type (cvector_t), intent(in)                       :: E1
    type (cvector_t)                                   :: E2

    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for scMultReal_cvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E2, E1%FEType)
    ! check to see if LHS (E2) is active (allocated)
    if(.not.E2%allocated) then
        write(0,*) 'LHS was not allocated for scMultReal_cvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if(E1%nd == E2%nd) then

            if (E1%FEType == E2%FEType) then

                ! complex scalar multiplication for datum
                E2%datum = E1%datum * c

            else
                write (0, *) 'not compatible usage for scMultReal_cvector_f'
            end if

        else

            write(0, *) 'Error:scMultReal_cvector_f: vectors not same size'

        end if
    end if

    E2%temporary = .true.

    end function scMultReal_cvector_f ! scMultReal_cvector_f

    !------------------------------------------------------------------------
    ! scMult_rvector multiplies vector stored as derived data type
    ! rvector_t with a real scalar; subroutine version
    ! E2 can overwrite E1
    !------------------------------------------------------------------------
    subroutine scMult_rvector(c, E1, E2)

    implicit none
    real (kind=8), intent(in)                         :: c
    ! a real scalar to be multiplied with
    type (rvector_t), intent(in)                       :: E1
    type (rvector_t), intent(inout)                    :: E2

    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for scMult_rvector'
        return
    endif

    ! check to see if LHS (E2) is active (allocated)
    if(.not.E2%allocated) then
        write(0,*) 'LHS was not allocated for scMult_rvector'
    else

        ! Check whether all the vector nodes are of the same size
        if(E1%nd == E2%nd) then

            if (E1%FEType == E2%FEType) then

                ! real scalar multiplication for datum
                E2%datum = E1%datum * c

            else
                write (0, *) 'not compatible usage for scMult_rvector'
            end if

        else

            write(0, *) 'Error:scMult_rvector: vectors not same size'

        end if
    end if

    end subroutine scMult_rvector ! scMult_rvector

    !------------------------------------------------------------------------
    ! scMult_rvector_f multiplies vector stored as derived data type
    ! rvector_t with a real scalar; function version
    !------------------------------------------------------------------------
    function scMult_rvector_f(c, E1) result(E2)

    implicit none
    real (kind=8), intent(in)                         :: c
    ! a complex scalar to be multiplied with
    type (rvector_t), intent(in)                       :: E1
    type (rvector_t)                                   :: E2

    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for scMult_rvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_rvector(E1%nd, E2, E1%FEType)
    ! check to see if LHS (E2) is active (allocated)
    if(.not.E2%allocated) then
        write(0,*) 'LHS was not allocated for scMult_rvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if(E1%nd == E2%nd) then

            if (E1%FEType == E2%FEType) then

                ! real scalar multiplication for datum
                E2%datum = E1%datum * c

            else
                write (0, *) 'not compatible usage for scMult_rvector_f'
            end if

        else

            write(0, *) 'Error:scMult_rvector_f: vectors not same size'

        end if
    end if

    E2%temporary = .true.

    end function scMult_rvector_f ! scMult_rvector_f

    !------------------------------------------------------------------------
    ! add_rvector adds vectors stored as derived data type
    ! rvector_t with ; subroutine version
    ! E3 can overwrite E1 and E2
    !------------------------------------------------------------------------
    subroutine add_rvector(E1, E2, E3)

    implicit none
    type (rvector_t), intent(in)               :: E1, E2
    type (rvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for add_rvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for add_rvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! add datum
                E3%datum = E1%datum + E2%datum

            else
                write (0, *) 'not compatible usage for add_rvector'
            end if

        else

            write(0, *) 'Error:add_rvector: vectors not same size'

        end if
    end if

    end subroutine add_rvector ! add_rvector

    !------------------------------------------------------------------------
    ! add_rvector_f adds vectors stored as derived data type
    ! rvector_t with ; function version
    !------------------------------------------------------------------------
    function add_rvector_f(E1, E2) result(E3)

    implicit none
    type (rvector_t), intent(in)               :: E1, E2
    type (rvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for add_rvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_rvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for add_rvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! add datum
                E3%datum = E1%datum + E2%datum

            else
                write (0, *) 'not compatible usage for add_rvector_f'
            end if

        else

            write(0, *) 'Error:add_rvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function add_rvector_f ! add_rvector_f

    !------------------------------------------------------------------------
    ! add_cvector adds vectors stored as derived data type
    ! cvector_t with ; subroutine version
    ! E3 can overwrite E1 and E2
    !------------------------------------------------------------------------
    subroutine add_cvector(E1, E2, E3)

    implicit none
    type (cvector_t), intent(in)               :: E1, E2
    type (cvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for add_cvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS not allocated for add_cvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! add datum
                E3%datum = E1%datum + E2%datum

            else
                write (0, *) 'not compatible usage for add_cvector'
            end if

        else

            write(0, *) 'Error:add_cvector: vectors not same size'

        end if
    end if

    end subroutine add_cvector ! add_cvector

    !------------------------------------------------------------------------
    ! add_cvector_f adds vectors stored as derived data type
    ! cvector_t with ; function version
    !------------------------------------------------------------------------
    function add_cvector_f(E1, E2) result(E3)

    implicit none
    type (cvector_t), intent(in)               :: E1, E2
    type (cvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for add_cvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for add_cvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! add datum
                E3%datum = E1%datum + E2%datum

            else
                write (0, *) 'not compatible usage for add_cvector_f'
            end if

        else

            write(0, *) 'Error:add_cvector: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function add_cvector_f ! add_cvector_f

    !------------------------------------------------------------------------
    ! subtract_rvector subtracts vectors stored as derived data type rvector_t with
    ! ; subroutine version
    ! E3 can overwrite E1 and E2
    !------------------------------------------------------------------------
    subroutine subtract_rvector(E1, E2, E3)

    implicit none
    type (rvector_t), intent(in)               :: E1, E2
    type (rvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for subtract_rvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for subtract_rvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! subtract datum
                E3%datum = E1%datum - E2%datum

            else
                write (0, *) 'not compatible usage for subtract_rvector'
            end if

        else

            write(0, *) 'Error: subtract_rvector: vectors not same size'

        end if
    end if

    end subroutine subtract_rvector ! subtract_rvector

    !------------------------------------------------------------------------
    ! subtract_rvector_f subtracts vectors stored as derived data type
    ! rvector_t with ; function version
    !------------------------------------------------------------------------
    function subtract_rvector_f(E1, E2) result(E3)

    implicit none
    type (rvector_t), intent(in)               :: E1, E2
    type (rvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for add_rvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_rvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for add_rvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! subtract datum
                E3%datum = E1%datum - E2%datum

            else
                write (0, *) 'not compatible usage for subtract_rvector_f'
            end if

        else

            write(0, *) 'Error:subtract_rvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function subtract_rvector_f ! subtract_rvector_f

    !------------------------------------------------------------------------
    ! subtract_cvector subtracts vectors stored as derived data type
    ! cvector_t with ; subroutine version
    ! E3 can overwrite E1 and E2
    !------------------------------------------------------------------------
    subroutine subtract_cvector(E1, E2, E3)

    implicit none
    type (cvector_t), intent(in)               :: E1, E2
    type (cvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for subtract_cvector'
        return
    endif

    ! check to see if LHS (E2) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS not allocated for subtract_cvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! subtract datum
                E3%datum = E1%datum - E2%datum

            else
                write (0, *) 'not compatible usage for subtract_cvector'
            end if

        else

            write(0, *) 'Error:subtract_cvector: vectors not same size'

        end if
    end if

    end subroutine subtract_cvector ! subtract_cvector

    !------------------------------------------------------------------------
    ! subtract_cvector_f subtracts vectors stored as derived data type
    ! cvector_t with ; function version
    !------------------------------------------------------------------------
    function subtract_cvector_f(E1, E2) result(E3)

    implicit none
    type (cvector_t), intent(in)               :: E1, E2
    type (cvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for subtract_cvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for subtract_cvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! subtract datum
                E3%datum = E1%datum - E2%datum

            else
                write (0, *) 'not compatible usage for subtract_cvector_f'
            end if

        else

            write(0, *) 'Error: subtract_cvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function subtract_cvector_f ! subtract_cvector_f

    !------------------------------------------------------------------------
    ! diagMult_rvector multiplies two vectors E1, E2 stored as derived data
    ! type rvector_t pointwise; subroutine version
    ! E3 can overwrite E1 or E2
    !------------------------------------------------------------------------
    subroutine diagMult_rvector(E1, E2, E3)

    implicit none
    type (rvector_t), intent(in)               :: E1, E2
    type (rvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_rvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagMult_rvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_rvector'
            end if

        else

            write(0, *) 'Error:diagMult_rvector: vectors not same size'

        end if
    end if

    end subroutine diagMult_rvector ! diagMult_rvector

    !------------------------------------------------------------------------
    ! diagMult_rvector_f multiplies two vectors E1, E2 stored as derived
    ! data type rvector_t pointwise; function version
    !------------------------------------------------------------------------
    function diagMult_rvector_f(E1, E2) result(E3)

    implicit none
    type (rvector_t), intent(in)               :: E1, E2
    type (rvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_rvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_rvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagMult_rvector_f'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_rvector_f'
            end if

        else

            write(0, *) 'Error:diagMult_rvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function diagMult_rvector_f ! diagMult_rvector_f

    !------------------------------------------------------------------------
    ! diagMult_cvector multiplies two vectors E1, E2 stored as derived data
    ! type cvector_t pointwise; subroutine version
    ! E3 can overwrite E1 or E2
    !------------------------------------------------------------------------
    subroutine diagMult_cvector(E1, E2, E3)

    implicit none
    type (cvector_t), intent(in)               :: E1, E2
    type (cvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_cvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diaMult_cvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_cvector'
            end if

        else

            write(0, *) 'Error:diagMult_cvector: vectors not same size'

        end if
    end if

    end subroutine diagMult_cvector ! diagMult_cvector

    !------------------------------------------------------------------------
    ! diagMult_cvector_f multiplies two vectors E1, E2 stored as derived
    ! data  type cvector_t pointwise; function version
    !------------------------------------------------------------------------
    function diagMult_cvector_f(E1, E2) result(E3)

    implicit none
    type (cvector_t), intent(in)               :: E1, E2
    type (cvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_cvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E3, E1%FEType)
    ! check to see if RHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'RHS was not allocated for diagMult_cvector_f'
    else

        ! Check whether both vectors are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_cvector_f'
            end if

        else

            write(0, *) 'Error:diagMult_cvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function diagMult_cvector_f ! diagMult_cvector_f

    !------------------------------------------------------------------------
    ! diagMult_crvector multiplies complex vector E1 with scalar vector E2
    ! stored as derived type cvector_t pointwise; subroutine version
    ! E3 can overwrite E1 or E2
    !------------------------------------------------------------------------
    subroutine diagMult_crvector(E1, E2, E3)

    implicit none
    type (cvector_t), intent(in)               :: E1
    type (rvector_t), intent(in)               :: E2
    type (cvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_crvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagMult_crvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_crvector'
            end if

        else

            write(0, *) 'Error:diagMult_crvector: vectors not same size'

        end if
    end if

    end subroutine diagMult_crvector ! diagMult_crvector

    !------------------------------------------------------------------------
    ! diagMult_crvector_f multiplies complex vector E1 with real vector
    ! E2 stored as derived data type cvector_t pointwise; function version
    !------------------------------------------------------------------------
    function diagMult_crvector_f(E1, E2) result(E3)

    implicit none
    type (cvector_t), intent(in)               :: E1
    type (rvector_t), intent(in)               :: E2
    type (cvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_crvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagMult_crvector_f'
    else

        ! Check whether both vectors are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_crvector_f'
            end if

        else

            write(0, *) 'Error:diagMult_Node_MixedCR_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function diagMult_crvector_f ! diagMult_crvector_f

    !------------------------------------------------------------------------
    ! diagMult_rcvector multiplies real vector E1 with complex vector E2
    ! stored as derived type cvector_t pointwise; subroutine version
    ! E3 can overwrite E1 or E2
    !------------------------------------------------------------------------
    subroutine diagMult_rcvector(E1, E2, E3)

    implicit none
    type (rvector_t), intent(in)               :: E1
    type (cvector_t), intent(in)               :: E2
    type (cvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_rcvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagMult_rcvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_rcvector'
            end if

        else

            write(0, *) 'Error:diagMult_rcvector: vectors not same size'

        end if
    end if

    end subroutine diagMult_rcvector ! diagMult_rcvector

    !------------------------------------------------------------------------
    ! diagMult_rcvector_f multiplies real vector E1 with complex vector E2
    ! stored as derived data type cvector_t pointwise; function version
    !------------------------------------------------------------------------
    function diagMult_rcvector_f(E1, E2) result(E3)

    implicit none
    type (rvector_t), intent(in)               :: E1
    type (cvector_t), intent(in)               :: E2
    type (cvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagMult_rcvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E3, E1%FEType)
    ! check to see if RHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'RHS was not allocated for diagMult_rcvector_f'
    else

        ! Check whether both vectors are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise multiplication for datum
                E3%datum = E1%datum * E2%datum

            else
                write (0, *) 'not compatible usage for diagMult_rcvector_f'
            end if

        else

            write(0, *) 'Error:diagMult_rcvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function diagMult_rcvector_f ! diagMult_rcvector_f

    !------------------------------------------------------------------------
    ! diagDiv_rvector divides real vector E1 with real vector E2
    ! stored as derived type rvector_t pointwise; subroutine version
    ! E3 can overwrite E1 or E2
    !------------------------------------------------------------------------
    subroutine diagDiv_rvector(E1, E2, E3)

    implicit none
    type (rvector_t), intent(in)               :: E1
    type (rvector_t), intent(in)               :: E2
    type (rvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagDiv_rvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagDiv_rvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise division for datum
                E3%datum = E1%datum / E2%datum

            else
                write (0, *) 'not compatible usage for diagDiv_rvector'
            end if

        else

            write(0, *) 'Error:diagDiv_rvector: vectors not same size'

        end if
    end if

    end subroutine diagDiv_rvector ! diagDiv_rvector

    !------------------------------------------------------------------------
    ! diagDiv_crvector divides complex vector E1 with real vector E2
    ! stored as derived type cvector_t pointwise; subroutine version
    ! E3 can overwrite E1 or E2
    !------------------------------------------------------------------------
    subroutine diagDiv_crvector(E1, E2, E3)

    implicit none
    type (cvector_t), intent(in)               :: E1
    type (rvector_t), intent(in)               :: E2
    type (cvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagDiv_crvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagDiv_crvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise division for datum
                E3%datum = E1%datum / E2%datum

            else
                write (0, *) 'not compatible usage for diagDiv_crvector'
            end if

        else

            write(0, *) 'Error:diagDiv_crvector: vectors not same size'

        end if
    end if

    end subroutine diagDiv_crvector ! diagDiv_crvector

    !------------------------------------------------------------------------
    ! diagDiv_crvector_f divides complex vector E1 with real vector
    ! E2 stored as derived data type cvector_t pointwise; function version
    !------------------------------------------------------------------------
    function diagDiv_crvector_f(E1, E2) result(E3)

    implicit none
    type (cvector_t), intent(in)               :: E1
    type (rvector_t), intent(in)               :: E2
    type (cvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagDiv_crvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagDiv_crvector_f'
    else

        ! Check whether both vectors are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise division for datum
                E3%datum = E1%datum / E2%datum

            else
                write (0, *) 'not compatible usage for diagDicrvector_f'
            end if

        else

            write(0, *) 'Error:diagDicrvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function diagDiv_crvector_f ! diagDiv_crvector_f

    !------------------------------------------------------------------------
    ! diagDiv_rcvector divides real vector E1 with complex vector E2
    ! stored as derived type cvector_t pointwise; subroutine version
    ! E3 can overwrite E1 or E2
    !------------------------------------------------------------------------
    subroutine diagDiv_rcvector(E1, E2, E3)

    implicit none
    type (rvector_t), intent(in)               :: E1
    type (cvector_t), intent(in)               :: E2
    type (cvector_t), intent(inout)            :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagDiv_rcvector'
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagDiv_rcvector'
    else

        ! Check whether all the vector nodes are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise divides for datum
                E3%datum = E2%datum / E1%datum

            else
                write (0, *) 'not compatible usage for diagDiv_rcvector'
            end if

        else

            write(0, *) 'Error:diagDiv_rcvector: vectors not same size'

        end if
    end if

    end subroutine diagDiv_rcvector ! diagDiv_rcvector

    !------------------------------------------------------------------------
    ! diagDiv_rcvector_f divides real vector E1 with complex vector E2
    ! stored as derived data type cvector_t pointwise; function version
    !------------------------------------------------------------------------
    function diagDiv_rcvector_f(E1, E2) result(E3)

    implicit none
    type (rvector_t), intent(in)               :: E1
    type (cvector_t), intent(in)               :: E2
    type (cvector_t)                           :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for diagDiv_rcvector_f'
        return
    endif

    ! In function version, appropriate data types need to be created
    Call create_cvector(E1%nd, E3, E1%FEType)
    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        write(0,*) 'LHS was not allocated for diagDiv_rcvector_f'
    else

        ! Check whether both vectors are of the same size
        if((E1%nd == E2%nd) .and. (E1%nd == E3%nd)) then

            if ((E1%FEType == E2%FEType).and.(E1%FEType == E3%FEType)) then

                ! pointwise divides for datum
                E3%datum = E2%datum / E1%datum

            else
                write (0, *) 'not compatible usage for diagDiv_rcvector_f'
            end if

        else

            write(0, *) 'Error:diagDiv_rcvector_f: vectors not same size'

        end if
    end if

    E3%temporary = .true.

    end function diagDiv_rcvector_f ! diagDiv_rcvector_f

    !------------------------------------------------------------------------
    ! dotProd_rvector computes dot product of two vectors stored
    ! as derived data type rvector_t, returning a real number
    !------------------------------------------------------------------------
    function dotProd_rvector_f(E1, E2) result(r)

    implicit none
    type (rvector_t), intent(in)   :: E1, E2
    type (rvector_t)               :: E3
    real(kind=8)                 :: r
    integer                      :: nd

    r = R_ZERO

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'input vectors not allocated yet for dotProd_rvector_f'
        return
    endif

    E3 = E1
    nd = E3%nd

    ! Check whether both input vectors are of the same size
    if(E1%nd == E2%nd) then

        if ((E1%FEType == E2%FEType)) then

            r = r + sum(E3%datum * E2%datum)

        else
            write (0,*) 'not compatible input vectors in dotProd_rvector_f'
        end if

    else

        write(0,*) 'vectors not the same size in dotProd_rvector_f'

    end if

    call deall_rvector(E3)

    end function dotProd_rvector_f  ! dotProd_rvector_f

    !------------------------------------------------------------------------
    ! dotProd_cvector computes dot product of two vectors stored
    ! as derived data type cvector_t, returning a complex number
    !------------------------------------------------------------------------
    function dotProd_cvector_f(E1, E2) result(c)

    implicit none
    type (cvector_t), intent(in)       :: E1, E2
    type (cvector_t)                   :: E3
    complex(kind=8)              :: c
    integer                          :: nd

    c = R_ZERO

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProdCC'
        return
    endif

    E3 = E1
    nd = E3%nd


    ! Check whether both input vectors are of the same size
    if(E1%nd == E2%nd) then

        if ((E1%FEType == E2%FEType)) then

            c = c + sum(conjg(E3%datum) * E2%datum)

        else
            write (0, *) 'not compatible input vectors in dotProd_cvector_f'
        end if

    else

        write(0, *) 'vectors not the same size in dotProd_cvector_f'

    end if

    call deall_cvector(E3)

    end function dotProd_cvector_f ! dotProd_cvector

    !------------------------------------------------------------------------
    ! dotProd_noConj_cvector computes dot product of two vectors stored
    ! as derived data type cvector_t, returning a complex number
    !------------------------------------------------------------------------
    function dotProd_noConj_cvector_f(E1, E2) result(c)

    implicit none
    type (cvector_t), intent(in)       :: E1, E2
    type (cvector_t)                   :: E3
    complex(kind=8)              :: c
    integer                          :: nd

    c = R_ZERO

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for dotProdCC'
        return
    endif

    E3 = E1
    nd = E3%nd


    ! Check whether both input vectors are of the same size
    if(E1%nd == E2%nd) then

        if ((E1%FEType == E2%FEType)) then

            c = c + sum(E3%datum * E2%datum)

        else
            write (0, *) 'not compatible input vectors in dotProd_noConj_cvector_f'
        end if

    else

        write(0, *) 'vectors not the same size in dotProd_noConj_cvector_f'

    end if

    call deall_cvector(E3)

    end function dotProd_noConj_cvector_f ! dotProd__noConj_cvector_f

    !------------------------------------------------------------------------
    ! linComb_cvector computes linear combination of two vectors
    ! stored as derived data type cvector_t; subroutine, not a function
    ! both input vectors must have the same dimension
    !------------------------------------------------------------------------
    subroutine linComb_cvector(inc1, E1, inc2, E2, E3)

    implicit none
    !   input vectors
    type (cvector_t), intent(in)             :: E1, E2
    !  input complex scalars
    complex (kind=8), intent(in)           :: inc1, inc2
    type (cvector_t), intent(inout)          :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        call warning('inputs not allocated yet for linComb_cvector')
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        call warning('output has to be allocated before call to linComb_cvector')

    elseif (compare(E1,E2) .and. compare(E1,E3)) then
        ! form linear combinatoin
        E3%datum = inc1*E1%datum + inc2*E2%datum

    else
        call warning('not compatible usage for linComb_cvector')
    end if

    end subroutine linComb_cvector ! linComb_cvector

    !------------------------------------------------------------------------
    ! linComb_rvector computes linear combination of two vectors
    ! stored as derived data type cvector_t; subroutine, not a function
    ! both input vectors must have the same dimension
    !------------------------------------------------------------------------
    subroutine linComb_rvector(inc1, E1, inc2, E2, E3)

    implicit none
    !   input vectors
    type (rvector_t), intent(in)             :: E1, E2
    !  input real scalars
    real (kind=8), intent(in)           :: inc1, inc2
    type (rvector_t), intent(inout)          :: E3

    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        call warning('inputs not allocated yet for linComb_rvector')
        return
    endif

    ! check to see if LHS (E3) is active (allocated)
    if(.not.E3%allocated) then
        call warning('output has to be allocated before call to linComb_rvector')
        return

    elseif (compare(E1,E2) .and. compare(E1,E3)) then
        ! form linear combinatoin
        E3%datum = inc1*E1%datum + inc2*E2%datum

    else
        call warning('not compatible usage for linComb_rvector')
        return
    end if

    end subroutine linComb_rvector ! linComb_rvector

    !------------------------------------------------------------------------
    ! scMultadd_cvector multiplies vector E1 stored as derived data type
    ! cvector_t with a complex scalar c, adding result to output vector E2
    !------------------------------------------------------------------------
    subroutine scMultAdd_cvector(c, E1, E2)

    implicit none
    complex(kind=8), intent(in)                   :: c
    ! a complex scalar to be multiplied with
    type (cvector_t), intent(in)                       :: E1
    type (cvector_t)                                   :: E2

    if(.not.E1%allocated) then
        write(0,*) 'RHS not allocated yet for scMultAdd_cvector'
        return
    endif

    ! check to see if LHS (E2) is active (allocated)
    if(.not.E2%allocated) then
        write(0,*) 'LHS was not allocated for scMultAdd_cvector'
    else

        ! Check whether both vectors are of the same size
        if(E1%nd == E2%nd) then

            if ((E1%FEType == E2%FEType)) then

                ! complex scalar multiplication for datum
                E2%datum = E2%datum + E1%datum * c

            else
                write (0, *) 'not compatible usage for scMultAdd_cvector'
            end if

        else

            write(0, *) 'Error:scMultAdd_cvector: vectors not same size'

        end if
    end if

    end subroutine scMultAdd_cvector ! scMultAdd_cvector

    !------------------------------------------------------------------------
    function conjg_cvector_f(E1) result (E2)
    ! conjg_cvector_f computes a conjugate of a derived data type cvector_t
    ! A.K.
    implicit none
    type (cvector_t), intent(in)            :: E1
    type (cvector_t)                        :: E2

    integer                               :: status

    ! check to see if RHS (E1) is active (allocated)
    if(.not.E1%allocated) then
        write(0,*) 'input not allocated yet for conjg_cvector_f'
    else

        if(E2%nd == E1%nd) then

            if  (E1%FEType == E2%FEType) then

                ! just conjugate components
                E2%datum = conjg(E1%datum)
                E2%FEType = E1%FEType

            else
                write (0, *) 'not compatible usage for conjg_cvector_f'
            end if

        else

            if(E2%allocated) then
                ! first deallocate memory for datum
                deallocate(E2%datum,STAT=status)
            end if

            !  then allocate E2 as correct size ...
            Call create_cvector(E1%nd, E2, E1%FEType)
            !   .... and conjugate E1
            E2%datum = conjg(E1%datum)
            E2%FEType = E1%FEType

        end if

    end if

    E2%temporary = .true.

    end function conjg_cvector_f  ! conjg_cvector_f

    !------------------------------------------------------------------------
    function cmplx_rvector_f(E1, E2) result (E3)
    ! inputs two real vectors, merges them as real1 + imag(real2), a complex
    ! vector
    implicit none
    type (rvector_t), intent(in)            :: E1
    type (rvector_t), intent(in)            :: E2
    type (cvector_t)                        :: E3

    integer                               :: status

    ! check to see if RHS (E1 and E2) are active (allocated)
    if((.not.E1%allocated).or.(.not.E2%allocated)) then
        write(0,*) 'RHS not allocated yet for cmplx_rvector_f'
    else

        if((E3%nd == E1%nd) .and. (E3%nd == E2%nd))  then

            if  ((E1%FEType == E2%FEType).and.(E1%FEType == E3% FEType)) then

                ! create a complex pair
                E3%datum = cmplx(E1%datum, E2%datum, kind=8)
                E3%FEType = E1%FEType

            else
                write (0, *) 'not compatible usage for cmplx_rvector_f'
            end if

        else

            if(E3%allocated) then
                ! first deallocate memory for datum
                deallocate(E3%datum,STAT=status)
            end if

            !  then allocate E3 as correct size ...
            Call create_cvector(E1%nd, E3, E1%FEType)
            !   .... and create a complex pair
            E3%datum = cmplx(E1%datum, E2%datum, kind=8)
            E3%FEType = E1%FEType

        end if

    end if

    E3%temporary = .true.

    end function cmplx_rvector_f  ! cmplx_rvector_f

    !------------------------------------------------------------------------
    ! real_cvector_f copies the real part of the derived data type cvector_t variable;
    ! to produce a derived data type rvector_t
    !------------------------------------------------------------------------
    function real_cvector_f(E1) result (E2)

    implicit none
    type (cvector_t), intent(in)            :: E1
    type (rvector_t)                        :: E2


    ! check to see if RHS (E1) is active (allocated)
    if(.not.E1%allocated) then
        write(0,*) 'input not allocated yet for real_cvector_f'
    else

        ! we know nothing about E2 ... deallocate just in case
        Call deall_rvector(E2)
        !  then allocate E2 as correct size ...
        Call create_rvector(E1%nd, E2, E1%FEType)
        !   .... and copy E1
        E2%datum = real(E1%datum)
        E2%FEType = E1%FEType

    end if

    E2%temporary = .true.

    end function real_cvector_f  ! real_cvector_f

    !------------------------------------------------------------------------
    ! imag_cvector_f copies the imag part of the derived data type cvector_t variable;
    ! to produce a derived data type rvector_t
    !------------------------------------------------------------------------
    function imag_cvector_f(E1) result (E2)

    implicit none
    type (cvector_t), intent(in)            :: E1
    type (rvector_t)                        :: E2


    ! check to see if RHS (E1) is active (allocated)
    if(.not.E1%allocated) then
        write(0,*) 'input not allocated yet for imag_cvector_f'
    else

        ! we know nothing about E2 ... deallocate just in case
        Call deall_rvector(E2)
        !  then allocate E2 as correct size ...
        Call create_rvector(E1%nd, E2, E1%FEType)
        !   .... and copy E1
        E2%datum = aimag(E1%datum)
        E2%FEType = E1%FEType

    end if

    E2%temporary = .true.

    end function imag_cvector_f  ! imag_cvector_f

    !------------------------------------------------------------------------
    ! convert complex vector to real vector
    !
    !------------------------------------------------------------------------
    function convert_1(cVector) result (rVector)
    implicit none
    type(cVector_t) , intent(in) :: cVector
    type(rVector_t) :: rVector
    integer(kind=4) :: stat
    character(len=256) :: errmsg
    integer(kind=4) :: i

    rVector%FEType = cVector%FEType
    rVector%temporary = .false.
    rVector%nd = 2*cVector%nd
    if(.not. allocated(rVector%datum))then
        allocate(rVector%datum(rVector%nd), stat=stat, errmsg=errmsg)
    else if(size(rVector%datum) /= rVector%nd)then
        deallocate(rVector%datum, stat=stat, errmsg=errmsg)
        allocate(rVector%datum(rVector%nd), stat=stat, errmsg=errmsg)
    end if
    rVector%datum = 0.0d0
    rVector%allocated = .true.

    do i = 1 , cVector%nd
        rVector%datum(i) = real(cVector%datum(i))
        rVector%datum(cVector%nd+i) = -imag(cVector%datum(i))
    end do

    end function
        
    !------------------------------------------------------------------------
    ! convert real vector to complex vector
    !
    !------------------------------------------------------------------------
    function convert_2(rVector) result (cVector)
    implicit none
    type(rVector_t) , intent(in) :: rVector
    type(cVector_t) :: cVector
    integer(kind=4) :: stat
    character(len=256) :: errmsg
    integer(kind=4) :: i

    cVector%FEType = rVector%FEType
    cVector%temporary = .false.
    cVector%nd = rVector%nd/2
    if(.not. allocated(cVector%datum))then
        allocate(cVector%datum(cVector%nd), stat=stat, errmsg=errmsg)
    else if(size(cVector%datum) /= cVector%nd)then
        deallocate(cVector%datum, stat=stat, errmsg=errmsg)
        allocate(cVector%datum(cVector%nd), stat=stat, errmsg=errmsg)
    end if
    cVector%datum = 0.0d0
    cVector%allocated = .true.

    do i = 1 , cVector%nd
        cVector%datum(i) = cmplx(rVector%datum(i),rVector%datum(cVector%nd+i))
    end do

    end function
    
    !------------------------------------------------------------------------
    end module
