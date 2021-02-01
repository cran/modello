!> Utility procedures to manipulate elementar entities.
!! @author Filippo Monari
module utils

  use env
  use errwarn
  
  private
  public &
       alloc, &
       dealloc, &
       dealloc_element, &
       append_to_array, &
       iminloc, &
       imaxloc, &
       swap, &
       unique, &
       dpEq
  
  character(len=*), parameter :: mod_utils_name_ = 'utils'

  !> Allocate real and integer allocatable ans pointers.
  !! @author Filippo Monari
  !! @param x entity to allocate
  !! @param v source entity
  !! @param m,n,... dimensions (only for entity with dimensions if v is not given)
  !! @param xname character identifying x (for error reporting purposes)
  interface alloc
     module procedure alloc__1
     module procedure alloc__2
     module procedure alloc__3
     module procedure alloc__4
     module procedure alloc__5
     module procedure alloc__6
     module procedure alloc__7
     module procedure alloc__8
     module procedure alloc__9
     module procedure alloc__10
     module procedure alloc__11
     module procedure alloc__12
     module procedure alloc__13
     module procedure alloc__14
     module procedure alloc__15
     module procedure alloc__16
     module procedure alloc__17
     module procedure alloc__18
     module procedure alloc__19
     module procedure alloc__20
     module procedure alloc__21
     module procedure alloc__22
  end interface alloc

  !> Deallocate real and integer allocatable and pointers.
  !! @author Filippo Monari
  !! @param x entity to deallocate
  !! @param xname character identifying x (for error reporting purposes)
  interface dealloc
     module procedure dealloc__1
     module procedure dealloc__2
     module procedure dealloc__3
     module procedure dealloc__4
     module procedure dealloc__5
     module procedure dealloc__6
     module procedure dealloc__7
     module procedure dealloc__8
     module procedure dealloc__9
     module procedure dealloc__10
     module procedure dealloc__11
     module procedure dealloc__12
     module procedure dealloc__13
  end interface dealloc

  !> Given the initialisation value of a pointer variable,
  !! takes the correct action to deallocate it.
  !! @author Filippo Monari
  !! @param[inout] x variable to deallocate
  !! @param[in] init, integer, intialosation value
  !! @param[in] xname character, variable name (for error reporting purposes)
  interface dealloc_element
     module procedure dealloc_element__1
     module procedure dealloc_element__2
     module procedure dealloc_element__3
     module procedure dealloc_element__4
  end interface dealloc_element
     
  !> Append to array of rank 1.
  !! @author Filippo Monari
  !! @param x1 appending array
  !! @param x2 array/scalar to be appended
  !! @param x1name character identifying x1
  interface append_to_array
     module procedure append_to_array__1
     module procedure append_to_array__2
     module procedure append_to_array__3
     module procedure append_to_array__4
     module procedure append_to_array__5
     module procedure append_to_array__6
     module procedure append_to_array__7
     module procedure append_to_array__8
  end interface append_to_array

  !> Find index of the min in an array
  !! @author Filippo Monari
  !! @param arr array
  interface iminloc
     module procedure iminloc__1
     module procedure iminloc__2
  end interface iminloc

  !> Find the index of the max in an array
  !! @author Filippo Monari
  !! @param arr array
  interface imaxloc
     module procedure imaxloc__1
     module procedure imaxloc__2
  end interface imaxloc

  !> Swap real/integer scalar/array, meaning
  !! a goes into b and b goes into a.
  !! @author Filippo Monari
  !! @param{a, b} arrays
  interface swap
     module procedure swap__1
     module procedure swap__2
     module procedure swap__3
  end interface swap
  
contains

  !> Allocates scalar integer pointers with source 'v'
  !! (if missing x is intialised to 0)
  !! @param[inout] x integer, pointer to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v integer, optional, value to assing
  subroutine alloc__1 (x, xname, v)
    implicit none
    integer, intent(inout), pointer :: x
    integer, optional :: v
    character(len=*), intent(in) :: xname
    call do_within('alloc__1', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: w, info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         w = 0
         if (present(v)) w = v
         allocate(x, source=w, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__1
  
  !> Allocates integer pointers of rank 1 with shape '[n]'
  !! @param[inout] x integer(:), pointer to allocate
  !! @param[in] n integer, size of the pointer
  !! @param[in] xname character, name of the variable (for error reporting) 
  subroutine  alloc__2 (x, n, xname)
    implicit none
    integer, intent(inout), pointer :: x(:)
    integer, intent(in) :: n
    character(len=*), intent(in) :: xname
    call do_within('alloc__2', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(n), source=0, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__2
  
  !> Allocates integer pointers of rank 1 with source 'v'.
  !! @param[inout] x integer(:), pointer to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v integer, value to assing 
  subroutine  alloc__3 (x, v, xname) 
    implicit none
    integer, intent(inout), pointer :: x(:)
    integer, intent(in) :: v(:)
    character(len=*), intent(in) :: xname
    call do_within('alloc__3', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__3
  
  !> Allocates integer pointers of rank 2 with shape '[m,n]'.
  !! @param[inout] x integer(:,:), pointer to allocate
  !! @param[in] m integer, fisrt dimension (number of rows)
  !! @param[in] n integer, second dimension (number of columns)
  !! @param[in] xname character, name of the variable (for error reporting)
  subroutine  alloc__4 (x, m, n, xname)
    implicit none
    integer, intent(inout), pointer :: x(:,:)
    integer, intent(in) :: m, n
    character(len=*), intent(in) :: xname
    call do_within('alloc__4', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(m,n), source=0, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__4
  
  !> Allocates integer pointers of rank 2 with source 'v'.
  !! @param[inout] x integer(:,:), pointer to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v integer(:,:), value to assing 
  subroutine  alloc__5 (x, v, xname) 
    implicit none
    integer, intent(inout), pointer :: x(:,:)
    integer, intent(in) :: v(:,:)
    character(len=*), intent(in) :: xname
    call do_within('alloc__5', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__5
  
  !> Allocates double precision scalar pointers with source 'v'
  !! (if missing 'x' is initialised to 0).
  !! @param[inout] x double precision, pointer to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v double precision, optional, value to assing 
  subroutine alloc__6 (x, xname, v)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x
    real(kind=dp_), optional :: v
    character(len=*), intent(in) :: xname
    call do_within('alloc__6', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      real(kind=dp_) :: w
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         w = 0
         if (present(v)) w = v
         allocate(x, source=w, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__6
  
  !> Allocates double precision pointers of rank 1 with shape '[n]'.
  !! @param[inout] x double precision(:), pointer to allocate
  !! @param[in] n insteger, size of the pointer
  !! @param[in] xname character, name of the variable (for error reporting) 
  subroutine  alloc__7 (x, n, xname)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x(:)
    integer, intent(in) :: n
    character(len=*), intent(in) :: xname
    call do_within('alloc__7', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(n), source=0._dp_, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__7
  
  !> Allocates double precision pointers of rank 1 with source 'v'.
  !! @param[inout] x double precision(:), pointer to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v double precision(:), value to assing 
  subroutine  alloc__8 (x, v, xname) 
    implicit none
    real(kind=dp_), intent(inout), pointer :: x(:)
    real(kind=dp_), intent(in) :: v(:)
    character(len=*), intent(in) :: xname
    call do_within('alloc__8', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__8
  
  !> Allocates double precision pointers of rank 2 with shape '[m,n]'.
  !! @param[inout] x double precision(:,:), pointer to allocate
  !! @param[in] m integer, first dimension (rows)
  !! @param[in] n integer, second dimension (columns)
  !! @param[in] xname character, name of the variable (for error reporting) 
  subroutine  alloc__9 (x, m, n, xname)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x(:,:)
    integer, intent(in) :: m, n
    character(len=*), intent(in) :: xname
    call do_within('alloc__9', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(m,n), source=0._dp_, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__9
  
  !> Allocates double precision pointers of rank 2 with source 'v'.
  !! @param[inout] x double precision(:,:), pointer to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v double precision(:,:), value to assing 
  subroutine  alloc__10 (x, v, xname) 
    implicit none
    real(kind=dp_), intent(inout), pointer :: x(:,:)
    real(kind=dp_), intent(in) :: v(:,:)
    character(len=*), intent(in) :: xname
    integer :: info
    call do_within('alloc__10', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call assert(.not. associated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__10
  
  !> Allocates scalar integer allocatables with source 'v'
  !! (if missing 'x' is initialised to 0).
  !! @param[inout] x integer, allocatable to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v integer, optional, value to assing 
  subroutine alloc__11 (x, xname, v)
    implicit none
    integer, intent(inout), allocatable :: x
    integer, optional :: v
    character(len=*), intent(in) :: xname
    call do_within('alloc__11', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: w, info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         w = 0
         if (present(v)) w = v
         allocate(x, source=w, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__11
  
  !> Allocates integer allocatables of rank 1 with shape '[n]'.
  !! @param[inout] x integer(:), allocatable to allocate
  !! @param[in] n integer, size of the allocatable
  !! @param[in] xname character, name of the variable (for error reporting)
  subroutine  alloc__12 (x, n, xname)
    implicit none
    integer, intent(inout), allocatable :: x(:)
    integer, intent(in) :: n
    character(len=*), intent(in) :: xname
    call do_within('lloc_int1_v0', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(n), source=0, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__12
  
  !> Allocates integer allocatables of rank 1 with source 'v'.
  !! @param[inout] x integer(:), allocatable to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v integer(:), value to assing 
  subroutine  alloc__13 (x, v, xname) 
    implicit none
    integer, intent(inout), allocatable :: x(:)
    integer, intent(in) :: v(:)
    character(len=*), intent(in) :: xname
    call do_within('alloc__13', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat=info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__13
  
  !> Allocates integer allocatables of rank 2 with shape '[m,n]'.
  !! @param[inout] x integer(:,:), allocatable to allocate
  !! @param[in] m integer, first dimension (rows)
  !! @param[in] n integer, second dimension (columns)
  !! @param[in] xname character, name of the variable (for error reporting) 
  subroutine  alloc__14 (x, m, n, xname)
    implicit none
    integer, intent(inout), allocatable :: x(:,:)
    integer, intent(in) :: m, n
    character(len=*), intent(in) :: xname
    call do_within('alloc__14', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(m,n), source=0, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__14
  
  !> Allocates integer allocatables of rank 2 with source 'v'.
  !! @param[inout] x integer(:,:), allocatable to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v integer(:,:), value to assing 
  subroutine  alloc__15 (x, v, xname) 
    implicit none
    integer, intent(inout), allocatable :: x(:,:)
    integer, intent(in) :: v(:,:)
    character(len=*), intent(in) :: xname
    call do_within('alloc__15', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__15
  
  !> Allocates double precision allocatables with source 'v'
  !! (optional, if missing 'x' is initialised to 0). 
  !! @param[inout] x double precision, allocatable to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v double precision, optional, value to assing 
  subroutine alloc__16 (x, xname, v)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x
    real(kind=dp_), optional :: v
    character(len=*), intent(in) :: xname
    call do_within('alloc__16', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      real(kind=dp_) :: w
      w = 0
      if (present(v)) w = v
      allocate(x, source=w, stat = info)
      call assert(info == 0, err_alloc_, xname)
    end subroutine private_allocate
  end subroutine alloc__16
  
  !> Allocates double precision allocatables of rank 1 with shape '[n]'.
  !! @param[inout] x double precision(:), allocatable to allocate
  !! @param[in] n integer, size of the allocatable
  !! @param[in] xname character, name of the variable (for error reporting)
  subroutine  alloc__17 (x, n, xname)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x(:)
    integer, intent(in) :: n
    character(len=*), intent(in) :: xname
    call do_within('alloc__17', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(n), source=0._dp_, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__17
  
  !> Allocates double precision allocatables of rank 1 with source 'v'.
  !! @param[inout] x double precision(:), allocatable to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v double precision(:), value to assing 
  subroutine  alloc__18 (x, v, xname) 
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x(:)
    real(kind=dp_), intent(in) :: v(:)
    character(len=*), intent(in) :: xname
    call do_within('alloc__18', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__18
  
  !> Allocates double precision allocatables of rank 2 with shape '[m,n]'.
  !! @param[inout] x double precision(:,:), allocatable to allocate
  !! @param[in] m integer, first dimension (rows)
  !! @param[in] n integer, second dimension (columns)
  !! @param[in] xname character, name of the variable (for error reporting)
  subroutine  alloc__19 (x, m, n, xname)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x(:,:)
    integer, intent(in) :: m, n
    character(len=*), intent(in) :: xname
    call do_within('alloc__19', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(m,n), source=0._dp_, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__19
  
  !> Allocates double precision allocatables of rank 2 with source 'v'.
  !! @param[inout] x double precision(:,:), allocatable to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v double precision(:,:), value to assing 
  subroutine  alloc__20 (x, v, xname) 
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x(:,:)
    real(kind=dp_), intent(in) :: v(:,:)
    character(len=*), intent(in) :: xname
    call do_within('alloc__20', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__20

  !> Allocates double precision allocatables of rank 3 with shape '[m,n,k]'.
  !! @param[inout] x double precision(:,:,:), allocatable to allocate
  !! @param[in] m integer, first dimension
  !! @param[in] n integer, second dimension
  !! @param[in] k integer, thrid dimension
  !! @param[in] xname character, name of the variable (for error reporting)
  subroutine  alloc__21 (x, m, n, k, xname)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x(:,:,:)
    integer, intent(in) :: m, n, k
    character(len=*), intent(in) :: xname
    call do_within('alloc__19', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x(m,n,k), source=0._dp_, stat = info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__21
  
  !> Allocates character allocatables of rank 0 with source 'v'.
  !! @param[inout] x character, allocatable to allocate
  !! @param[in] xname character, name of the variable (for error reporting)
  !! @param[in] v character, value to assing 
  subroutine alloc__22 (x, v, xname)
    implicit none
    character(len=:), intent(inout), allocatable :: x
    character(len=*), intent(in) :: v
    character(len=*), intent(in) :: xname
    call do_within('alloc__21', mod_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      call assert(.not. allocated(x), err_alreadyAlloc_, xname)
      if (err_free()) then
         allocate(x, source=v, stat=info)
         call assert(info == 0, err_alloc_, xname)
      end if
    end subroutine private_allocate
  end subroutine alloc__22

  !> Deallocates integer scalar pointers
  !! @param[inout] x integer, pointer to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__1 (x, xname)
    implicit none
    integer, intent(inout), pointer :: x
    character(len=*), intent(in) :: xname
    call do_within('dealloc__1', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(associated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__1

  !> Deallocates integer pointers of rank 1
  !! @param[inout] x integer(:), pointer to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__2 (x, xname)
    implicit none
    integer, intent(inout), pointer :: x(:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc__2', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(associated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__2

  !> Deallocates integer pointer of rank 2
  !! @param[inout] x integer(:,:), pointer to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__3 (x, xname)
    implicit none
    integer, intent(inout), pointer :: x(:,:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc__3', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(associated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if      
    end subroutine private_deallocate
  end subroutine dealloc__3

  !> Deallocates double precision pointers of rank 0
  !! @param[inout] x double precision, pointer to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__4 (x, xname)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x
    character(len=*), intent(in) :: xname
    call do_within('dealloc__4', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(associated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__4

  !> Deallocates double precision pointers of rank 1
  !! @param[inout] x double precision(:), pointer to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__5 (x, xname)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x(:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc__5', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(associated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__5

  !> Deallocates double precision pointers of rank 2
  !! @param[inout] x double precision(:,:), pointer to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__6 (x, xname)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x(:,:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc_doPtr2', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(associated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__6

  !> Deallocates integer allocatables of rank 0
  !! @param[inout] x integer, allocatable to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__7 (x, xname)
    implicit none
    integer, intent(inout), allocatable :: x
    character(len=*), intent(in) :: xname
    call do_within('dealloc__7', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(allocated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__7

  !> Deallocates integer allocatables of rank 1
  !! @param[inout] x integer(:,:), allocatable to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__8 (x, xname)
    implicit none
    integer, intent(inout), allocatable :: x(:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc__8', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(allocated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__8

  !> Deallocates integer allocatables of rank 2
  !! @param[inout] x integer(:,:), allocatable to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__9 (x, xname)
    implicit none
    integer, intent(inout), allocatable :: x(:,:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc__9', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(allocated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__9

  !> Deallocates double precision allocatables of rank 0
  !! @todo remove?
  subroutine dealloc__10 (x, xname)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x
    character(len=*), intent(in) :: xname
    call do_within('dealloc__10', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(allocated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if      
    end subroutine private_deallocate
  end subroutine dealloc__10

  !> Deallocates double precision allocatables of rank 1
  !! @param[inout] x double precision(:), allocatable to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__11 (x, xname)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x(:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc__11', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(allocated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__11

  !> Deallocates double precision allocatables of rank 2
  !! @param[inout] x double precision(:,:), allocatable to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__12 (x, xname)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x(:,:)
    character(len=*), intent(in) :: xname
    call do_within('dealloc__12', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(allocated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if      
    end subroutine private_deallocate
  end subroutine dealloc__12

  !> Deallocates character allocatables of rank 0
  !! @param[inout] x character, allocatable to deallocate
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc__13 (x, xname)
    implicit none
    character(len=:), intent(inout), allocatable :: x
    character(len=*), intent(in) :: xname
    call do_within('dealloc__13', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: info
      call assert(allocated(x), err_notAlloc_, xname)
      if (err_free()) then
         deallocate(x, stat=info)
         call assert(info == 0, err_dealloc_, xname)
      end if
    end subroutine private_deallocate
  end subroutine dealloc__13  

  !> Deallocates scalar integer pointer elements
  !! @param[inout] x integer, pointer element to deallocate
  !! @param[in] init integer, flag indicating the association type (associated or allocate)
  !! @param[in] xname character, variable name (for error reporting)
  subroutine  dealloc_element__1 (x, init, xname) 
    implicit none
    integer, intent(inout), pointer :: x
    integer, intent(in) :: init
    character(len=*), intent(in) :: xname
    call do_safe_within('dealloc_element1', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      select case (init)
      case (init_alloc_)
         call dealloc(x, xname)
      case (init_assoc_)
         nullify(x)
      case (init_null_)
         continue
      case default
         call raise_error('init', err_unknwnVal_)
      end select
    end subroutine private_deallocate
  end subroutine dealloc_element__1

  !> Deallocates integer pointer elements of rank 1
  !! @param[inout] x integer(:), pointer element to deallocate
  !! @param[in] init integer, flag indicating the association type (associated or allocate)
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc_element__2 (x, init, xname)
    implicit none
    integer, intent(inout), pointer :: x(:)
    integer, intent(in) :: init
    character(len=*), intent(in) :: xname
    call do_safe_within('deallocate_element__2', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      select case (init)
      case (init_alloc_)
         call dealloc(x, xname)
      case (init_assoc_)
         nullify(x)
      case default
         call raise_error('init', err_unknwnVal_)
      end select
    end subroutine private_deallocate
  end subroutine dealloc_element__2

  !> Deallocates scalar double precision pointer elements
  !! @param[inout] x double precision, pointer element to deallocate
  !! @param[in] init integer, flag indicating the association type (associated or allocate)
  !! @param[in] xname character, variable name (for error reporting)
  subroutine dealloc_element__3 (x, init, xname) 
    implicit none
    real(kind=dp_), intent(inout), pointer :: x
    integer, intent(in) :: init
    character(len=*), intent(in) :: xname
    call do_safe_within('dealloc_element__3', mod_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      select case (init)
      case (init_alloc_)
         call dealloc(x, xname)
      case (init_assoc_)
         nullify(x)
      case default
         call raise_error('init', err_unknwnVal_)
      end select
    end subroutine private_deallocate
  end subroutine dealloc_element__3

  !> Deallocates double precision pointer elements of rank 1
  !! @param[inout] x double precision(:), pointer element to deallocate
  !! @param[in] init integer, flag indicating the association type (associated or allocate)
  !! @param[in] xname character, variable name (for error reporting)
  subroutine  dealloc_element__4 (x, init, xname) 
    implicit none
    real(kind=dp_), intent(inout), pointer :: x(:)
    integer, intent(in) :: init
    character(len=*), intent(in) :: xname
    call do_safe_within('dealloc_element__4', mod_utils_name_, private_do)
  contains
    subroutine private_do
      call err_safe(private_deallocate)
    end subroutine private_do
    subroutine private_deallocate
      select case (init)
      case (init_alloc_)
         call dealloc(x, xname)
      case (init_assoc_)
         nullify(x)
      case default
         call raise_error('init', err_unknwnVal_)
      end select
    end subroutine private_deallocate
  end subroutine dealloc_element__4
  
  !> Swaps integer scalars.
  !! @param[inout] a integer
  !! @param[inout] b integer
  subroutine swap__2 (a, b)
    implicit none
    integer, intent (inout) :: a, b
    call do_safe_within('swap__2', mod_utils_name_, private_swap)
  contains
    subroutine private_swap
      integer :: dum
      dum = a
      a = b
      b = dum
    end subroutine private_swap
  end subroutine swap__2

  !> Swaps double precision scalars.
  !! @param[inout] a double precision
  !! @param[inout] b double precision
  subroutine swap__1 (a, b)
    implicit none
    real(kind=dp_), intent (inout) :: a, b
    call do_safe_within('swap__1', mod_utils_name_, private_swap)
  contains
    subroutine private_swap
      real(kind=dp_) :: dum
      dum = a
      a = b
      b = dum
    end subroutine private_swap
  end subroutine swap__1

  !> Swaps double precision variables of rank 1
  !! with equal size.
  !! @param[inout] a double precision(:)
  !! @param[inout] b double precision(:)
  subroutine swap__3 (a, b)
    implicit none
    real(kind=dp_), intent (inout) :: a(:), b(:)
    call do_within('swap__3', mod_utils_name_, private_swap)
  contains
    subroutine private_swap
      real(kind=dp_) :: dum(size(a))
      call assert(size(a) == size(b), err_wrngSz_, 'a or b')
      if (err_free()) dum=a; a=b; b=dum
    end subroutine private_swap
  end subroutine swap__3

  !> Finds the max index in integers of rank 1.
  !! @param[in] arr integer(:)
  integer function imaxloc__1 (arr)
    implicit none
    integer, intent (in) :: arr(:)
    call do_safe_within('imaxloc', mod_utils_name_, private_imax)
  contains
    subroutine private_imax
      integer :: imax(1)
      imax = maxloc(arr(:))
      imaxloc__1 = imax(1)
    end subroutine private_imax
  end function imaxloc__1

  !> Finds the max index in double precision arrays of rank 1.
  !! @param[in] arr double precision(:)
  integer function imaxloc__2 (arr)
    implicit none
    real(kind=dp_), intent (in) :: arr(:)
    call do_safe_within('imaxloc__2', mod_utils_name_, private_imax)
  contains
    subroutine private_imax
      integer :: imax(1)
      imax = maxloc(arr(:))
      imaxloc__2 = imax(1)
    end subroutine private_imax
  end function imaxloc__2

  !> Finds the min index in integers of rank 1.
  !! @param[in] arr integer(:)
  integer function iminloc__1 (arr)
    implicit none
    integer, intent(in) :: arr(:)
    call do_safe_within('iminloc__1', mod_utils_name_, private_iminloc)
  contains
    subroutine private_iminloc
      integer :: imin(1)
      imin = minloc(arr(:))
      iminloc__1 = imin(1)
    end subroutine private_iminloc
  end function iminloc__1

  !> Finds the min index in double precision arrays of rank 1.
  !! @param[in] arr double precision(:)
  integer function iminloc__2 (arr)
    implicit none
    real(kind=dp_), intent (in) :: arr(:)
    call do_safe_within('iminloc__2', mod_utils_name_, private_iminloc)
  contains
    subroutine private_iminloc
      integer :: imin(1)
      imin = minloc(arr(:))
      iminloc__2 = imin(1)
    end subroutine private_iminloc
  end function iminloc__2

  !> Appends integer scalar to allocatable array.
  !! @param[inout] x1 integer(:), allocatable
  !! @param[in] x2 integer, value to append
  !! @param[in] x1name character, x1 name (for error reporting)
  subroutine append_to_array__1 (x1, x2, x1name)
    implicit none
    integer, intent(inout), allocatable :: x1(:)
    integer, intent(in) :: x2
    character(len=*), intent(in) :: x1name
    call do_within('append_to_array__1', mod_utils_name_, private_append)
  contains
    subroutine private_append
      integer, allocatable :: tmp(:)
      if (allocated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [integer::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__1

  !> Appends integer of rank 1 to alloctable array.
  subroutine append_to_array__2 (x1, x2, x1name)
    implicit none
    integer, intent(inout), allocatable :: x1(:)
    character(len=*), intent(in) :: x1name
    integer, intent(in) :: x2(:)
    call do_within('append_to_array__2', mod_utils_name_, private_append)
  contains
    subroutine private_append
      integer, allocatable :: tmp(:)
      if (allocated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [integer::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__2

  !> Appends integer scalar to pointer array.
  subroutine append_to_array__3 (x1, x2, x1name)
    implicit none
    integer, intent(inout), pointer :: x1(:)
    integer, intent(in) :: x2
    character(len=*), intent(in) :: x1name
    call do_within('append_to_array__3', mod_utils_name_, private_append)
  contains
    subroutine private_append
      integer, allocatable :: tmp(:)
      if (associated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [integer::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__3

  !> Appends integer of rank 1 to pointer array.
  subroutine append_to_array__4 (x1, x2, x1name)
    implicit none
    integer, intent(inout), pointer :: x1(:)
    character(len=*), intent(in) :: x1name
    integer, intent(in) :: x2(:)
    call do_within('append_to_array__4', mod_utils_name_, private_append)
  contains
    subroutine private_append
      integer, allocatable :: tmp(:)
      if (associated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [integer::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__4

  !> Appends double precision of rank 0 to alloctable array.
  subroutine append_to_array__5 (x1, x2, x1name)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x1(:)
    real(kind=dp_), intent(in) :: x2
    character(len=*), intent(in) :: x1name
    call do_within('append_to_array__5', mod_utils_name_, private_append)
  contains
    subroutine private_append
      real(kind=dp_), allocatable :: tmp(:)
      if (allocated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [real(kind=dp_)::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__5

  !> Appends double precision of rank 1 to alloctable array.
  subroutine append_to_array__6 (x1, x2, x1name)
    implicit none
    real(kind=dp_), intent(inout), allocatable :: x1(:)
    character(len=*), intent(in) :: x1name
    real(kind=dp_), intent(in) :: x2(:)
    call do_within('append_to_array__6', mod_utils_name_, private_append)
  contains
    subroutine private_append
      real(kind=dp_), allocatable :: tmp(:)
      if (allocated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [real(kind=dp_)::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__6

  !> Appends double precision of rank 0 to pointer array.
  subroutine append_to_array__7 (x1, x2, x1name)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x1(:)
    real(kind=dp_), intent(in) :: x2
    character(len=*), intent(in) :: x1name
    call do_within('append_to_array__7', mod_utils_name_, private_append)
  contains
    subroutine private_append
      real(kind=dp_), allocatable :: tmp(:)
      if (associated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [real(kind=dp_)::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__7

  !> Appends double precision of rank 1 to pointer array.
  subroutine append_to_array__8 (x1, x2, x1name)
    implicit none
    real(kind=dp_), intent(inout), pointer :: x1(:)
    character(len=*), intent(in) :: x1name
    real(kind=dp_), intent(in) :: x2(:)
    call do_within('append_to_array__8', mod_utils_name_, private_append)
  contains
    subroutine private_append
      real(kind=dp_), allocatable :: tmp(:)
      if (associated(x1)) then
         call alloc(tmp, x1, 'tmp')
         call dealloc(x1, x1name)
      else
         call alloc(tmp, [real(kind=dp_)::], 'tmp')
      end if
      call alloc(x1, [tmp, x2], x1name)
    end subroutine private_append
  end subroutine append_to_array__8

  !> Returns the unique elements in a integer array of rank 1.
  !! @author Filippo Monari
  !! @param[in] x integer array of rank 1
  recursive function unique (x) result(ans)
    implicit none
    integer, intent(in) :: x(:)
    integer, allocatable :: ans(:)
    if (size(x) == 0) then
       ans = [integer::]
    else
       ans = [x(1), unique(pack(x, x /= x(1)))]
    end if
  end function unique

  !> Compares to double precision scalar for equality
  !! @author Filippo Monari
  !! @param[in] a,b double precsion scalars
  pure function dpEq (a, b) result(ans)
    implicit none
    real(kind=dp_), intent(in) :: a, b
    logical :: ans
    ans = a - b < eps_dp_
  end function dpEq
  
end module utils
    
    
