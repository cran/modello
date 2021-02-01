module numbers_utils

  use env
  use types
  use registers, only: &
       NUMBERS_, &
       INLOCKS_, &
       NUMNDS_
  use errwarn
  use utils
  
  private
  public &
       number__allocate, &
       number__associate, &
       number__deallocate, &
       number__get_init, &
       number__set_v, &
       number__set_dv, &
       number__set_slice_v, &
       number__set_slice_dv, &
       number__set_flat_slice_v, &
       number__set_flat_slice_dv, &
       number__get_v, &
       number__get_dv, &
       number__set_nd, &
       number__get_nd, &
       number__reset_nd, &
       number__inlock, &
       number__inlock_free, &
       number__with_shape, &
       number__slice_indexes, &
       number__slice_dim_indexes, &
       number__take_slice, &
       number__take_flat_slice, &
       number__take_contiguous_slice, &
       number__make_reshape, &
       number__make_drop_shape, &
       number__fill_bind, &
       number__make_bind, &
       higher_shape, &
       lower_shape, &
       get_number, &
       nnn

  character(len=*), parameter :: mod_numbers_utils_name_ = 'numbers_utils'

  !> Allocates a 'number'.
  !! @author Filippo Monari
  !! @param[inout] x 'number' to allocate
  !! @param[in] v optional, source array (rank 0 or 1)
  !! @param[in] shp integer vector, shape
  !! @param[in] dx logical, if .true. the derivative of the is allocated as well
  interface number__allocate
     module procedure number__allocate__1
     module procedure number__allocate__2
     module procedure number__allocate__3
  end interface number__allocate

  !> Sets the value ('x%v') of a 'number'.
  !! @author Filippo Monari
  !! @param[inout] x 'number'
  !! @param[in] v double precision array (rank 0 or 1), value 
  !! @details
  !! If 'v' has rank 1, must be compatible with the 'number' shape. 
  interface number__set_v
     module procedure number__set_v__1
     module procedure number__set_v__2
  end interface number__set_v

  !> Sets the value of the derivative ('x%dx') of a 'number'.
  !! @author Filippo Monari
  !! @param[inout] x class(number)
  !! @param[in] dv value
  interface number__set_dv
     module procedure number__set_dv__1
     module procedure number__set_dv__2
  end interface number__set_dv

  interface number__set_slice_v
     module procedure number__set_slice_v__1
     module procedure number__set_slice_v__2
  end interface number__set_slice_v

  interface number__set_slice_dv
     module procedure number__set_slice_dv__1
     module procedure number__set_slice_dv__2
  end interface number__set_slice_dv

  interface number__set_flat_slice_v
     module procedure number__set_flat_slice_v__1
     module procedure number__set_flat_slice_v__2
  end interface number__set_flat_slice_v

  interface number__set_flat_slice_dv
     module procedure number__set_flat_slice_dv__1
     module procedure number__set_flat_slice_dv__2
  end interface number__set_flat_slice_dv
  
  !> Gives shape to a 'number' by associating to it a pointer
  !! of the correct rank and shape
  !! @author Filippo Monari
  !! @param[in] x 'number'
  !! @param[out] xs pointer of correct type and shape
  !! @param[in] which character, 'v' for shaping 'x%v', 'dv' for shaping 'x%dv'
  !! @details
  !! The correctness of the provided shape of the porvided pointer is not
  !! checked.
  interface number__with_shape
     module procedure number__with_shape__1
     module procedure number__with_shape__2
     ! module procedure number__with_shape__3
     ! module procedure number__with_shape__4
  end interface number__with_shape

  !> Returns the shape of the number with higher rank.
  !! @author Filippo Monari
  !! @param[in] x1,x2 'numbers'
  !! @param[in] shp1 integer vector, shape of x1 (instead of x1 itself).
  interface higher_shape
     module procedure higher_shape__1
     module procedure higher_shape__2
  end interface higher_shape

  !> Returns the shape of the number with lower rank.
  !! @author Filippo Monari
  !! @param[in] x1,x2 'numbers'
  !! @param[in] shp1 integer vector, shape of x1 (instead of x1 itself).
  interface lower_shape
     module procedure lower_shape__1
     module procedure lower_shape__2
  end interface lower_shape

contains

  !> Allocates the 'init' register of a given number.
  !! @author Filipppo Monari
  !! @param[inout] x a 'number'
  subroutine number__allocate_init (x)
    implicit none
    class(number), intent(inout) :: x
    call do_safe_within('number__allocate_init', mod_numbers_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call assert(.not. is_init(x), err_alreadyInit_, 'x')
      call alloc(x%init, number_init_sz_, 'x%init')
    end subroutine private_allocate
  end subroutine number__allocate_init

  !> Given a 'number' and a character identifying an element, sets the
  !! appropriate slot in the 'init' register of the 'number' to
  !! an initialisation value.
  !! @author Filippo Monari
  !! @param[in] x a 'number'
  !! @param[in] k character identifying the element
  !! @param[in] w integer, initialisation value
  subroutine number__set_init (x, k, w)
    implicit none
    type(number), intent(inout) :: x
    character(len=*), intent(in) :: k
    integer, intent(in) :: w
    call do_safe_within('number__set_init', mod_numbers_utils_name_, private_do)
  contains
    subroutine private_do
      call assert(is_init(x), err_notInit_, 'x')
      call assert(.not. is_allocated(x), err_alreadyAlloc_, 'x')
      call assert(any(init_vals_ == w), err_unknwnVal_, 'w')
      call err_safe(private_set_init)
    end subroutine private_do
    subroutine private_set_init
      select case (k)
      case ('shp')
         call assert(x%init(number_init_shpi_) == 0, err_alreadyAlloc_, 'x%shp')
         if (err_free()) x%init(number_init_shpi_) = w
      case ('id')
         call assert(x%init(number_init_idi_) == 0, err_alreadyAlloc_, 'x%id')
         if (err_free()) x%init(number_init_idi_) = w
      case ('v')
         call assert(x%init(number_init_vi_) == 0, err_alreadyAlloc_, 'x%v')
         if (err_free()) x%init(number_init_vi_) = w
      case ('dv')
         call assert(x%init(number_init_dvi_) == 0, err_alreadyAlloc_, 'x%dv') 
         if (err_free()) x%init(number_init_dvi_) = w
      case default
         call raise_error('k', err_unknwnVal_)
      end select
    end subroutine private_set_init
  end subroutine number__set_init

  !> Retrieves the initialisation value stored in the 'init' register
  !! of a number relaive to the element indentified by the provided character.
  !! @author Filippo Monari
  !! @param[in] x 'number'
  !! @param[in] k character indicating the element 
  function number__get_init (x, k) result(ans)
    implicit none
    class(number), intent(inout) :: x
    character(*), intent(in) :: k
    integer :: ans
    call do_safe_within('number__get_init', mod_numbers_utils_name_, private_get_init)
  contains
    subroutine private_get_init
      select case (k)
      case ('shp')
         ans = x%init(number_init_shpi_)
      case('id')
         ans = x%init(number_init_idi_)
      case ('v')
         ans = x%init(number_init_vi_)
      case ('dv')
         ans = x%init(number_init_dvi_)
      case default
         call raise_error('k', err_unknwnVal_)
      end select
    end subroutine private_get_init
  end function number__get_init

  !> Allocates a 'number' with given shape 'shp'.
  subroutine number__allocate__1 (x, shp, dx)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: shp(:)
    logical, intent(in) :: dx
    call do_safe_within('number__allocate__1', mod_numbers_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call assert(.not. is_init(x), err_alreadyInit_, 'x')
      call number__allocate_init(x)
      call alloc(x%shp, shp, 'x%shp')
      call number__set_init(x, 'shp', init_alloc_)
      call alloc(x%id, 'x%id')
      call number__set_init(x, 'id', init_alloc_)
      call alloc(x%v, product(shp), 'x%v')
      call number__set_init(x, 'v',  init_alloc_)
      if (dx) then
         call alloc(x%dv, product(shp), 'x%dv')
      else
         call alloc(x%dv, [real(kind=dp_)::], 'x%dv')
      end if
      call number__set_init(x, 'dv', init_alloc_)
    end subroutine private_allocate
  end subroutine number__allocate__1
  
  !> Allocates a 'number' with given shape 'shp' and as source the scalar 'v'.
  subroutine number__allocate__2 (x, shp, dx, v)
    implicit none
    type(number), intent(inout) :: x
    real(kind=dp_), intent(in) :: v
    integer, intent(in) :: shp(:)
    logical, intent(in) :: dx
    call do_safe_within('number__allocate__2', 'mod_numbers_utils', private_allocate)
  contains
    subroutine private_allocate
      call number__allocate__1(x, shp, dx)
      call number__set_v(x, v)
    end subroutine private_allocate
  end subroutine number__allocate__2

  !> Allocate a 'number' with given shape 'shp' and as source the array 'v'.
  subroutine number__allocate__3 (x, shp, dx, v)
    implicit none
    type(number), intent(inout) :: x
    real(kind=dp_), intent(in) :: v(:)
    integer, intent(in) :: shp(:)
    logical, intent(in) :: dx
    call do_safe_within('number__allocate__4', mod_numbers_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call number__allocate__1(x, shp, dx)
      call number__set_v(x, v)
    end subroutine private_allocate
  end subroutine number__allocate__3

  !> Associates a 'number' to another.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' to associate
  !! @param[in] x2 target 'number'
  subroutine number__associate (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    call do_safe_within('number__associate', mod_numbers_utils_name_, private_associate)
  contains
    subroutine private_associate
      call assert(.not. is_init(x1), err_alreadyInit_, 'x1')
      call assert(is_allocated(x2), err_notAlloc_, 'x2')
      call number__allocate_init(x1)
      if (err_free()) x1%shp => x2%shp
      call number__set_init(x1, 'shp', init_assoc_)
      if (err_free()) x1%id => x2%id
      call number__set_init(x1, 'id', init_assoc_)
      if (err_free()) x1%v => x2%v
      call number__set_init(x1, 'v', init_assoc_)
      if (err_free()) x1%dv => x2%dv
      call number__set_init(x1, 'dv', init_assoc_)
    end subroutine private_associate
  end subroutine number__associate

  !> Deallocates a 'number'
  !! @author Filippo Monari
  !! @param[inout] x 'number' to deallocate.
  subroutine number__deallocate (x)
    implicit none
    type(number), intent(inout) :: x
    call do_safe_within('number__deallocate', mod_numbers_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call dealloc_element(x%shp, number__get_init(x, 'shp'), 'shp')
      call dealloc_element(x%id, number__get_init(x, 'id'), 'id')
      call dealloc_element(x%v, number__get_init(x, 'v'), 'v')
      call dealloc_element(x%dv, number__get_init(x, 'dv'), 'dv')
      call dealloc(x%init, 'x%init')
    end subroutine private_deallocate
  end subroutine number__deallocate

  !> To set 'x%v' to the same scalar value 'v'
  !! @param[inout] x 'number'
  !! @param[in] v double precision, value
  subroutine number__set_v__1 (x, v)
    implicit none
    type(number), intent(inout) :: x
    real(kind=dp_), intent(in) :: v
    call do_safe_within('number__set_v__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      if (err_free()) x%v = v
    end subroutine private_set
  end subroutine number__set_v__1

  !> To set 'x%v' to the vector 'v'
  !! @param[inout] x 'number'
  !! @param[in] double precision(:), value
  subroutine number__set_v__2 (x, v)
    implicit none
    type(number), intent(inout) :: x
    real(kind=dp_), intent(in) :: v(:)
    call do_safe_within('number__set_v__2', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(size(x%v) == size(v), err_wrngSz_, 'v')
      if (err_free()) x%v = v
    end subroutine private_set
  end subroutine number__set_v__2

  !> To set 'x%dv' to the same scalar value 'v'
  !! @param[inout] x 'number'
  !! @param[in] dv double precision, derivative value
  subroutine number__set_dv__1 (x, dv)
    implicit none
    type(number), intent(inout) :: x
    real(kind=dp_), intent(in) :: dv
    call do_safe_within('dual_number__set_dv__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(has_dx(x), err_generic_, 'x has no dx.')
      if (err_free()) x%dv = dv
    end subroutine private_set
  end subroutine number__set_dv__1

  !> To set 'x%dv' to the vector 'v'
  !! @param[inout] x 'number'
  !! @param[in] dv double precision(:), derivative value
  subroutine number__set_dv__2 (x, dv)
    implicit none
    type(number), intent(inout) :: x
    real(kind=dp_), intent(in) :: dv(:)
    call do_safe_within('dual_number__set_dv__2', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(has_dx(x), err_generic_, 'x has no dx.')
      call assert(size(x%v) == size(dv), err_wrngSz_, 'v')
      if (err_free()) x%dv = dv
    end subroutine private_set
  end subroutine number__set_dv__2

  !> Retrieves the value of a 'number'.
  !! @author Filippo Monari
  !! @param[in] x 'number'
  !! @param[out] v real(:), allocatable, variable wherein to store the value
  subroutine number__get_v (x, v)
    implicit none
    type(number), intent(in) :: x
    real(kind=dp_), intent(out), allocatable :: v(:)
    call do_safe_within('number__get_v', mod_numbers_utils_name_, private_get)
  contains
    subroutine private_get
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call alloc(v, x%v, 'v')
    end subroutine private_get
  end subroutine number__get_v

  !> Retrieves the value of the derivative of 'number'.
  !! @author Filippo Monari
  !! @param[in] x 'number'
  !! @param[out] dv real(:) allocatable, variable wherein to store the value of dx
  subroutine number__get_dv (x, dv)
    implicit none
    type(number), intent(in) :: x
    real(kind=dp_), intent(out), allocatable :: dv(:)
    call do_safe_within('number__get_dv', mod_numbers_utils_name_, private_get)
  contains
    subroutine private_get
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(has_dx(x), err_generic_, 'x has no dx.')
      call alloc(dv, x%dv, 'dv')
    end subroutine private_get
  end subroutine number__get_dv

  !> Increases or decreases the inlock count of a 'number'.
  !! @author Filippo Monari
  !! @param[in] x 'number'
  !! @param[in] m character indicating if to increase or to decrease the inlock count
  !! @details
  !! If 'm' is '+' then the inlock count is increased.
  !! If 'm' is '-' then the inlock count is decreased.
  subroutine number__inlock (x, m)
    implicit none
    type(number), intent(in) :: x
    character(len=*), intent(in) :: m
    call do_safe_within('number__inlock', mod_numbers_utils_name_, private_do)
  contains
    subroutine private_do
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call err_safe(private_inlock)
    end subroutine private_do
    subroutine private_inlock
      select case (m)
      case ('+')
         INLOCKS_(x%id) = INLOCKS_(x%id) + 1
      case ('-')
         INLOCKS_(x%id) = INLOCKS_(x%id) - 1
      case default
         call raise_error('m', err_unknwnVal_)
      end select
    end subroutine private_inlock
  end subroutine number__inlock

  !> Checks if for a 'number' the inlock if free (i.e. count = 0).
  !! @author Filippo Monari
  !! @param[in] x 'number'
  function number__inlock_free (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    call do_safe_within('number__inlock_free', mod_numbers_utils_name_, private_inlock_free)
  contains
    subroutine private_inlock_free
      call assert(is_allocated(x), err_notAlloc_, 'x')
      if (err_free()) ans = INLOCKS_(x%id) == 0
    end subroutine private_inlock_free
  end function number__inlock_free

  !> Sets the id corresponding to the 'node' generating the given 'number'.
  !! @author Filippo Monri
  !! @param[in] x 'number'
  !! @param[in] nd integer, 'node' id
  subroutine number__set_nd (x, nd)
    implicit none
    type(number), intent(in) :: x
    integer, intent(in) :: nd
    call do_safe_within('number__set_nd', mod_numbers_utils_name_, private_set_nd)
  contains
    subroutine private_set_nd
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(NUMNDS_(x%id) == 0, err_generic_, 'NUMBER_NODES(x%id) /= 0.')
      if (err_free()) NUMNDS_(x%id) = nd
    end subroutine private_set_nd
  end subroutine number__set_nd

  !> Retrieves the id of the 'node' generating the given 'number'.
  !! @author Filippo Monari
  !! @param[in] x 'number'
  function number__get_nd (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    integer :: ans
    call do_safe_within('number__get_nd', mod_numbers_utils_name_, private_get_nd)
  contains
    subroutine private_get_nd
      call assert(is_allocated(x), err_notAlloc_, 'x')
      ans = NUMNDS_(x%id)
    end subroutine private_get_nd
  end function number__get_nd

  !> Given its id it retrieves a number from the 'NUMBERS_'
  !! register. Returns a pointer to it.
  !! @param[in] i integer, 'number' id
  !! @param[out] 'number', pointer
  subroutine get_number (i, x)
    implicit none
    integer, intent(in) :: i
    type(number), intent(out), pointer :: x
    call do_safe_within('get_number', mod_numbers_utils_name_, private_get)
  contains
    subroutine private_get
      call assert(allocated(NUMBERS_), err_notAlloc_, 'NUMBERS_')
      if (err_free()) call assert(i > 0 .and. i <= size(NUMBERS_), err_oorng_, 'i')
      if (err_free()) call assert(is_allocated(NUMBERS_(i)), err_notAlloc_, 'NUMBERS_(i)')
      if (err_free()) x => NUMBERS_(i)
    end subroutine private_get
  end subroutine get_number  
  
  !> Returns a pointer 'number', given its index (position in 'NUMBERS_').
  !! Handy to pass 'number' arguments with intent(in) to procedure thiorugh
  !! their integer id.
  !! @author Filippo Monari
  !! @param[in] i integer, 'number' index
  function nnn (i) result(ans)
    implicit none
    integer, intent(in) :: i
    type(number), pointer :: ans
    call do_safe_within('nnn', mod_numbers_utils_name_, private_nnn)
  contains
    subroutine private_nnn
      call get_number(i, ans)
    end subroutine private_nnn
  end function nnn

  !> Resets (sets to 0) the 'node' id relative to the given 'number'.
  !! @author Filippo Monari
  !! @param[in] x 'number'
  subroutine number__reset_nd (x)
    implicit none
    type(number), intent(in) :: x
    call do_safe_within('number__reset_nd', mod_numbers_utils_name_, private_reset_nd)
  contains
    subroutine private_reset_nd
      call assert(is_allocated(x), err_notAlloc_, 'x')
      NUMNDS_(x%id) = 0
    end subroutine private_reset_nd
  end subroutine number__reset_nd

  !> Gives shape to rank 2 'number'.
  !! @param[in] x 'number'
  !! @param[out] xs double precision(:,:), pointer
  subroutine number__with_shape__1 (x, xs, dx)
    implicit none
    type(number), intent(in), target :: x
    real(kind=dp_), intent(out), pointer :: xs(:,:)
    logical, intent(in) ::dx
    if (dx) then
       xs(1:x%shp(1),1:x%shp(2)) => x%dv
    else
       xs(1:x%shp(1),1:x%shp(2)) => x%v
    end if
  end subroutine number__with_shape__1
  
  !> !> Gives shape to rank 3 'number'.
  !! @param[in] x 'number'
  !! @param[out] xs double precision(:,:,:), pointer
  subroutine number__with_shape__2 (x, xs, dx)
    implicit none
    type(number), intent(in), target :: x
    real(kind=dp_), intent(out), pointer :: xs(:,:,:)
    logical, intent(in) :: dx
    if (dx) then
       xs(1:x%shp(1),1:x%shp(2),1:x%shp(3)) => x%dv
    else
       xs(1:x%shp(1),1:x%shp(2),1:x%shp(3)) => x%v
    end if
  end subroutine number__with_shape__2

  !> Higher shape between two 'numbers'.
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  function higher_shape__1 (x1, x2) result(shp)
    implicit none
    type(number), intent(in) :: x1, x2
    integer, allocatable :: shp(:)
    call do_safe_within("higher_shape__1", mod_numbers_utils_name_, private_shape)
  contains
    subroutine private_shape
      if (mtrnk(x1) > mtrnk(x2)) then
         call alloc(shp, x1%shp, 'shp')
      else if (mtrnk(x2) > mtrnk(x1)) then
         call alloc(shp, x2%shp, 'shp')
      else if (size(x1%v) > size(x2%v)) then
         call alloc(shp, x1%shp, 'shp')
      else
         call alloc(shp, x1%shp, 'shp')
      end if
    end subroutine private_shape
  end function higher_shape__1
  
  !> Higher shape between a shape vector and a 'number'.
  !! @param[in] shp1 shape vector
  !! @param[in] x2 'number'
  function higher_shape__2 (shp1, x2) result(shp)
    implicit none
    integer, intent(in) :: shp1(:)
    type(number), intent(in) :: x2
    integer, allocatable :: shp(:)
    call do_safe_within("higher_shape__2", mod_numbers_utils_name_, private_shape)
  contains
    subroutine private_shape
      if (size(shp1) > mtrnk(x2)) then
         call alloc(shp, shp1, 'shp')
      else if (mtrnk(x2) > size(shp1)) then
         call alloc(shp, x2%shp, 'shp')
      else if (product(shp1) > size(x2%v)) then
         call alloc(shp, shp1, 'shp')
      else
         call alloc(shp, x2%shp, 'shp')
      end if
    end subroutine private_shape
  end function higher_shape__2

  !> Lower shape between two 'numbers'.
  !! @param[in] x1 'number'
  !! @param[in] x2 'number'
  function lower_shape__1 (x1, x2) result(shp)
    implicit none
    type(number), intent(in) :: x1, x2
    integer, allocatable :: shp(:)
    call do_safe_within("lower_shape__1", mod_numbers_utils_name_, private_shape)
  contains
    subroutine private_shape
      if (mtrnk(x1) < mtrnk(x2)) then
         call alloc(shp, x1%shp, 'shp')
      else if (mtrnk(x2) < mtrnk(x1)) then
         call alloc(shp, x2%shp, 'shp')
      else if (size(x1%v) < size(x2%v)) then
         call alloc(shp, x1%shp, 'shp')
      else
         call alloc(shp, x2%shp, 'shp')
      end if
    end subroutine private_shape
  end function lower_shape__1

  !> Lower shape between a shape vector and a 'number'.
  !! @param[in] shp1 shape vector
  !! @param[in] x2 'number'
  function lower_shape__2 (shp1, x2) result(shp)
    implicit none
    integer, intent(in) :: shp1(:)
    type(number), intent(in) :: x2
    integer, allocatable :: shp(:)
    call do_safe_within("higher_shape__2", mod_numbers_utils_name_, private_shape)
  contains
    subroutine private_shape
      if (size(shp1) < mtrnk(x2)) then
         call alloc(shp, shp1, 'shp')
      else if (mtrnk(x2) < size(shp1)) then
         call alloc(shp, x2%shp, 'shp')
      else if (product(shp1) < size(x2%v)) then
         call alloc(shp, shp1, 'shp')
      else
         call alloc(shp, x2%shp, 'shp')
      end if
    end subroutine private_shape
  end function lower_shape__2

  !> Helper function for slicing 'numbers'.
  !!Given a slice identifying a single value in a 'number',
  !! and the 'number' shape, return the corresponding index in the flat array.
  !! @author Filippo Monari
  !! @param[in] s integer(:), slice
  !! @param[in] shp integer(:), 'number' shape
  function number__slice_index (s, shp) result(ans)
    implicit none
    integer, intent(in) :: s(:), shp(:)
    integer :: ans, i
    ans = s(1) + sum((s(2:size(s)) - 1) * [(product(shp(1:i)), i = 1, size(shp) - 1)])
  end function number__slice_index

  !> Helper function for slicing 'numbers'.
  !! Given a matrix having as columns slices indentifying individual values in a 'number',
  !! returns the corresponding indexes in the flat array.
  !! @author Filippo Monari
  !! @param[in] s integer(:,:), slice matrix
  !! @param[in] shp integer(:), 'number' shape
  function number__slice_indexes (s, shp) result(ans)
    implicit none
    integer, intent(in) :: s(:,:), shp(:)
    integer :: i, ans(size(s, 2))
    call do_safe_within('number__slice_indexes', mod_numbers_utils_name_, private_slice_indexes)
  contains
    subroutine private_slice_indexes
      do i = 1, size(s, 2)
         ans(i) = number__slice_index(s(:,i), shp)
      end do
    end subroutine private_slice_indexes
  end function number__slice_indexes

  subroutine number__slice_dim_indexes (shp, s, ans)
    implicit none
    integer, intent(in) :: shp(:), s
    integer, intent(out) :: ans(product(shp))
    integer :: l1, l2, i, ii, j, k
    l1 = product(shp(1:s)) 
    l2 = product(shp) / l1
    l1 = l1 / shp(s)
    j = 0
    do i = 1, l2
       do k = 1, shp(s)
          do ii = 1, l1
             j = j + 1
             ans(j) = k
          end do
       end do
    end do
  end subroutine number__slice_dim_indexes
  
  !> Helper function for slicing 'numbers'.
  !! Given a matrix having as columns slices indentifying individual
  !! values in a 'number' returns the shape of the resulting slice.
  !! @author Filippo Monari
  !! @param[in] s integer(:,:), slice matrix
  function number__slice_shape (s) result(ans)
    implicit none
    integer, intent(in) :: s(:,:)
    integer :: ans(size(s, 1))
    integer :: i
    do i = 1, size(s, 1)
       ans(i) = size(unique(s(i,:)))
    end do
  end function number__slice_shape

  !> Take a gneric slice from a 'number'
  !! @author Filippo Monari
  !! @param[inout] x1 'number' storing the slice
  !! @param[in] x2 'number' to slice
  !! @param[in] s integer(:,:), slice matrix
  !! @details
  !! Each column of the slice matrix has to identify an individual
  !! value of the 'number'.
  subroutine number__take_slice (x1, x2, s)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: s(:,:)
    call do_safe_within("number_take_slice", mod_numbers_utils_name_, private_slice)
  contains
    subroutine private_slice
      integer, allocatable :: indx(:)
      call assert(is_allocated(x2), err_notAlloc_, "x2")
      call assert(size(s, 1) == mtrnk(x2), err_wrngArg_, 's')
      call assert(all(minval(s, 2) > 0), err_oorng_, 's')
      call assert(all(maxval(s, 2) <= x2%shp), err_oorng_, 's')
      call number__allocate(x1, number__slice_shape(s), has_dx(x2))
      if (err_free()) then
         call alloc(indx, number__slice_indexes(s, x2%shp), 'indx')
         x1%v = x2%v(indx)
         if (has_dx(x2)) x1%dv = x2%dv(indx)
      end if
    end subroutine private_slice
  end subroutine number__take_slice

  subroutine number__set_slice_v__1 (x, s, v)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:,:)
    real(kind=dp_), intent(in) :: v
    call do_safe_within('number__set_lice_v__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(size(s, 1) == mtrnk(x), err_wrngArg_, 's')
      call assert(all(minval(s, 2) > 0), err_oorng_, 's')
      call assert(all(maxval(s, 2) <= x%shp), err_oorng_, 's')
      if (err_free()) x%v(number__slice_indexes(s, x%shp)) = v
    end subroutine private_set
  end subroutine number__set_slice_v__1
  
  subroutine number__set_slice_v__2 (x, s, v)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:,:)
    real(kind=dp_), intent(in) :: v(:)
    call do_safe_within('number__set_slice_v__2', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(size(s, 1) == mtrnk(x), err_wrngArg_, 's')
      call assert(size(s, 2) == size(v), err_wrngArg_, 'v')
      call assert(all(minval(s, 2) > 0), err_oorng_, 's')
      call assert(all(maxval(s, 2) <= x%shp), err_oorng_, 's')
      if (err_free()) x%v(number__slice_indexes(s, x%shp)) = v
    end subroutine private_set
  end subroutine number__set_slice_v__2

  subroutine number__set_slice_dv__1 (x, s, dv)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:,:)
    real(kind=dp_), intent(in) :: dv
    call do_safe_within('number__set_slice_dv__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(has_dx(x), err_generic_, "x has no dx")
      call assert(size(s, 1) == mtrnk(x), err_wrngArg_, 's')
      call assert(all(minval(s, 2) > 0), err_oorng_, 's')
      call assert(all(maxval(s, 2) <= x%shp), err_oorng_, 's')
      if (err_free()) x%dv(number__slice_indexes(s, x%shp)) = dv
    end subroutine private_set
  end subroutine number__set_slice_dv__1
  
  subroutine number__set_slice_dv__2 (x, s, dv)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:,:)
    real(kind=dp_), intent(in) :: dv(:)
    call do_safe_within('number__set_slice_dv__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(has_dx(x), err_generic_, "x has no dx")
      call assert(size(s, 1) == mtrnk(x), err_wrngArg_, 's')
      call assert(size(s, 2) == size(dv), err_wrngArg_, 'sv')
      call assert(all(minval(s, 2) > 0), err_oorng_, 's')
      call assert(all(maxval(s, 2) <= x%shp), err_oorng_, 's')
      if (err_free()) x%dv(number__slice_indexes(s, x%shp)) = dv
    end subroutine private_set
  end subroutine number__set_slice_dv__2
  
  !> Take a flat slice from a 'number' (along the flat array 'x%v')
  !! @author Filippo Monari
  !! @param[inout] x1 'number' storing the slice
  !! @param[in] x2 'number' to slice
  !! @param[in] s integer(:), slice vector
  subroutine number__take_flat_slice (x1, x2, s)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: s(:)
    call do_safe_within("number__take_flat_slice", mod_numbers_utils_name_, private_slice)
  contains
    subroutine private_slice
      call assert(is_allocated(x2), err_notAlloc_, "x2")
      call assert(minval(s) > 0 .and. maxval(s) <= size(x2%v), err_oorng_, 's')
      call number__allocate(x1, [size(s)], has_dx(x2), x2%v(s))
      if (err_free()) then
         if (has_dx(x2)) x1%dv = x2%dv(s)
      end if
    end subroutine private_slice
  end subroutine number__take_flat_slice

  subroutine number__set_flat_slice_v__1 (x, s, v)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:)
    real(kind=dp_), intent(in) :: v
    call do_safe_within('number__set_flat_slice_v__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(minval(s) > 0, err_oorng_, 's')
      call assert(maxval(s) <= size(x%v), err_oorng_, 's')
      if (err_free()) x%v(s) = v
    end subroutine private_set
  end subroutine number__set_flat_slice_v__1

  subroutine number__set_flat_slice_v__2 (x, s, v)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:)
    real(kind=dp_), intent(in) :: v(:)
    call do_safe_within('number__set_flat_slice_dv__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(size(s) == size(v), err_wrngArg_, 'v')
      call assert(minval(s) > 0, err_oorng_, 's')
      call assert(maxval(s) <= size(x%v), err_oorng_, 's')
      if (err_free()) x%v(s) = v
    end subroutine private_set
  end subroutine number__set_flat_slice_v__2

  subroutine number__set_flat_slice_dv__1 (x, s, dv)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:)
    real(kind=dp_), intent(in) :: dv
    call do_safe_within('dual_number__set_dv__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(has_dx(x), err_generic_, 'x has no dx')
      call assert(minval(s) > 0, err_oorng_, 's')
      call assert(maxval(s) <= size(x%v), err_oorng_, 's')
      if (err_free()) x%dv(s) = dv
    end subroutine private_set
  end subroutine number__set_flat_slice_dv__1

  subroutine number__set_flat_slice_dv__2 (x, s, dv)
    implicit none
    type(number), intent(inout) :: x
    integer, intent(in) :: s(:)
    real(kind=dp_), intent(in) :: dv(:)
    call do_safe_within('dual_number__set_dv__1', mod_numbers_utils_name_, private_set)
  contains
    subroutine private_set
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(size(s) == size(dv), err_wrngArg_, 'v')
      call assert(minval(s) > 0, err_oorng_, 's')
      call assert(maxval(s) <= size(x%v), err_oorng_, 's')
      if (err_free()) x%dv(s) = dv
    end subroutine private_set
  end subroutine number__set_flat_slice_dv__2
  
  !> Take a contiguous slice from a number (along the highest rank dimension)
  !! @author Filippo Monari
  !! @param[inout] x1 'number' storing the slice
  !! @param[in] x2 'number' to slice
  !! @param[in] s1,s2 integer, initial and final index of the slice
  !! @details
  !! The slice is realized by means of pointers and no data is copied.
  subroutine number__take_contiguous_slice (x1, x2, s1, s2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: s1, s2
    integer :: init_dv
    call do_safe_within("number_slice", mod_numbers_utils_name_, private_slice)
  contains
    subroutine private_slice
      call assert(.not. is_init(x1), err_alreadyInit_, "x1")
      call assert(is_allocated(x2), err_notAlloc_, "x2")
      call assert(s1 > 0 .and. s2 >= s1 .and. s2 <= x2%shp(mtrnk(x2)), err_oorng_, 's1 or s2')
      call number__allocate_init(x1)
      if (mtrnk(x2) == 1) then
         call err_safe(private_slice_rank1)
      else if (mtrnk(x2) > 1) then 
         call err_safe(private_slice_rankGt1)
      else
         call raise_error("slice implemented only for rank 1 and ran2", err_generic_)
      end if
      call err_safe(private_complete_allocation)
    end subroutine private_slice
    subroutine private_slice_rank1
      x1%v => x2%v(s1:s2)
      if (has_dx(x2)) then
         x1%dv => x2%dv(s1:s2)
         init_dv = init_assoc_
      else 
         init_dv = init_alloc_
      end if
      call alloc(x1%shp, [s2 - s1 + 1], 'x1%shp')
    end subroutine private_slice_rank1
    subroutine private_slice_rankGt1
      logical :: mask(mtrnk(x2))
      integer :: l, a, b, sz
      mask = .true.
      mask(mtrnk(x2)) = .false.
      l = product(pack(x2%shp, mask))
      a = (s1 - 1) * l + 1
      b = s2 * l
      sz = b - a + 1
      x1%v(1:sz) => x2%v(a:b)
      if (has_dx(x2)) then
         x1%dv(1:sz) => x2%dv(a:b)
         init_dv = init_assoc_
      else
         init_dv = init_alloc_
      end if
      call alloc(x1%shp, [pack(x2%shp, mask), s2 - s1 + 1], 'x1%shp')
    end subroutine private_slice_rankGt1
    subroutine private_complete_allocation
      if (init_dv == init_alloc_) call alloc(x1%dv, [real(kind=dp_)::], "x1%dv")
      call number__set_init(x1, "v", init_assoc_)
      call number__set_init(x1, "dv", init_dv)
      call number__set_init(x1, "shp", init_alloc_)
      call alloc(x1%id, "x%id")
      call number__set_init(x1, "id", init_alloc_)
    end subroutine private_complete_allocation  
  end subroutine number__take_contiguous_slice

  !> Make the reshae of a 'number'
  !! @author Filippo Monari
  !! @param[inout] x1 'number' sotoring the reshaping
  !! @param[in] x2 'number' to reshape
  !! @param[in] shp integer(:), new shape
  !! @details
  !! The reshaping is done thorugh pointers and no data is copied.
  subroutine number__make_reshape (x1, x2, shp)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: shp(:)
    call do_safe_within('number__make_reshape', mod_numbers_utils_name_, private_reshape)
  contains
    subroutine private_reshape
      call assert(.not. is_init(x1), err_alreadyInit_, 'x1')
      call assert(is_allocated(x2), err_notAlloc_, 'x2')
      call assert(product(shp) == size(x2%v), err_wrngArg_, 'shp')
      call number__allocate_init(x1)
      call alloc(x1%shp, shp, 'x1%shp')
      call number__set_init(x1, 'shp', init_alloc_)
      call alloc(x1%id, 'x%id')
      call number__set_init(x1, 'id', init_alloc_)
      if(err_free()) then
         x1%v => x2%v
         call number__set_init(x1, 'v',  init_assoc_)
      end if
      if (err_free()) then
         if (has_dx(x2)) then
            x1%dv => x2%dv
            call number__set_init(x1, 'dv',  init_assoc_)
         else
            call alloc(x1%dv, [real(kind=dp_)::], 'x1%dv')
            call number__set_init(x1, 'dv', init_assoc_)
         end if
      end if
    end subroutine private_reshape
  end subroutine number__make_reshape

  !> Drops the dimesions of a number that are equal to 1.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' storing the reshaping
  !! @param[in] x2 'number' to reshape
  !! @details
  !! The reshaping is done thorugh pointers and no data is copied.
  subroutine number__make_drop_shape (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    call do_safe_within('number__make_drop_shape', mod_numbers_utils_name_, private_drop_shape)
  contains
    subroutine private_drop_shape
      call number__make_reshape(x1, x2, pack(x2%shp, x2%shp /= 1))
    end subroutine private_drop_shape
  end subroutine number__make_drop_shape

  !> Binds two numbers togheter along the given dimension.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' storing the bind
  !! @param[in] x2,x3 'numbers' to bind together
  !! @param[in] k integer, dimension along which realise the bind 
  subroutine number__make_bind (x1, x2, x3, k)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2, x3
    integer, intent(in) :: k
    integer :: r
    logical :: mask(mtrnk(x2))
    call do_safe_within('number__make_bind', mod_numbers_utils_name_, private_make_bind)
  contains
    subroutine private_make_bind
      call assert(.not. is_init(x1), err_alreadyInit_, 'x1')
      call assert(is_allocated(x2), err_notAlloc_, 'x2')
      call assert(is_allocated(x3), err_notAlloc_, 'x3')
      call assert(mtrnk(x2) == mtrnk(x3), err_wrngSz_, 'x2 or x3')
      call assert(k <= mtrnk(x2), err_wrngArg_, 'k')
      call assert(has_dx(x2) .eqv. has_dx(x3), err_generic_, 'has_dx(x2) /= has_dx(x3)')
      r = mtrnk(x2)
      mask = .true.
      mask(k) = .false.
      call assert(all(pack(x2%shp, mask) == pack(x3%shp, mask)), err_wrngSz_, 'x2 or x3')
      call err_safe(private_allocate)
      call err_safe(private_bind)
    end subroutine private_make_bind
    subroutine private_bind
      call number__fill_bind(x1%v, x2%v, x3%v, x2%shp, x3%shp, k, .false.)
    end subroutine private_bind
    subroutine private_allocate
      integer :: shp(mtrnk(x2))
      shp = 0
      shp(k) = x3%shp(k)
      call number__allocate(x1, x2%shp + shp, has_dx(x2))
    end subroutine private_allocate
  end subroutine number__make_bind

  !> Helper function to bind 'numbers' together.
  !! Fills the binding 'number' or the binded 'numbers' (depending on the value of 'rev')
  !! with the correct values taken from source/s.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' storing the bind
  !! @param[inout] x2,x3 'numbers' binded together
  !! @param[in] s2 integer, shape of x1
  !! @param[in] s3 integer, shape of x3
  !! @param[in] k integer, dimension along which to do the binding
  !! @param[in] rev logical, if .true. elements flows from the binding to the binded element
  subroutine number__fill_bind (x1, x2, x3, s2, s3, k, rev)
    implicit none
    real(kind=dp_), intent(inout) :: x1(:)
    real(kind=dp_), intent(inout) :: x2(:), x3(:)
    integer, intent(in) :: k, s2(:), s3(:)
    logical, intent(in) :: rev
    integer ::l2, l3
    l2 = number__bind_l(s2, k)
    l3 = number__bind_l(s3, k)
    if (rev) then
       x2 = x1(number__bind_indexes(0, l2, l3, size(x1)))
       x3 = x1(number__bind_indexes(l2, l3, l2, size(x1)))
    else
       x1(number__bind_indexes(0, l2, l3, size(x1))) = x2
       x1(number__bind_indexes(l2, l3, l2, size(x1))) = x3
    end if
  end subroutine number__fill_bind

  !> Helper function to bind 'numbers' together.
  !! Calculates the lag (offset) that is necessary to consider
  !! in binding two 'numbers'
  !! @author Filippo Monari
  !! @param[in] shp integer(:), 'number' shape
  !! @param[in] k integer,  dimension along which to do the binding
  function number__bind_l (shp, k) result(ans)
    implicit none
    integer, intent(in) :: shp(:), k
    integer :: ans
    ans = product(shp(1:k))
  end function number__bind_l

  !> Helper function to bind 'numbers' together.
  !! Calculates the index corresponding to the values of a
  !! binded 'number'.
  !! @author Filippo Monari
  !! @param[in] l0 integer, initial offset
  !! @param[in] l1 integer, step length of the binded number
  !! @param[in] l2 integer, offset due to the second binded 'number'
  !! @param[in] n integer, total length of the bind
  function number__bind_indexes (l0, l1, l2, n) result(ans)
    implicit none
    integer, intent(in) :: l0, l1, l2, n
    integer, allocatable :: ans(:)
    ans = private_indexes(l0)
  contains
    recursive function private_indexes(pl0) result(pans)
      integer, intent(in) :: pl0
      integer, allocatable :: pans(:)
      integer :: i, a, b
      a = pl0 + 1
      b = pl0 + l1
      if (b > n) then
         pans = [integer::]
      else
         pans = [(i, i=a, b), private_indexes(b + l2)]
      end if
    end function private_indexes
  end function number__bind_indexes
  
end module numbers_utils
