module optim_utils

  use env
  use types
  use errwarn
  use registers
  use math
  use nodes
  use utils
  use numbers_utils

  character(len=*), parameter :: mod_optim_utils_name_ = 'optim_utils'

contains
  

  !> Check the graph, the inputs and the outout provided to a optimisation algorithm
  !! @param[in] g integer, graph index in the GRAPH_ array
  !! @param[in] xin integer(:), indexes in the NUMBERS_ array of the input 'numbers'
  !! @param[in] xout integer, index in the NUMBERS_ array of the output 'number'
  !! @todo DEPRECATED?
  subroutine optim_check_gxinout (g, xin, xout)
    implicit none
    integer, intent(in) :: g, xin(:), xout
    call do_safe_within("optim_check_xin", mod_optim_utils_name_, private_check_g)
    call do_safe_within("optim_check_xin", mod_optim_utils_name_, private_check_xinout)
  contains
    subroutine private_check_g
      call assert(g > 0 .and. g < size(GRAPHS_), err_oorng_, 'g')
      call assert(.not. is_empty(GRAPHS_(g)), err_notAlloc_, 'GRAPHS_(g)')
    end subroutine private_check_g
    subroutine private_check_xinout
      integer :: i
      type(number), pointer :: x
      do i = 1, size(xin)
         x => NUMBERS_(xin(i))
         call assert(is_allocated(x), err_notAlloc_, 'xin')
         if (.not. err_free()) exit
      end do
      call assert(is_allocated(NUMBERS_(xout)), err_notAlloc_, 'xout')
      call assert(rank(NUMBERS_(xout)) == 0, err_generic_, 'xout is not a scalar')
    end subroutine private_check_xinout
  end subroutine optim_check_gxinout

  !> Collects all the input values in the given allocatable ('x').
  !! @param[in] xin integer(:), indexes in the NUMBERS_ array of the input 'numbers'
  !! @param[out] x double precision allocatable, variable collecting the values
  subroutine optim_collect_xin (xin, x)
    implicit none
    integer, intent(in) :: xin(:)
    real(kind=dp_), intent(out), allocatable :: x(:)
    integer :: i
    do i = 1, size(xin)
       call append_to_array(x, NUMBERS_(xin(i))%v, 'x')
       if (.not. err_free()) exit
    end do
  end subroutine optim_collect_xin

  !> Collects the input derivative values in the given allocatable ('dx')
  !! @param[in] xin integer(:), indexes in the NUMBERS_ array of the input 'numbers'
  !! @param[out] dx double precision allocatable, variable collecting the values
  subroutine optim_collect_dxin (xin, dx)
    implicit none
    integer, intent(in) :: xin(:)
    real(kind=dp_), intent(out), allocatable :: dx(:)
    integer :: i
    do i = 1, size(xin)
       call append_to_array(dx, NUMBERS_(xin(i))%dv, 'dx')
       if (.not. err_free()) exit
    end do
  end subroutine optim_collect_dxin
  
  !> Same as 'optim_collect_dxin' but normalises the gradient.
  !! @param[in] xin integer(:), index in the NUMBERS_ array of the input 'numbers'
  !! @param[in] dx double precision allocatable, variable collecting the values
  !! @param[in] nrm integer, flag specifying the kind of normalisation
  subroutine optim_collect_dxin_norm (xin, dx, nrm)
    implicit none
    integer, intent(in) :: xin(:), nrm
    real(kind=dp_), intent(out), allocatable :: dx(:)
    call optim_collect_dxin(xin, dx)
    if (nrm > 0) dx = dx / l2norm(dx)
  end subroutine optim_collect_dxin_norm

  !> Initialises to one the values of the output derivative
  !! @param[in] xout integer, index in the NUMBERS_ array of the output 'number'
  !! @todo delete
  subroutine optim_init_bw (xout)
    implicit none
    integer, intent(in) :: xout
    NUMBERS_(xout)%dv = 1
  end subroutine optim_init_bw

  !> Simple input update. Just overwrites the input values with the updates ones.
  !! @param[in] xin integer(:), indexes in the NUMBERS_ array of the input numbers
  !! @param[in] p double precision(:), updated values
  subroutine optim_simple_update (xin, p)
    implicit none
    integer, intent(in) :: xin(:)
    real(kind=dp_), intent(in) :: p(:)
    type(number), pointer :: x
    integer :: i, a, b
    b = 0
    do i = 1, size(xin)
       x => NUMBERS_(xin(i))
       a = b + 1
       b = b + mtsz(x)
       x%v = p(a:b)
    end do
  end subroutine optim_simple_update

  !> Stochastic gradient descent update.
  !! @param[in] xin integer(:), indexes in the NUMBERS_ array of the input 'numbers'
  !! @param[in] lr double precision, learning rate
  subroutine sgd_update (xin, lr)
    implicit none
    integer, intent(in) :: xin(:)
    real(kind=dp_), intent(in) :: lr
    type(number), pointer :: x
    integer :: i
    do i = 1, size(xin)
       x => NUMBERS_(xin(i))
       x%v = x%v - lr * x%dv
    end do
  end subroutine sgd_update

  !> Stochastig gradient descent with momentum update
  !! @param[in] xin integer(:), indexes in the NUMBERS_ array of the input 'numbers'
  !! @param[in] lr double precision, learning rate
  !! @param[in] alpha double precision, weigting factor the exponential moving average
  !! @param[in] v0 double precision(:), moving average values
  subroutine sgdwm_update (xin, lr, alpha, v0)
    implicit none
    integer, intent(in) :: xin(:)!, iter
    real(kind=dp_), intent(in) :: lr, alpha
    real(kind=dp_), intent(inout), target :: v0(:)
    type(number), pointer :: x
    real(kind=dp_), pointer :: vv0(:)
    integer :: i, a, b
    b = 0
    do i = 1, size(xin)
       x => NUMBERS_(xin(i))
       a = b + 1
       b = b + mtsz(x)
       vv0 => v0(a:b)
       vv0 = vv0 * alpha + x%dv * (1 - alpha)
       !vv0 = vv0 / (1 - alpha**real(iter, dp_))
       x%v = x%v - lr * vv0
    end do
  end subroutine sgdwm_update

  !> Adam update.
  !! @param[in] xin integer(:), indexes in the NUMBERS_ array of the input 'numbers'
  !! @param[in] lr double precision, learning rate
  !! @param[in] beta1 double precision, weigting factor the exponential moving average of the 1st moment
  !! @param[in] beta2 double precision, weigting factor the exponential moving average of the 2nd moment
  !! @param[in] m double precision(:), 1st moment moving average
  !! @param[in] v douvle precision(:), 2nd moment moving average
  !! @param[in] iter integer, current iteration number
  subroutine adam_update (xin, lr, beta1, beta2, m, v, iter)
    implicit none
    integer, intent(in) :: xin(:), iter
    real(kind=dp_), intent(in) :: lr, beta1, beta2
    real(kind=dp_), intent(inout), target :: m(:), v(:)
    type(number), pointer :: x
    real(kind=dp_), pointer :: mm(:), vv(:)
    real(kind=dp_) :: alpha
    integer :: i, a, b
    b = 0
    do i = 1, size(xin)
       x => NUMBERS_(xin(i))
       a = b + 1
       b = b + mtsz(x)
       mm => m(a:b)
       vv => v(a:b)
       mm = mm * beta1 + x%dv * (1 - beta1)
       vv = vv * beta2 + x%dv**2 * (1 - beta2)
       alpha = lr * sqrt(1 - beta2**real(iter, dp_)) / (1 - beta1**real(iter, dp_))
       x%v = x%v - alpha * mm / (sqrt(vv) + tol_dp_)
    end do
  end subroutine adam_update

  !> Given a graph calculates and return the objective function value.
  !! @param[in] g integer, graph index in the GRAPHS_ array
  !! @param[in] xout integer, index in the NUMBERS_ array of the output 'number'
  function optim_obj (g, xout) result(ans)
    implicit none
    integer, intent(in) :: g
    type(number), intent(in) :: xout
    real(kind=dp_) :: ans
    call graph__op(g)
    ans = xout%v(1)
  end function optim_obj

  !> Allocates the GOPTS register.
  !! @param[in] n integer, array size
  subroutine allocate_gopts (n)
    implicit none
    integer, intent(in) :: n
    call do_within('allocate_gopts', mod_optim_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      info = 0
      call assert(.not. allocated(GOPTS_), err_alreadyAlloc_, 'GOPTS_')
      if (err_free()) allocate(GOPTS_(n), stat=info)
      call assert(info == 0, err_alloc_, 'GOPTS_')
    end subroutine private_allocate
  end subroutine allocate_gopts

  !> Returns the next free slot in the GOPTS_ register.
  function gopt__next () result(i)
    implicit none
    integer :: i
    i = 0
    call do_safe_within('gopt__next', mod_optim_utils_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(GOPTS_), err_notAlloc_, 'GOPS_')
      call err_safe(private_next)
    end subroutine private_do
    subroutine private_next
      integer :: info
      info = 1
      do while (i <= size(GOPTS_))
         i = i + 1
         if (is_deallocated(GOPTS_(i))) then
            info = 0
            exit
         end if
      end do
      call assert(info == 0, err_generic_, 'GOPTS_ register if full.')
    end subroutine private_next
  end function gopt__next    

  !> Deallocates an 'opt' object
  !! @param[in] x 'opt'
  subroutine gopt__deallocate (x)
    implicit none
    type(gopt), intent(inout) :: x
    call do_safe_within('gopt__deallocate', mod_optim_utils_name_, private_deallocate)
  contains
    subroutine private_deallocate
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call dealloc(x%xin, 'x%in')
      call dealloc(x%v1, 'x%v1')
      call dealloc(x%v2, 'x%v2')
      if (err_free()) then
         x%iter = 0
      end if
    end subroutine private_deallocate
  end subroutine gopt__deallocate

  !> Deallocates the GOPTS_ register
  subroutine deallocate_gopts ()
    implicit none
    call do_safe_within('deallocate_gopts', mod_optim_utils_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(GOPTS_), err_notAlloc_, 'GOPTS_')
      call err_safe(private_deallocate)
    end subroutine private_do
    subroutine private_deallocate
      integer :: i, info
      info = 0
      do i = 1, size(GOPTS_)
         if (is_allocated(GOPTS_(i))) call gopt__deallocate(GOPTS_(i))
      end do
      if (err_free()) deallocate(GOPTS_, stat=info)
      call assert(info == 0, err_dealloc_, 'GOPTS_')
    end subroutine private_deallocate
  end subroutine deallocate_gopts

  ! !> @todo it should return an integer
  ! subroutine gopts (i, ans)
  !   implicit none
  !   integer, intent(in) :: i
  !   type(gopt), intent(inout), pointer :: ans
  !   call do_safe_within('gopts', mod_optim_utils_name_, private_get)
  ! contains
  !   subroutine private_get
  !     call assert(allocated(GOPTS_), err_notAlloc_, 'GOPTS_')
  !     call assert(i > 0 .and. i <= size(GOPTS_), err_oorng_, 'i')
  !     call assert(is_allocated(GOPTS_(i)), err_notAlloc_, 'GOPTS_(i)')
  !     ans => GOPTS_(i)
  !   end subroutine private_get
  ! end subroutine gopts

  !> Checks if a given index is indicaitng a valid optimiser.
  !! Returns the index itself if all checks suceede.
  !! @param[in] i integer, opt index
  function opi (i) result(ans)
    implicit none
    integer, intent(in) :: i
    integer :: ans
    call do_safe_within('opi', mod_optim_utils_name_, private_opi)
  contains
    subroutine private_opi
      call assert(allocated(GOPTS_), err_notAlloc_, 'GOPTS_')
      call assert(i > 0 .and. i <= size(GOPTS_), err_oorng_, 'i')
      call assert(is_allocated(GOPTS_(i)), err_notAlloc_, 'GOPTS_(i)')
      ans = i
    end subroutine private_opi
  end function opi
  
  !> Pops (removes) an 'opt' object from the GOPTS_ register.
  !! @param[in] i integer, 'opt' index
  subroutine gopt__pop (i)
    implicit none
    integer, intent(in) :: i
    call do_safe_within('gopt__pop', mod_optim_utils_name_, private_pop)
  contains
    subroutine private_pop
      call assert(allocated(GOPTS_), err_notAlloc_, 'GOPTS_')
      call assert(i > 0 .and. i <= size(GOPTS_), err_oorng_, 'i')
      call assert(is_allocated(GOPTS_(i)), err_notAlloc_, 'GOPTS_(i)')
      call gopt__deallocate(GOPTS_(i))
    end subroutine private_pop
  end subroutine gopt__pop
    
end module optim_utils
