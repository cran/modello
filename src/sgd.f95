module sgd

  use env
  use types
  use registers, only: GOPTS_
  use errwarn
  use math
  use nodes 
  use optim_utils

  private

  public &
       sgd__append, &
       sgd_step, &
       sgdwm__append, &
       sgdwm_step, &
       adam__append, &
       adam_step

  character(len=*), parameter :: mod_sgd_name_ = 'sgd' 
  
contains

  !> @defgroup sgd_sgd_ Stochastic Gradient Descent
  !! @{
  subroutine assert_xin_has_dx (xin)
    implicit none
    integer, intent(in) :: xin(:)
    integer :: i
    do i = 1, size(xin)
       call assert(has_dx(nnn(xin(i))), err_generic_, "Gradient descent input has no gradient.")
    end do
  end subroutine assert_xin_has_dx
  
  !> Allocates a sgd optimiser.
  !! @param[inout] x 'opt'
  !! @param[in] xin integer(:), parameter vector with the indexes
  !! of the 'numbers' to optimise
  subroutine sgd__allocate (x, xin)
    implicit none
    type(gopt), intent(inout) :: x
    integer, intent(in) :: xin(:)
    call do_safe_within('sgd__allocate', mod_optim_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call assert(is_deallocated(x), err_alreadyAlloc_, 'x')
      call assert_xin_has_dx(xin)
      call alloc(x%xin, xin, 'x%xin')
      call alloc(x%v1, [real(kind=dp_)::], 'x%v1')
      call alloc(x%v2, [real(kind=dp_)::], 'x%v2')
    end subroutine private_allocate
  end subroutine sgd__allocate

  !> Appends a sgd optimiser to the GOPTS_ register.
  !! @param[out] i integer, 'opt' index
  !! @param[in] xin intger(:), parameter vector with the indexes of the
  !! numbers to optimise
  subroutine sgd__append (i, xin)
    implicit none
    integer, intent(out) :: i
    integer, intent(in) :: xin(:)
    call do_safe_within('sgd__append', mod_optim_utils_name_, private_append)
  contains
    subroutine private_append
      i = gopt__next()
      call sgd__allocate(GOPTS_(i), xin)
    end subroutine private_append
  end subroutine sgd__append

  !> Stochastic gradient descent optimizer step
  !! @param[in] xgopt integer, optmiser index
  !! @param[in] g integer, index of the 'graph' with the computations
  !! @param[in] xout 'number', graph output number
  !! @param[in] lr double precision, learning rate
  !! @param[inout] niter integer, number of steps
  subroutine sgd_step (opti, g, xout, lr, niter, ftrain)
    implicit none
    integer, intent(in) :: opti
    integer, intent(in) :: g
    type(number), intent(in) :: xout
    integer, intent(in) :: niter
    real(kind=dp_), intent(in) :: lr
    real(kind=dp_), intent(out) :: ftrain
    integer :: i
    type(gopt), pointer :: xopt
    xopt => GOPTS_(opti)
    xout%dv = 1
    do i = 1, niter
       xopt%iter = xopt%iter + 1
       ftrain = optim_obj(g, xout)
       call graph__bw_zero(g)
       call graph__bw(g)
       call sgd_update(xopt%xin, lr)
    end do
  end subroutine sgd_step
  !> @}

  !> @defgroup sgd_sgdwm_ Stochastic Gradient Descent With Momentum
  !! @{

  !> Allocate a sgdwm optimiser.
  !! @param[inout] x 'opt'
  !! @param[in] xin integer(:). parameter vector with in
  !! indexes of the 'numbers' to optimise
  subroutine sgdwm__allocate (x, xin)
    implicit none
    type(gopt), intent(inout) :: x
    integer, intent(in) :: xin(:)
    call do_safe_within('sgdwm__allocate', mod_optim_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call assert(is_deallocated(x), err_alreadyAlloc_, 'x')
      call assert_xin_has_dx(xin)
      call alloc(x%xin, xin, 'x%xin')
      call optim_collect_dxin(xin, x%v1)
      call alloc(x%v2, [real(kind=dp_)::], 'x%v2')
    end subroutine private_allocate
  end subroutine sgdwm__allocate

  !> Appends a sgdwm optimiser to the GOPTS_ register.
  !! @param[out] i integer, 'opt' index
  !! @param[in] xin intger(:), parameter vector with the indexes of the
  !! numbers to optimise
  subroutine sgdwm__append (i, xin)
    implicit none
    integer, intent(out) :: i
    integer, intent(in) :: xin(:)
    call do_safe_within('sgdwm__append', mod_optim_utils_name_, private_append)
  contains
    subroutine private_append
      i = gopt__next()
      call sgdwm__allocate(GOPTS_(i), xin)
    end subroutine private_append
  end subroutine sgdwm__append

  !> Stochastic gradient descent with momentum optimizer
  !! @param[in] opti integer, optmiser index
  !! @param[in] g 'graph' with the computations
  !! @param[in] xout 'number', graph output number
  !! @param[in] lr double precision, learning rate
  !! @param[in] alpha double precision, moement parameter
  !! @param[inout] niter integer, number of steps
  subroutine sgdwm_step (opti, g, xout, lr, alpha, niter, ftrain)
    implicit none
    integer, intent(in) :: opti
    integer, intent(in) :: g
    type(number), intent(in) :: xout
    integer, intent(in) :: niter
    real(kind=dp_), intent(in) :: lr, alpha
    real(kind=dp_), intent(out) :: ftrain
    integer :: i
    type(gopt), pointer :: xopt
    xopt => GOPTS_(opti)
    xout%dv = 1
    do i = 1, niter
       xopt%iter = xopt%iter + 1
       ftrain = optim_obj(g, xout)
       call graph__bw_zero(g)
       call graph__bw(g)
       call sgdwm_update(xopt%xin, lr, alpha, xopt%v1)
    end do
  end subroutine sgdwm_step
  !> @}

  !> @defgroup sgd_adam_ Adam
  !! @{

  !> Allocate an Adam optimiser.
  !! @param[inout] x 'opt'
  !! @param[in] xin integer(:). parameter vector with in
  !! indexes of the 'numbers' to optimise
  subroutine adam__allocate (x, xin)
    implicit none
    type(gopt), intent(inout) :: x
    integer, intent(in) :: xin(:)
    call do_safe_within('adam__allocate', mod_optim_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call assert(is_deallocated(x), err_alreadyAlloc_, 'x')
      call assert_xin_has_dx(xin)
      call alloc(x%xin, xin, 'x%xin')
      call optim_collect_dxin(xin, x%v1)
      call alloc(x%v2, x%v1**2, 'x%v2')
    end subroutine private_allocate
  end subroutine adam__allocate

  !> Appends an Adam optimiser to the GOPTS_ register.
  !! @param[out] i integer, 'opt' index
  !! @param[in] xin intger(:), parameter vector with the indexes of the
  !! numbers to optimise
  subroutine adam__append (i, xin)
    implicit none
    integer, intent(out) :: i
    integer, intent(in) :: xin(:)
    call do_safe_within('sgd__append', mod_optim_utils_name_, private_append)
  contains
    subroutine private_append
      i = gopt__next()
      call adam__allocate(GOPTS_(i), xin)
    end subroutine private_append
  end subroutine adam__append
  
  !> Adam optimizer
  !! @param[in] xgopt 'opt', optmiser
  !! @param[in] g 'graph' with the computations
  !! @param[in] xout 'number', graph output number
  !! @param[in] lr double precision, learning rate
  !! @param[in] beta1 double precision, first order moement parameter
  !! @param[in] beta2 double precision, second order moment parameter
  !! @param[inout] niter integer, number of steps
  subroutine adam_step (opti, g, xout, lr, beta1, beta2, niter, ftrain)
    implicit none
    integer, intent(in) :: opti
    integer, intent(in) :: g
    type(number), intent(in) :: xout
    integer, intent(in) :: niter
    real(kind=dp_), intent(in) :: lr, beta1, beta2
    real(kind=dp_), intent(out) :: ftrain
    integer :: i
    type(gopt), pointer :: xopt
    xopt => GOPTS_(opti)
    xout%dv = 1
    do i = 1, niter
       xopt%iter = xopt%iter + 1
       call graph__bw_zero(g)
       call graph__bw(g)
       call adam_update(xopt%xin, lr, beta1, beta2, xopt%v1, xopt%v2, xopt%iter)
       ftrain = optim_obj(g, xout)
    end do
  end subroutine adam_step
  !> @}
  
end module sgd


