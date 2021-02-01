module operators

  use omp_lib
  use env
  use types
  use registers
  use errwarn
  use numbers_utils
  use math

  implicit none

  private

  public &
       op_slice, &
       fw_slice, &
       bw_slice, &
       op_flat_slice, &
       fw_flat_slice, &
       bw_flat_slice, &
       op_bind, &
       fw_bind, &
       bw_bind, &
       op_embeddings, &
       fw_embeddings, &
       bw_embeddings, &
       op_abs, &
       fw_abs, &
       bw_abs, &
       op_exp, &
       fw_exp, &
       bw_exp, &
       op_log, &
       fw_log, &
       bw_log, &
       op_sin, &
       fw_sin, &
       bw_sin, &
       op_cos, &
       fw_cos, &
       bw_cos, &
       op_tan, &
       fw_tan, &
       bw_tan, &
       op_sinh, &
       fw_sinh, &
       bw_sinh, &
       op_cosh, &
       fw_cosh, &
       bw_cosh, &
       op_tanh, &
       fw_tanh, &
       bw_tanh, &
       op_sigmoid, &
       fw_sigmoid, &
       bw_sigmoid, &
       op_relu, &
       fw_relu, &
       bw_relu, &
       op_swish, &
       fw_swish, &
       bw_swish, &
       op_softmax, &
       fw_softmax, &
       bw_softmax, &
       op_elu, &
       fw_elu, &
       bw_elu, &
       op_add, &
       fw_add, &
       bw_add, &
       op_sub, &
       fw_sub, &
       bw_sub, &
       op_mult, &
       fw_mult, &
       bw_mult, &
       op_pow, &
       fw_pow, &
       bw_pow, &
       op_div, &
       fw_div, &
       bw_div, &
       op_bin_entropy, &
       fw_bin_entropy, &
       bw_bin_entropy, &
       op_cross_entropy, &
       fw_cross_entropy, &
       bw_cross_entropy, &
       op_mse, &
       fw_mse, &
       bw_mse, &
       op_mae, &
       fw_mae, &
       bw_mae, &
       op_dgemm, &
       fw_dgemm, &
       bw_dgemm, &
       op_dp_gemv, &
       fw_dp_gemv, &
       bw_dp_gemv, &
       op_dp_ger, &
       bw_dp_ger, &
       op_dp_dot, &
       bw_dp_dot, &
       op_invMat, &
       fw_invMat, &
       bw_invMat, &
       op_sum, &
       fw_sum, &
       bw_sum, &
       op_product, &
       !fw_product, &
       bw_product, &
       op_ssq, &
       fw_ssq, &
       bw_ssq, &
       op_ldexp, &
       fw_ldexp, &
       bw_ldexp, &
       op_ldlaplace, &
       fw_ldlaplace, &
       bw_ldlaplace, &
       op_ldbeta, &
       fw_ldbeta, &
       bw_ldbeta, &
       op_ldgamma, &
       fw_ldgamma, &
       bw_ldgamma, &
       op_ldnorm, &
       fw_ldnorm, &
       bw_ldnorm, &
       op_ldmvnorm__1, &
       bw_ldmvnorm__1, &
       op_lkh_norm, &
       fw_lkh_norm, &
       bw_lkh_norm, &
       op_ksqexp, &
       bw_ksqexp

  character(len=*), parameter :: mod_operators_name_ = 'operators'

  interface op_sum
     module procedure op_sum__1
     module procedure op_sum__2
  end interface op_sum

  interface op_product
     module procedure op_product__1
     module procedure op_product__2
  end interface op_product
 
  interface bw_sum
     module procedure bw_sum__1
     module procedure bw_sum__2
  end interface bw_sum

  interface bw_product
     module procedure bw_product__1
     module procedure bw_product__2
  end interface bw_product
  
  interface op_softmax
     module procedure op_softmax__1
     module procedure op_softmax__2
  end interface op_softmax

  interface fw_softmax
     module procedure fw_softmax__1
     module procedure fw_softmax__2
  end interface fw_softmax

  interface bw_softmax
     module procedure bw_softmax__1
     module procedure bw_softmax__2
  end interface bw_softmax

  interface op_dgemm
     module procedure op_dgemm__1
     module procedure op_dgemm__2
     module procedure op_dgemm__3
     module procedure op_dgemm__4
  end interface op_dgemm

  interface fw_dgemm
     module procedure fw_dgemm__1
     module procedure fw_dgemm__2
     module procedure fw_dgemm__3
     module procedure fw_dgemm__4
  end interface fw_dgemm

  interface bw_dgemm
     module procedure bw_dgemm__1
     module procedure bw_dgemm__2
     module procedure bw_dgemm__3
     module procedure bw_dgemm__4
  end interface bw_dgemm

  interface op_dp_gemv
     module procedure op_dp_gemv__1
     module procedure op_dp_gemv__2
     module procedure op_dp_gemv__3
     module procedure op_dp_gemv__4
  end interface op_dp_gemv

  interface fw_dp_gemv
     module procedure fw_dp_gemv__1
     module procedure fw_dp_gemv__2
     module procedure fw_dp_gemv__3
     module procedure fw_dp_gemv__4
  end interface fw_dp_gemv

  interface bw_dp_gemv
     module procedure bw_dp_gemv__1
     module procedure bw_dp_gemv__2
     module procedure bw_dp_gemv__3
     module procedure bw_dp_gemv__4
  end interface bw_dp_gemv

  interface op_dp_ger
     module procedure op_dp_ger__1
     module procedure op_dp_ger__2
     module procedure op_dp_ger__3
  end interface op_dp_ger

  interface bw_dp_ger
     module procedure bw_dp_ger__1
     module procedure bw_dp_ger__2
     module procedure bw_dp_ger__3
  end interface bw_dp_ger

  interface op_lkh_norm
     module procedure op_lkh_norm__1
     module procedure op_lkh_norm__2
  end interface

  interface fw_lkh_norm
     module procedure fw_lkh_norm__1
     module procedure fw_lkh_norm__2
  end interface fw_lkh_norm

  interface bw_lkh_norm
     module procedure bw_lkh_norm__1
     module procedure bw_lkh_norm__2
  end interface bw_lkh_norm
   
contains

  !> @defgroup operators_ Operators
  !! @{
  
  !> @defgroup operators_modifiers_ Modifiers Operators
  !! @author Filippo Monari
  !! Feed, Slicing, Bind and Embeddings operators.
  !! @{
    
  !> @defgroup operators_modifiers_slice_ General Slice
  !! @{
  
  !> General slice - operator
  !! @param[in   ] x1 'number' input
  !! @param[inout] x2 'number' output
  !! @param[in   ] s integer(:), slice containing the indexes w.r.t. to x1%v
  subroutine op_slice (x1, x2, s)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in), target :: s(:)
    integer, pointer :: ss(:,:)
    ss(1:mtrnk(x1),1:(size(s)/mtrnk(x1))) => s
    x2%v = x1%v(number__slice_indexes(ss, x1%shp))
  end subroutine op_slice

  !> General slice - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  !! @param[in] s integer(:), slice containing the indexes w.r.t. to x1%v
  subroutine fw_slice (x1, x2, s)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in), target :: s(:)
    integer, pointer :: ss(:,:)
    ss(1:mtrnk(x1),1:(size(s)/mtrnk(x1))) => s
    x2%dv = x1%dv(number__slice_indexes(ss, x1%shp))
  end subroutine fw_slice

  !> General slice - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  !! @param[in] s integer(:), slice containing the indexes w.r.t. to x1%v
  subroutine bw_slice (x1, x2, s)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    integer, intent(in), target :: s(:)
    integer, pointer :: ss(:,:)
    ss(1:mtrnk(x1),1:(size(s)/mtrnk(x1))) => s
    x1%dv(number__slice_indexes(ss, x1%shp)) = x2%dv 
  end subroutine bw_slice
  !> @}

  !> @defgroup operators_modifiers_flat_slice_ Flat Slice
  !! @{
  
  !> Flat slice - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  !! @param[in] s integer(:), slice containing the indexes w.r.t. to x1%v
  subroutine op_flat_slice (x1, x2, s)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in) :: s(:)
    x2%v = x1%v(s)
  end subroutine op_flat_slice

  !> Flat slice - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  !! @param[in] s integer(:), slice containing the indexes w.r.t. to x1%v
  subroutine fw_flat_slice (x1, x2, s)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in) :: s(:)
    x2%dv = x1%dv(s)
  end subroutine fw_flat_slice

  !> Flar slice - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  !! @param[in] s integer(:), slice containing the indexes w.r.t. to x1%v
  subroutine bw_flat_slice (x1, x2, s)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in) :: s(:)
    x1%dv(s) = x2%dv 
  end subroutine bw_flat_slice
  !> @}
  
  !> @defgroup operators_modifiers_bind_ Bind
  !! @{

  !> Bind - operator
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  !! @param[in] k integer, dimension along hich to bind
  subroutine op_bind (x1, x2, x3, k)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    integer, intent(in) :: k
    call number__fill_bind(x3%v, x1%v, x2%v, x1%shp, x2%shp, k, .false.)
  end subroutine op_bind

  !> Bind - forward differentiation
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  !! @param[in] k integer, dimension along hich to bind
  subroutine fw_bind (x1, x2, x3, k)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    integer, intent(in) :: k
    call number__fill_bind(x3%dv, x1%dv, x2%dv, x1%shp, x2%shp, k, .false.)
  end subroutine fw_bind

  !> Bind - backward differentiation
  !! @param[inout] x1 'number', input
  !! @param[inout] x2 'number', input
  !! @param[in] x3 'number' output
  !! @param[in] k integer, dimension along hich to bind
  subroutine bw_bind (x1, x2, x3, k)
    implicit none
    type(number), intent(inout) :: x1, x2
    type(number), intent(in) :: x3
    integer, intent(in) :: k
    call number__fill_bind(x3%dv, x1%dv, x2%dv, x1%shp, x2%shp, k, .true.)
  end subroutine bw_bind
  !> @}

  !> @defgroup operators_modifiers_ Ebeddings
  !! @{

  !> Enbeddings - operator
  !! @param[in] f 'number', factors
  !! @param[in] x 'number', embeddings
  !! @param[in] e 'number', embeddings array
  subroutine op_embeddings (f, x, e, t)
    implicit none
    type(number), intent(in) :: f, x
    type(number), intent(inout) :: e
    integer, intent(in) :: t
    real(kind=dp_), pointer :: xx(:,:), ee(:,:)
    call do_within("op_embeddings", mod_operators_name_, private_op)
  contains
    subroutine private_op
      call number__with_shape(x, xx, .false.)
      call number__with_shape(e, ee, .false.)
      if (t > 0) then
         call private_fill2
      else
         call private_fill1
      end if
    end subroutine private_op
    subroutine private_fill1
      integer :: i, j
      !$omp parallel do
      do i = 1, size(f%v)
         j = nint(f%v(i))
         ee(i,:) = xx(:,j) 
      end do
      !$omp end parallel do
    end subroutine private_fill1
    subroutine private_fill2
      integer :: i, j
      !$omp parallel do
      do i = 1, size(f%v)
         j = nint(f%v(i))
         ee(:,i) = xx(:,j) 
      end do
      !$omp end parallel do
    end subroutine private_fill2
  end subroutine op_embeddings

  !> Enbeddings - forward differentiation
  !! @param[in] f 'number', factors
  !! @param[in] x 'number', embeddings
  !! @param[in] e 'number', embeddings array
  subroutine fw_embeddings (f, x, e, t)
    implicit none
    type(number), intent(in) :: f, e
    type(number), intent(inout) :: x
    integer, intent(in) :: t
     real(kind=dp_), pointer :: dx(:,:), de(:,:)
    call do_within("bw_embeddings", mod_operators_name_, private_fw)
  contains
    subroutine private_fw
      call number__with_shape(x, dx, .true.)
      call number__with_shape(e, de, .true.)
      if (t > 0) then
         call private_fill2
      else
         call private_fill1
      end if
    end subroutine private_fw
    subroutine private_fill1
      integer :: i, j
      do i = 1, size(f%v)
         j = nint(f%v(i))
         de(i,:) = dx(:,j)
      end do
    end subroutine private_fill1
    subroutine private_fill2
      integer :: i, j
      do i = 1, size(f%v)
         j = nint(f%v(i))
         de(:,i) = dx(:,j)
      end do
    end subroutine private_fill2
  end subroutine fw_embeddings

  !> Enbeddings - backward differentiation
  !! @param[in] f 'number', factors
  !! @param[in] x 'number', embeddings
  !! @param[in] e 'number', embeddings array
  subroutine bw_embeddings (f, x, e, t)
    implicit none
    type(number), intent(in) :: f, e
    type(number), intent(inout) :: x
    integer :: t
    real(kind=dp_), pointer :: dx(:,:), de(:,:)
    call do_within("bw_embeddings", mod_operators_name_, private_bw)
  contains
    subroutine private_bw
      call number__with_shape(x, dx, .true.)
      call number__with_shape(e, de, .true.)
      if (t > 0) then
         call private_fill2
      else
         call private_fill1
      end if
    end subroutine private_bw
    subroutine private_fill1
      integer :: i, j
      !$omp parallel do
      do i = 1, size(f%v)
         j = nint(f%v(i))
         dx(:,j) = dx(:,j) + de(i,:)
      end do
      !$omp end parallel do
    end subroutine private_fill1
    subroutine private_fill2
      integer :: i, j
      !$omp parallel do
      do i = 1, size(f%v)
         j = nint(f%v(i))
         dx(:,j) = dx(:,j) + de(:,i)
      end do
      !$omp end parallel do
    end subroutine private_fill2
  end subroutine bw_embeddings
  !> @}
  !> @}

  !> Helper function - Reshapes the larger array (h) according to the
  !! size of smaller (m) through a pointer (xh).
  !! The reshaping is done along the leading dimensions (i.e. columns)
  !! Its purpose is to allow broadcast binary operations.
  !! @param[in] h double precision(:), array to reshape 
  !! @param[in] m integer, carachteristc reshaping length
  !! @param[in] xh double precision(:,:), pointer, reshaping pointer
  subroutine op_reshape_bcast (h, m, xh)
    implicit none
    real(kind=dp_), intent(in), target :: h(:)
    integer, intent(in) :: m
    real(kind=dp_), intent(out), pointer :: xh(:,:)
    integer :: n
    n = size(h) / m
    xh(1:m,1:n) => h
  end subroutine op_reshape_bcast
  
 
  !> @defgroup operators_binary_ Binary Operators
  !! Take two inputs and return an output
  !! (e.g. Addition, subtraction, multiplication, power, and division).
  !! @author Filippo Monari
  !! @{
  
  !> @defgroup operators_binary_addition_ Addition
  !! @{

  !> Addition - operator
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine op_add (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (mtrnk(x1) == 0) then
       !$omp parallel workshare
       x3%v = x1%v(1) + x2%v
       !$omp end parallel workshare
    else if (mtrnk(x2) == 0) then
       !$omp parallel workshare
       x3%v = x1%v + x2%v(1)
       !$omp end parallel workshare
    else if (mtsz(x1) == mtsz(x2)) then
       !omp parallel workshare
       x3%v = x1%v + x2%v
       !omp end parallel workshare
    else
       call private_bcast
    end if
  contains
    subroutine private_bcast
      real(kind=dp_), pointer :: xh(:,:), xo(:,:), xl(:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh)
         call op_reshape_bcast(x3%v, mtsz(x1), xo)
         xl => x1%v
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh)
         call op_reshape_bcast(x3%v, mtsz(x2), xo)
         xl => x2%v
      end if
      !omp parallel do
      do i = 1, size(xo, 2)
         xo(:,i) = xh(:,i) + xl
      end do
      !omp end parallel do
    end subroutine private_bcast
  end subroutine op_add

  !> Addition - forward differentiation
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine fw_add (x1, x2, x3)
    type(number) :: x1, x2
    type(number), intent(inout) :: x3
    if (has_dx(x3)) then
       call private_fw_x(x1)
       call private_fw_x(x2)
    end if
  contains
    subroutine private_fw_x (x)
      type(number), intent(in) :: x
      if (has_dx(x)) then
         if (mtrnk(x) == 0) then
            x3%dv = x3%dv + x%dv(1)
         else if (mtsz(x) == mtsz(x3)) then
            x3%dv = x3%dv + x%dv
         else
            call private_bcast(x)
         end if
      end if
    end subroutine private_fw_x
    subroutine private_bcast (x)
      type(number), intent(in) :: x
      real(kind=dp_), pointer :: xo(:,:)
      integer :: i
      call op_reshape_bcast(x3%dv, mtsz(x), xo)
      do i = 1, size(xo, 2)
         xo(:,i) = xo(:,i) + x%dv
      end do
    end subroutine private_bcast  
  end subroutine fw_add

  !> Addition -backward differentiation
  !! @param[inout] x1 'number', input
  !! @param[inout] x2 'number', input
  !! @param[in] x3 'number' output
  subroutine bw_add (x1, x2, x3)
    implicit none
    type(number), intent(inout) :: x1, x2
    type(number), intent(in) :: x3
    if (has_dx(x3)) then
       call private_bw_x(x1)
       call private_bw_x(x2)
    end if
  contains
    subroutine private_bw_x (x)
      type(number), intent(inout) :: x
      if (has_dx(x)) then
         if (mtrnk(x) == 0) then
            !omp parallel workshare
            x%dv = x%dv + sum(x3%dv)
            !omp end parallel workshare
         else if (mtsz(x) == mtsz(x3)) then
            !omp parallel workshare
            x%dv = x%dv + x3%dv
            !omp end parallel workshare
         else
            call private_bcast(x)
         end if
      end if
    end subroutine private_bw_x
    subroutine private_bcast (x)
      type(number), intent(in) :: x
      real(kind=dp_), pointer :: xo(:,:)
      call op_reshape_bcast(x3%dv, mtsz(x), xo)
      !$omp parallel workshare
      x%dv = x%dv + sum(xo, 2)
      !$omp end parallel workshare
    end subroutine private_bcast
  end subroutine bw_add
  !> @}

  !> @defgroup operators_binary_subtraction_ Subtraction
  !! @{

  !> Subtraction - operator
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine op_sub (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (mtrnk(x1) == 0) then
       !$omp parallel workshare
       x3%v = x1%v(1) - x2%v
       !$omp end parallel workshare
    else if (mtrnk(x2) == 0) then
       !$omp parallel workshare
       x3%v = x1%v - x2%v(1)
       !$omp end parallel workshare
    else if (mtsz(x1) == mtsz(x2)) then
       !$omp parallel workshare
       x3%v = x1%v - x2%v
       !$omp end parallel workshare
    else
       call private_bcast 
    end if
  contains
    subroutine private_bcast
      real(kind=dp_), pointer :: xh(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh)
         call op_reshape_bcast(x3%v, mtsz(x1), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xo(:,i) = x1%v - xh(:,i)
         end do
         !$omp end parallel do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh)
         call op_reshape_bcast(x3%v, mtsz(x2), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xo(:,i) = xh(:,i) - x2%v
         end do
         !$omp end parallel do
      end if
    end subroutine private_bcast     
  end subroutine op_sub

  !> Subtraction - forward differentiation
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine fw_sub (x1, x2, x3)
    type(number) :: x1, x2
    type(number), intent(inout) :: x3
    if (has_dx(x3)) then
       call private_fw_x(x1, 1._dp_)
       call private_fw_x(x2, -1._dp_)
    end if
  contains
    subroutine private_fw_x (x, sgn)
      type(number), intent(in) :: x
      real(kind=dp_), intent(in) :: sgn
      if (has_dx(x)) then
         if (mtrnk(x) == 0) then
            x3%dv = x3%dv + x%dv(1) * sgn
         else if (mtsz(x) == mtsz(x3)) then
            x3%dv = x3%dv + x%dv * sgn
         else
            call private_bcast(x, sgn)
         end if
      end if
    end subroutine private_fw_x
    subroutine private_bcast (x, sgn)
      type(number), intent(in) :: x
      real(kind=dp_), intent(in) :: sgn
      real(kind=dp_), pointer :: xo(:,:)
      integer :: i
      call op_reshape_bcast(x3%dv, mtsz(x), xo)
      do i = 1, size(xo, 2)
         xo(:,i) = xo(:,i) + x%dv * sgn
      end do
    end subroutine private_bcast
  end subroutine fw_sub

  !> Subtraction - backeard differentiation
  !! @param[inout] x1 'number', input
  !! @param[inout] x2 'number', input
  !! @param[in] x3 'number' output
  subroutine bw_sub (x1, x2, x3)
    implicit none
    type(number), intent(inout) :: x1, x2
    type(number), intent(in) :: x3
    if (has_dx(x3)) then
       call private_bw_x(x1, 1._dp_)
       call private_bw_x(x2, -1._dp_)
    end if
  contains
    subroutine private_bw_x (x, sgn)
      type(number), intent(inout) :: x
      real(kind=dp_), intent(in) :: sgn
      if (has_dx(x)) then
         if (mtrnk(x) == 0) then
            !$omp parallel workshare
            x%dv = x%dv(1) + sum(x3%dv) * sgn
            !$omp end parallel workshare
         else if (mtsz(x) == mtsz(x3)) then
            !$omp parallel workshare
            x%dv = x%dv + x3%dv * sgn
            !$omp end parallel workshare
         else
            call private_bcast(x, sgn)
         end if
      end if
    end subroutine private_bw_x
    subroutine private_bcast (x, sgn)
      type(number), intent(in) :: x
      real(kind=dp_), intent(in) :: sgn
      real(kind=dp_), pointer :: xo(:,:)
      call op_reshape_bcast(x3%dv, mtsz(x), xo)
      !$omp parallel workshare
      x%dv = x%dv + sum(xo, 2) * sgn
      !$omp end parallel workshare
    end subroutine private_bcast
  end subroutine bw_sub
  !> @}

  !> @defgroup operators_binary_multiplication_ Multiplication
  !! @{

  !> Multiplication - operator
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine op_mult (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (mtrnk(x1) == 0) then
       !$omp parallel workshare
       x3%v = x1%v(1) * x2%v
       !$omp end parallel workshare
    else if (mtrnk(x2) == 0) then
       !omp parallel wprkshare
       x3%v = x1%v * x2%v(1)
       !omp end parallel workshare
    else if (mtsz(x1) == mtsz(x2)) then
       !omp parallel workshare
       x3%v = x1%v * x2%v
       !omp end parallel workshare
    else
       call private_bcast
    end if
  contains
    subroutine private_bcast
      real(kind=dp_), pointer :: xh(:,:), xl(:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh)
         call op_reshape_bcast(x3%v, mtsz(x1), xo)
         xl => x1%v
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh)
         call op_reshape_bcast(x3%v, mtsz(x2), xo)
         xl => x2%v
      end if
      !$omp parallel do
      do i = 1, size(xo, 2)
         xo(:,i) = xh(:,i) * xl
      end do
      !$omp end parallel do
    end subroutine private_bcast
  end subroutine op_mult

  !> Multiplication - forward differentiation
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine fw_mult (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (has_dx(x3)) then
       call private_fw_x(x1, x2)
       call private_fw_x(x2, x1)
    end if
  contains
    subroutine private_fw_x (a, b)
      type(number), intent(in) :: a, b
      if (has_dx(a)) then
         if (mtrnk(a) == 0) then
            !$omp parallel workshare
            x3%dv = x3%dv + a%dv(1) * b%v
            !$omp end parallel workshare
         else if (mtrnk(b) == 0) then
            !$omp parallel workshare
            x3%dv = x3%dv + a%dv * b%v(1)
            !$omp end parallel workshare
         else if (mtsz(x1) == mtsz(x2)) then
            !$omp parallel workshare
            x3%dv = x3%dv + a%dv * b%v
            !$omp end parallel workshare
         else
            call private_bcast(a, b)
         end if
      end if
    end subroutine private_fw_x
    subroutine private_bcast (a, b)
      type(number), intent(in) :: a, b
      real(kind=dp_), pointer :: xh(:,:), xl(:), xo(:,:)
      integer :: i
      if (mtsz(a) < mtsz(b)) then
         call op_reshape_bcast(b%v, mtsz(a), xh)
         call op_reshape_bcast(x3%dv, mtsz(a), xo)
         xl => a%dv
      else
         call op_reshape_bcast(a%dv, mtsz(b), xh)
         call op_reshape_bcast(x3%dv, mtsz(b), xo)
         xl => b%v
      end if
      !$omp parallel do
      do i = 1, size(xo, 2)
         xo(:,i) = xo(:,i) + xh(:,i) * xl
      end do
      !$omp end parallel do
    end subroutine private_bcast
  end subroutine fw_mult

  !> Multiplication - backward differentiation
  !! @param[inout] x1 'number', input
  !! @param[inout] x2 'number', input
  !! @param[in] x3 'number' output
  subroutine bw_mult (x1, x2, x3)
    implicit none
    type(number), intent(inout) :: x1, x2
    type(number), intent(in) :: x3
    if (has_dx(x3)) then
       call private_bw_x(x1, x2)
       call private_bw_x(x2, x1)
    end if
  contains
    subroutine private_bw_x (a, b)
      type(number), intent(inout) :: a
      type(number), intent(in) :: b
      if (has_dx(a)) then
         if (mtrnk(a) == 0) then
            !$omp parallel workshare
            a%dv = a%dv(1) + sum(b%v * x3%dv)
            !$omp end parallel workshare
         else if (mtrnk(b) == 0) then
            !$omp parallel workshare
            a%dv = a%dv + b%v(1) * x3%dv
            !$omp end parallel workshare
         else if (mtsz(a) == mtsz(b)) then
            !$omp parallel workshare
            a%dv = a%dv + b%v * x3%dv
            !$omp end parallel workshare
         else
            call private_bcast(a, b)
         end if
      end if
    end subroutine private_bw_x
    subroutine private_bcast (a, b)
      type(number), intent(in) :: a, b
      real(kind=dp_), pointer :: xh(:,:), xo(:,:)
      integer :: i
      if (mtsz(a) < mtsz(b)) then
         call op_reshape_bcast(b%v, mtsz(a), xh)
         call op_reshape_bcast(x3%dv, mtsz(a), xo)
         !$omp parallel workshare
         a%dv = a%dv + sum(xh * xo, 2)
         !$omp end parallel workshare
      else
         call op_reshape_bcast(a%dv, mtsz(b), xh)
         call op_reshape_bcast(x3%dv, mtsz(b), xo)
         do i = 1, size(xh, 2)
            !$omp parallel workshare
            xh(:,i) = xh(:,i) + xo(:,i) * b%v
            !$omp end parallel workshare
         end do
      end if
    end subroutine private_bcast
  end subroutine bw_mult
  !> @}

  !> @defgroup operators_binary_power_ Power
  !! @{

  !> Power - operator
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine op_pow (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (mtrnk(x1) == 0) then
       !$omp parallel workshare
       x3%v = x1%v(1) ** x2%v
       !$omp end parallel workshare
    else if (mtrnk(x2) == 0) then
       !$omp parallel workshare
       x3%v = x1%v ** x2%v(1)
       !$omp end parallel workshare
    else if (mtsz(x1) == mtsz(x2)) then
       !$omp parallel workshare
       x3%v = x1%v ** x2%v
       !$omp end parallel workshare
    else
       call private_bcast
    end if
  contains
    subroutine private_bcast
      real(kind=dp_), pointer :: xh(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh)
         call op_reshape_bcast(x3%v, mtsz(x1), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xo(:,i) = x1%v ** xh(:,i)
         end do
         !$omp end parallel do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh)
         call op_reshape_bcast(x3%v, mtsz(x2), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xo(:,i) = xh(:,i) ** x2%v
         end do
         !$omp end parallel do
      end if
    end subroutine private_bcast
  end subroutine op_pow

  !> Power - forward differentiation
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine fw_pow (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (has_dx(x3)) then
       call private_fw_x1
       call private_fw_x2
    end if
  contains
    subroutine private_fw_x1
      if (has_dx(x1)) then
         if (mtrnk(x1) == 0) then
            x3%dv = x3%dv + x1%dv(1) * dx_xpow(x1%v(1), x2%v)
         else if (mtrnk(x2) == 0) then
            x3%dv = x3%dv + x1%dv * dx_xpow(x1%v, x2%v(1))
         else if (mtsz(x1) == mtsz(x2)) then
            x3%dv = x3%dv + x1%dv * dx_xpow(x1%v, x2%v)
         else
            call private_bcast_x1
         end if
      end if
    end subroutine private_fw_x1
    subroutine private_bcast_x1
      real(kind=dp_), pointer :: xh1(:,:), xh2(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh1)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + x1%dv * dx_xpow(x1%v, xh1(:,i))
         end do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh1)
         call op_reshape_bcast(x1%dv, mtsz(x2), xh2)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + xh2(:,i) * dx_xpow(xh1(:,i), x2%dv)
         end do
      end if
    end subroutine private_bcast_x1
    subroutine private_fw_x2
      if (has_dx(x2)) then
         if (mtrnk(x1) == 0) then
            x3%dv = x3%dv + x2%dv * dx_powx(x1%v(1), x2%v)
         else if (mtrnk(x2) == 0) then
            x3%dv = x3%dv + x2%dv(1) * dx_powx(x1%v, x2%v(1))
         else if (mtsz(x1) == mtsz(x2)) then
            x3%dv = x3%dv + x2%dv * dx_powx(x1%v, x2%v)
         else
            call private_bcast_x2
         end if
      end if
    end subroutine private_fw_x2
    subroutine private_bcast_x2
      real(kind=dp_), pointer :: xh1(:,:), xh2(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh1)
         call op_reshape_bcast(x2%dv, mtsz(x1), xh2)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + xh2(:,i) * dx_xpow(x1%v, xh1(:,i))
         end do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh1)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + x2%dv * dx_xpow(xh1(:,i), x2%dv)
         end do
      end if
    end subroutine private_bcast_x2
  end subroutine fw_pow

  !> Power - backward differentiation
  !! @param[inout] x1 'number', input
  !! @param[inout] x2 'number', input
  !! @param[in] x3 'number' output
  subroutine bw_pow (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (has_dx(x3)) then
       call private_bw_x1
       call private_bw_x2
    end if
  contains
    subroutine private_bw_x1
      if (has_dx(x1)) then
         if (mtrnk(x1) == 0) then
            !$omp parallel workshare
            x1%dv = x1%dv(1) + sum(x3%dv * dx_xpow(x1%v(1), x2%v))
            !$omp end parallel workshare
         else if (mtrnk(x2) == 0) then
            !$omp parallel workshare
            x1%dv = x1%dv + x3%dv * dx_xpow(x1%v, x2%v(1))
            !$omp end parallel workshare
         else if (mtsz(x1) == mtsz(x2)) then
            !$omp parallel workshare
            x1%dv = x1%dv + x3%dv * dx_xpow(x1%v, x2%v)
            !$omp end parallel workshare
         else
            call private_bcast_x1
         end if
      end if
    end subroutine private_bw_x1
    subroutine private_bcast_x1
      real(kind=dp_), pointer :: xh1(:,:), xh2(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh1)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         do i = 1, size(xo, 2)
            !$omp parallel workshare
            x1%dv = x1%dv + xo(:,i) * dx_xpow(x1%v, xh1(:,i))
            !$omp end parallel workshare
         end do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh1)
         call op_reshape_bcast(x1%dv, mtsz(x2), xh2)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xh2(:,i) = xh2(:,i) + xo(:,i) * dx_xpow(xh1(:,i), x2%v)
         end do
         !$omp end parallel do
      end if
    end subroutine private_bcast_x1
    subroutine private_bw_x2
      if (has_dx(x2)) then
         if (mtrnk(x1) == 0) then
            !$omp parallel workshare
            x2%dv = x2%dv + x3%dv * dx_powx(x1%v(1), x2%v)
            !$omp end parallel workshare
         else if (mtrnk(x2) == 0) then
            !$omp parallel workshare
            x2%dv = x2%dv(1) + sum(x3%dv * dx_powx(x1%v, x2%v(1)))
            !$omp end parallel workshare
         else if (mtsz(x1) == mtsz(x2)) then
            !$omp parallel workshare
            x2%dv = x2%dv + x3%dv * dx_powx(x1%v, x2%v)
            !$omp end parallel workshare
         else
            call private_bcast_x2
         end if
      end if
    end subroutine private_bw_x2
    subroutine private_bcast_x2
      real(kind=dp_), pointer :: xh1(:,:), xh2(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh1)
         call op_reshape_bcast(x2%dv, mtsz(x1), xh2)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xh2(:,i) = xh2(:,i) + xo(:,i) * dx_powx(x1%v, xh1(:,i))
         end do
         !$omp end parallel do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh1)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         do i = 1, size(xo, 2)
            !$omp parallel workshare
            x2%dv = x2%dv + xo(:,i) * dx_powx(xh1(:,i), x2%v)
            !$omp end parallel workshare
         end do
      end if
    end subroutine private_bcast_x2
  end subroutine bw_pow
  !> @}

  !> @defgroup operators_binary_division_ Division
  !! @{

  !> Division - operator
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine op_div (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (mtrnk(x1) == 0) then
       !$omp parallel workshare
       x3%v = x1%v(1) / x2%v
       !$omp end parallel workshare
    else if (mtrnk(x2) == 0) then
       !$omp parallel workshare
       x3%v = x1%v / x2%v(1)
       !$omp end parallel workshare
    else if (mtsz(x1) == mtsz(x2)) then
       !$omp parallel workshare
       x3%v = x1%v / x2%v
       !$omp end parallel workshare
    else
       call private_bcast
    end if
  contains
    subroutine private_bcast
      real(kind=dp_), pointer :: xh(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh)
         call op_reshape_bcast(x3%v, mtsz(x1), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xo(:,i) = x1%v / xh(:,i) 
         end do
         !$omp end parallel do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh)
         call op_reshape_bcast(x3%v, mtsz(x2), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xo(:,i) = xh(:,i) / x2%v 
         end do
         !$omp end parallel do
      end if
    end subroutine private_bcast
  end subroutine op_div

  !> Division - forward differentiation
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[inout] x3 'number' output
  subroutine fw_div (x1, x2, x3)
    implicit none
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: x3
    if (has_dx(x3)) then
       call private_fw_x1
       call private_fw_x2
    end if
  contains
    subroutine private_fw_x1
      if (has_dx(x1)) then
         if (mtrnk(x1) == 0) then
            x3%dv = x3%dv + x1%dv(1) * x2%v ** (-1._dp_)
         else if (rank(x2) == 0) then
            x3%dv = x3%dv + x1%dv * x2%v(1) ** (-1._dp_)
         else if (mtsz(x1) == mtsz(x2)) then
            x3%dv = x3%dv + x1%dv * x2%v ** (-1._dp_)
         else
            call private_bcast_x1
         end if
      end if
    end subroutine private_fw_x1
    subroutine private_bcast_x1
      real(kind=dp_), pointer :: xh(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + x1%dv * xh(:,i) ** (-1._dp_)
         end do
      else
         call op_reshape_bcast(x1%dv, mtsz(x2), xh)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + xh(:,i) * x2%v ** (-1._dp_)
         end do
      end if
    end subroutine private_bcast_x1
    subroutine private_fw_x2
      if (has_dx(x2)) then
         if (mtrnk(x1) == 0) then
            x3%dv = x3%dv + x2%dv * x1%v(1) * dx_xpow(x2%v, -1._dp_)
         else if (mtrnk(x2) == 0) then
            x3%dv = x3%dv + x2%dv(1) * x1%v * dx_xpow(x2%v(1), -1._dp_)
         else if (mtsz(x1) == mtsz(x2)) then
            x3%dv = x3%dv + x2%dv * x1%v * dx_xpow(x2%v, -1._dp_)
         else
            call private_bcast_x2
         end if
      end if
    end subroutine private_fw_x2
    subroutine private_bcast_x2
      real(kind=dp_), pointer :: xh1(:,:), xh2(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh1)
         call op_reshape_bcast(x2%dv, mtsz(x1), xh2)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + xh2(:,i) + x1%v * dx_xpow(xh1(:,i), -1._dp_)
         end do
      else
         call op_reshape_bcast(x1%dv, mtsz(x2), xh1)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         do i = 1, size(xo, 2)
            xo(:,i) = xo(:,i) + x2%dv * xh1(:,i) * dx_xpow(x2%v, -1._dp_)
         end do
      end if
    end subroutine private_bcast_x2
  end subroutine fw_div

  !> Division - backward differentiation
  !! @param[inout] x1 'number', input
  !! @param[inout] x2 'number', input
  !! @param[in] x3 'number' output
  subroutine bw_div (x1, x2, x3)
    implicit none
    type(number), intent(inout) :: x1, x2
    type(number), intent(in) :: x3
    if (has_dx(x3)) then
       call private_bw_x1
       call private_bw_x2
    end if
  contains
    subroutine private_bw_x1
      if (has_dx(x1)) then
         if (mtrnk(x1) == 0) then
            !$omp parallel workshare
            x1%dv = x1%dv(1) + sum(x2%v ** (-1._dp_) * x3%dv)
            !$omp end parallel workshare
         else if (mtrnk(x2) == 0) then
            !$omp parallel workshare
            x1%dv = x1%dv + x2%v(1) ** (-1._dp_) * x3%dv
            !$omp end parallel workshare
         else if (mtsz(x1) == mtsz(x2)) then
            !$omp parallel workshare
            x1%dv = x1%dv + x2%v ** (-1._dp_) * x3%dv
            !$omp end parallel workshare
         else
            call private_bcast_x1
         end if
      end if
    end subroutine private_bw_x1
    subroutine private_bcast_x1
      real(kind=dp_), pointer :: xh(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         !$omp parallel workshare
         x1%dv = x1%dv + sum(xh ** (-1._dp_) * xo, 2)
         !$omp end parallel workshare
      else
         call op_reshape_bcast(x1%dv, mtsz(x2), xh)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xh(:,i) = xh(:,i) + x2%v ** (-1._dp_) * xo(:,i)
         end do
         !$omp end parallel do
      end if
    end subroutine private_bcast_x1
    subroutine private_bw_x2
      if (has_dx(x2)) then
         if (mtrnk(x1) == 0) then
            !$omp parallel workshare
            x2%dv = x2%dv + x1%v(1) * dx_xpow(x2%v, -1._dp_) * x3%dv
            !$omp end parallel workshare
         else if (mtrnk(x2) == 0) then
            !$omp parallel workshare
            x2%dv(1) = x2%dv(1) + sum(x1%v * dx_xpow(x2%v(1), -1._dp_) * x3%dv)
            !$omp end parallel workshare
         else if (mtsz(x1) == mtsz(x2)) then
            !$omp parallel workshare
            x2%dv = x2%dv + x1%v * dx_xpow(x2%v, -1._dp_) * x3%dv
            !$omp end parallel workshare
         else
            call private_bcast_x2
         end if
      end if
    end subroutine private_bw_x2
    subroutine private_bcast_x2
      real(kind=dp_), pointer :: xh1(:,:), xh2(:,:), xo(:,:)
      integer :: i
      if (mtsz(x1) < mtsz(x2)) then
         call op_reshape_bcast(x2%v, mtsz(x1), xh1)
         call op_reshape_bcast(x2%dv, mtsz(x1), xh2)
         call op_reshape_bcast(x3%dv, mtsz(x1), xo)
         !$omp parallel do
         do i = 1, size(xo, 2)
            xh2(:,i) = xh2(:,i) + x1%v * dx_xpow(xh1(:,i), -1._dp_) * xo(:,i)
         end do
         !$omp end parallel do
      else
         call op_reshape_bcast(x1%v, mtsz(x2), xh1)
         call op_reshape_bcast(x3%dv, mtsz(x2), xo)
         !$omp parallel workshare
         x2%dv = x2%dv + dx_xpow(x2%v, -1._dp_) * sum(xh1  * xo, 2)
         !$omp end parallel workshare
      end if
    end subroutine private_bcast_x2
  end subroutine bw_div
  !> @}

  !> @defgroup operators_binary_elu_ Exponential Linera Unit (ELU)
  !! @{
  
  !> ELU - operator
  !! @param[in] x1 'number', input
  !! @param[inout] x2 'number', output
  !! @param[in] a 'number', parameter
  subroutine op_elu (x1, x2, a)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    type(number), intent(in) :: a
    !$omp parallel workshare
    x2%v = elu(x1%v, a%v(1))
    !$omp end parallel workshare
  end subroutine op_elu

  !> ELU - forward differentiation
  !! @param[in] x1 'number', input
  !! @param[inout] x2 'number', output
  !! @param[in] a 'number', parameter
  subroutine fw_elu (x1, x2, a)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    type(number), intent(in) :: a
    if (has_dx(x2)) x2%dv = x1%dv * dx_elu(x1%v, a%v(1))
  end subroutine fw_elu

  !> ELU - backward differentiation
  !! @param[inout] x1 'number', input
  !! @param[in] x2 'number', output
  !! @param[in] a 'number', parameter
  subroutine bw_elu (x1, x2, a)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    type(number), intent(in) :: a
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_elu(x1%v, a%v(1)) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_elu
  !> @}
  !> @}
  
  !> @defgroup operators_unary_ Unary Operators
  !! Take one input and return one output
  !! (e.g. Exponential, logarithm, sin, cos, tan, sinh, cosh, tanh,
  !! Softmax, and sigmoid).
  !! @author Filippo Monari
  !! @{

  !> @defgroup operators_unary_abs_ Abs
  !! @{

  !> Abs - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_abs (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = abs(x1%v)
    !$omp end parallel workshare
  end subroutine op_abs

  !> Abs - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_abs (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = dx_abs(x1%v) * x1%dv
  end subroutine fw_abs

  !> Abs - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_abs (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_abs(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_abs
  
  !> @defgroup operators_unary_exp_ Exponential
  !! @{

  !> Exponential - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_exp (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = exp(x1%v)
    !$omp end parallel workshare
  end subroutine op_exp

  !> Exponential - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_exp (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_exp(x1%v)
  end subroutine fw_exp

  !> Exponential - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_exp (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_exp(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_exp
  !> @}

  !> @defgroup operators_unary_log_ Logarithm
  !! @{

  !> Logarithm - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_log (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = log(x1%v)
    !$omp end parallel workshare
  end subroutine op_log

  !> Logarithm - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_log (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_log(x1%v)
  end subroutine fw_log

  !> Logarithm - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_log (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_log(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_log
  !> @}
  
  !> @defgroup operators_unary_sin_ Sine
  !! @{

  !> Sine - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_sin (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = sin(x1%v)
    !$omp end parallel workshare
  end subroutine op_sin

  !> Sine - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_sin (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_sin(x1%v)
  end subroutine fw_sin

  !> sine - backward differenatiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_sin (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_sin(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_sin
  !> @}

  !> @defgroup operators_unary_cos_ Cosine
  !! @{

  !> Cosine - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_cos (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = cos(x1%v)
    !$omp end parallel workshare
  end subroutine op_cos

  !> Cosine - forward differenation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_cos (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_cos(x1%v)
  end subroutine fw_cos

  !> Cosine - backward differenation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine bw_cos (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_cos(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_cos
  !> @}

  !> @defgroup operators_unary_tan_ Tangent
  !! @{

  !> Tangent - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_tan (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = tan(x1%v)
    !$omp end parallel workshare
  end subroutine op_tan

  !> Tangent - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_tan (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_tan(x1%v)
  end subroutine fw_tan

  !> Tangent - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_tan (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_tan(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_tan
  !> @}

  !> @defgroup operators_unary_sinh_ Hyperbolic Sine
  !! @{

  !> Hyperbolic Sine - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_sinh (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = sinh(x1%v)
    !$omp end parallel workshare
  end subroutine op_sinh

  !> Hyperbolic Sine - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_sinh (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_sinh(x1%v)
  end subroutine fw_sinh

  !> Hyperbolic Sine - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_sinh (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_sinh(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_sinh
  !> @}

  !> @defgroup operators_unary_cosh_ Hyperbolic Cosine
  !! @{

  !> Hyperbolic Cosine - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_cosh (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = cosh(x1%v)
    !$omp end parallel workshare
  end subroutine op_cosh

  !> Hyperbolic Cosine - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_cosh (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_cosh(x1%v)
  end subroutine fw_cosh

  !> Hyperbolic Cosine - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_cosh (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_cosh(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_cosh
  !> @}

  !> @defgroup operators_unary_tanh_ Hyperbolic Tangent
  !! @{

  !> Hyperbolic Tangent - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_tanh (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = tanh(x1%v)
    !$omp end parallel workshare
  end subroutine op_tanh

  !> Hyerbolic Tangent - forward difference
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_tanh (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_tanh(x1%v)
  end subroutine fw_tanh

  !> Hyperbolic Tangent - backward difference
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_tanh (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_tanh(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_tanh
  !> @}

  !> @defgroup operators_unary_relu_ ReLU
  !! @{

  !> ReLU - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_relu (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = relu(x1%v)
    !$omp end parallel workshare
  end subroutine op_relu

  !> ReLU - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_relu (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_relu(x1%v)
  end subroutine fw_relu

  !> ReLU - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_relu (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_relu(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_relu
  !> @}

  !> @defgroup operators_unary_swish_ Swish
  !! @{

  !> Swish - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_swish (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = swish(x1%v)
    !$omp end parallel workshare
  end subroutine op_swish

  !> Swish - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_swish (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_swish(x1%v)
  end subroutine fw_swish

  !> Swish - bacward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_swish (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_swish(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_swish
  !> @}

  !> @defgroup operators_unary_sigmoid_ Sigmoid
  !! @{

  !> Sigmoid - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine op_sigmoid (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = sigmoid(x1%v)
    !$omp end parallel workshare
  end subroutine op_sigmoid

  !> Sigmoid - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine fw_sigmoid (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * dx_sigmoid(x1%v)
  end subroutine fw_sigmoid

  !> Sigmoid - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine bw_sigmoid (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_sigmoid(x1%v) * x2%dv
       !$omp end parallel workshare
    end if
  end subroutine bw_sigmoid
  !> @}

  !> @defgroup operators_unary_softmax1_ Softmax
  !! @{

  !> Standard Softmax - operator
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine op_softmax__1 (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    x2%v = SOFTMAX(x1%v)
  end subroutine op_softmax__1

  !> Standard Softmax - forward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  subroutine fw_softmax__1 (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = x1%dv * sum(DX_SOFTMAX(x1%v, x2%v), 1)
  end subroutine fw_softmax__1

  !> Standard Softmax - backward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  subroutine bw_softmax__1 (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    real(kind=dp_) :: dx(mtsz(x1))
    if (has_dx(x2)) then
       call dp_gemv(1, 1._dp_, DX_SOFTMAX(x1%v, x2%v), x2%dv, 0._dp_, dx)
       !$omp parallel workshare
       x1%dv = x1%dv + dx
       !$omp end parallel workshare
     end if
  end subroutine bw_softmax__1

  !> Softmax by Dimension - operator
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  !! @param[in] k integer, dimension index
  subroutine op_softmax__2 (x1, x2, k)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in) :: k
    real(kind=dp_) :: m
    integer :: i, indexes(product(x1%shp))
    call number__slice_dim_indexes(x1%shp, k, indexes)
    m = maxval(x1%v)
    do i = 1, x1%shp(k)
       !$omp parallel workshare
       where (indexes == i)
          x2%v = exp(x1%v - m + tol_dp_)
          x2%v = x2%v / sum(pack(x2%v, indexes == i))
       end where
       !$omp end parallel workshare
    end do
  end subroutine op_softmax__2

  !> Softmax by Dimension - forward differentiation
  !! @param[in] x1 'number' input
  !! @param[inout] x2 'number' output
  !! @param[in] k integer, dimension index
  !! @todo fix!
  subroutine fw_softmax__2 (x1, x2, k)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in) :: k
    call do_within("fw_softmax__2", mod_operators_name_, private_fw)
  contains
    subroutine private_fw
      real(kind=dp_), dimension(:,:), pointer :: xx1, dxx1, xx2, dxx2
      integer :: i, j
      !to fix
      j = k + 1 !to eliminate
      if (has_dx(x2)) then
         call number__with_shape(x1, xx1, .false.)
         call number__with_shape(x1, dxx1, .true.)
         call number__with_shape(x2, xx2, .false.)
         call number__with_shape(x2, dxx2, .true.)
         do i = 1, size(xx1, 1)
            dxx2(i,:) = dxx1(i,:) * sum(dx_softmax(xx1(i,:), xx2(i,:) ), 1)
         end do
      end if
    end subroutine private_fw
  end subroutine fw_softmax__2

  !> Softmax by Dimension - backward differentiation
  !! @param[inout] x1 'number' input
  !! @param[in] x2 'number' output
  !! @param[in] k integer, dimension index
  subroutine bw_softmax__2 (x1, x2, k)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: k
    integer :: i, indexes(product(x1%shp))
    !$omp parallel do
    do i = 1, x1%shp(k)
       where (indexes == i) x1%dv = private_bw(x1%v, x2%v, x2%dv)
    end do
    !$omp end parallel do
  contains
    function private_bw (xx1, xx2, dxx2) result(ans)
      implicit none
      real(kind=dp_), intent(in), dimension(:) :: xx1, xx2, dxx2
      real(kind=dp_) :: ans(size(xx1))
      call dp_gemv(1, 1._dp_, DX_SOFTMAX(xx1, xx2), dxx2, 1._dp_, ans)
    end function private_bw
  end subroutine bw_softmax__2
  !> @}
  !> @}

  !> @defgroup operators_objectives_ Objective Operators
  !! Given a target ('y') variable and its estimate (prediction, 'yh')
  !! return a divergenge measure between the two.
  !! The gradient w.r.t. the target is never calculated or considered.
  !! @{

  !> @defgroup operators_objectives_bin_entropy_ Binary Entropy
  !! @{

  !> Bin Entropy - operator
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine op_bin_entropy (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(inout) :: j
    !$omp parallel workshare
    j%v = sum(bin_entropy(y%v, yh%v))
    !$omp end parallel workshare
  end subroutine op_bin_entropy

  !> Bin Entropy - forward differentiation
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine fw_bin_entropy (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(in) :: j
    if (has_dx(j)) j%dv = sum(yh%dv * dx_bin_entropy(y%v, yh%v))
  end subroutine fw_bin_entropy

  !> Bin Entropy - backward differentiation
  !! param[in] y 'numbner', target variable
  !! param[inout] yh 'number', predictions
  !! param[in] j 'number', objective value
  subroutine bw_bin_entropy (y, yh, j)
    implicit none
    type(number), intent(in) :: y, j
    type(number), intent(inout) :: yh
    if (has_dx(j)) then
       !$omp parallel workshare
       yh%dv = yh%dv + dx_bin_entropy(y%v, yh%v) * j%dv(1)
       !$omp end parallel workshare
    end if
  end subroutine bw_bin_entropy
  !> @}

  !> @defgroup operators_objectives_crossentropy1_ Cross-entropy
  !! @{

  !> Cross-entropy - operator
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine op_cross_entropy (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(inout) :: j
    !$omp parallel workshare
    j%v = sum(cross_entropy(y%v, yh%v))
    !$omp end parallel workshare
  end subroutine op_cross_entropy

  !> Cross-entropy - forward differentiation
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine fw_cross_entropy (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(inout) :: j
    if (has_dx(j)) j%dv = yh%dv * sum(dx_cross_entropy(y%v, yh%v))
  end subroutine fw_cross_entropy

  !> Cross-entropy - backward differentiation
  !! param[in] y 'numbner', target variable
  !! param[inout] yh 'number', predictions
  !! param[in] j 'number', objective value
  subroutine bw_cross_entropy (y, yh, j)
    implicit none
    type(number), intent(in) :: y, j
    type(number), intent(inout) :: yh
    if (has_dx(j)) then
       !$omp parallel workshare
       yh%dv = yh%dv + dx_cross_entropy(y%v, yh%v) * j%dv(1)
       !$omp end parallel workshare
    end if
  end subroutine bw_cross_entropy
  !> @}

  !> @defgroup  operators_objectives_mse_ Mean Squared Error (MSE)
  !! @{

  !> MSE - operator
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine op_mse (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(inout) :: j
    integer :: n
    n = mtsz(y)
    !$omp parallel workshare
    j%v = sum((y%v - yh%v)**2) / n
    !$omp end parallel workshare
  end subroutine op_mse

  !> MSE - forward differentiation
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine fw_mse (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(inout) :: j
    if (has_dx(j)) j%dv = sum(dx_ssq(y%v - yh%v) * (-yh%dv)) / mtsz(y)
  end subroutine fw_mse

  !> MSE - backward differentiation
  !! param[in] y 'numbner', target variable
  !! param[inout] yh 'number', predictions
  !! param[in] j 'number', objective value
  subroutine bw_mse (y, yh, j)
    implicit none
    type(number), intent(in) :: y, j
    type(number), intent(inout) :: yh
    integer :: n
    n = mtsz(y)
    if (has_dx(j)) then
       !$omp parallel workshare
       yh%dv = yh%dv - (dx_ssq(y%v - yh%v) * j%dv(1)) / n
       !$omp end parallel workshare
    end if
  end subroutine bw_mse
  !> @}

  !> @defgroup operators_objectives_mae_ Mean Absolute Error (MAE)
  !! @{

  !> MAE - operator
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine op_mae (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(inout) :: j
    integer :: n
    n = mtsz(y)
    !$omp parallel workshare
    j%v = sum(abs(y%v - yh%v)) / n
    !$omp end parallel workshare
  end subroutine op_mae

  !> MAE - forward differentiation
  !! param[in] y 'numbner', target variable
  !! param[in] yh 'number', predictions
  !! param[inout] j 'number', objective value
  subroutine fw_mae (y, yh, j)
    implicit none
    type(number), intent(in) :: y, yh
    type(number), intent(inout) :: j
    if (has_dx(j)) j%dv = sum(dx_abs(y%v - yh%v) * (-yh%dv)) / mtsz(y)
  end subroutine fw_mae

  !> MAE - backward differentiation
  !! param[in] y 'numbner', target variable
  !! param[inout] yh 'number', predictions
  !! param[in] j 'number', objective value
  subroutine bw_mae (y, yh, j)
    implicit none
    type(number), intent(in) :: y, j
    type(number), intent(inout) :: yh
    integer :: n
    n = mtsz(y)
    if (has_dx(j)) then
       !$omp parallel workshare
       yh%dv = yh%dv - (dx_abs(y%v - yh%v) * j%dv(1)) / n
       !$omp end parallel workshare
    end if
  end subroutine bw_mae
  !> @}
  !> @}

  !> defgroup gemm_operators_ General Matrix Multiplictions
  !! Different kind of standard multiplications between general matrices.
  !! @author Filippo Monari
  !! @param[inout] alpha,beta 'numbers' of rank 0
  !! @param[inout] A,B,C,CC 'numbers' of rank 2
  !! @param[in] transA,transB integer, transposition flags
  !! @{

  !> @defgroup gemm_operators_gemm1_ Gemm 1
  !! alpha * op(A) . op(B) + beta * C
  !! @{

  !> Gemm 1 - operator
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] alpha 'number of rank 0
  !! @param[in] A 'number' of rank 2
  !! @param[in] B 'number' of rank 2
  !! @param[in] beta 'number' of rank 1
  !! @param[in] C 'number' of rank 2
  !! @param[inout] CC 'number' of rank 2
  subroutine op_dgemm__1 (transA, transB, alpha, A, B, beta, C, CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in), target :: alpha, beta
    type(number), intent(in), target :: A, B
    type(number), intent(in) :: C
    type(number), intent(inout), target :: CC
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xcc(:,:)
    CC%v = C%v
    call number__with_shape(A, xa, .false.)
    call number__with_shape(B, xb, .false.)
    call number__with_shape(CC, xcc, .false.)
    call dp_gemm(transA, transB, alpha%v(1), xa, xb, beta%v(1), xcc)
  end subroutine op_dgemm__1

  !> Gemm 1 - forward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] alpha 'number of rank 0
  !! @param[in] A 'number' of rank 2
  !! @param[in] B 'number' of rank 2
  !! @param[in] beta 'number' of rank 1
  !! @param[in] C 'number' of rank 2
  !! @param[inout] CC 'number' of rank 2
  subroutine fw_dgemm__1 (transA, transB, alpha, A, B, beta, C, CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in), target :: A, B, alpha
    type(number), intent(inout), target :: CC
    type(number), intent(in) :: beta, C
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xcc(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxcc(:,:) 
    double precision :: bb
    if (has_dx(CC)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(CC, xcc, .false.)
       call number__with_shape(CC, dxcc, .true.) 
       call number__with_shape(CC, dxcc, .true.)
       bb = 0
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dp_gemm(transA, transB, alpha%v(1), dxa, xb, bb, xcc)
          bb = 1._dp_
       end if
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dp_gemm(transA, transB, alpha%v(1), xa, dxb, bb, xcc)
          bb = 1._dp_
       end if
       if (has_dx(alpha)) then
          call dp_gemm(transA, transB, alpha%dv(1), xa, xb, bb, xcc)
       end if
       if (has_dx(C)) then
          CC%dv = CC%dv + beta%v(1) * C%dv
       end if
       if (has_dx(beta)) then
          CC%dv = CC%dv + beta%dv(1) * C%v
       end if
    end if
  end subroutine fw_dgemm__1

  !> Gemm 1 - backward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[inout] alpha 'number of rank 0
  !! @param[inout] A 'number' of rank 2
  !! @param[inout] B 'number' of rank 2
  !! @param[inout] beta 'number' of rank 1
  !! @param[inout] C 'number' of rank 2
  !! @param[in] CC 'number' of rank 2
  subroutine bw_dgemm__1 (transA, transB, alpha, A, B, beta, C, CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(inout) :: A, B, C
    type(number), intent(inout) :: alpha, beta
    type(number), intent(in) :: CC
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xcc(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxcc(:,:) 
    if (has_dx(CC)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(CC, xcc, .false.)
       call number__with_shape(CC, dxcc, .true.)
       !$omp parallel sections
       !$omp section
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dx_bw_dp_gemm_A(transA, transB, alpha%v(1), xb, dxcc, dxa)
       end if
       !$omp section
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dx_bw_dp_gemm_B(transA, transB, alpha%v(1), xa, dxcc, dxb)
       end if
       !$omp section
       if (has_dx(alpha)) then
          call DX_BW_DP_GEMM_ALPHA(transA, transB, xa, xb, dxcc, alpha%dv(1))
       end if
       !$omp end parallel sections
       if (has_dx(C)) then
          !$omp parallel workshare
          C%dv = C%dv + beta%v(1) * CC%dv
          !$omp end parallel workshare
       end if
       if (has_dx(beta)) then
          !$omp parallel workshare
          beta%dv(1) = beta%dv(1) + sum(C%v * CC%dv)
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_dgemm__1
  !> @}

  !> @defgroup gemm_operators_gemm3_ Gemm 3
  !! op(A) . op(B) + C
  !! @{

  !> Gemm 3 - operator
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] A 'number' of rank 2
  !! @param[in] B 'number' of rank 2
  !! @param[in] C 'number' of rank 2
  !! @param[inout] CC 'number' of rank 2
  subroutine op_dgemm__3 (transA, transB, A, B, C, CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: A, B, C
    type(number), intent(inout) :: CC
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xcc(:,:)
    CC%v = C%v
    call number__with_shape(A, xa, .false.)
    call number__with_shape(B, xb, .false.)
    call number__with_shape(CC, xcc, .false.)
    call dp_gemm(transA, transB, 1._dp_, xa, xb, 1._dp_, xcc)
  end subroutine op_dgemm__3

  !> Gemm 3 - forward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] A 'number' of rank 2
  !! @param[in] B 'number' of rank 2
  !! @param[in] C 'number' of rank 2
  !! @param[inout] CC 'number' of rank 2
  subroutine fw_dgemm__3 (transA, transB, A, B, C, CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: A, B, C
    type(number), intent(inout) :: CC
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xcc(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxcc(:,:)
    double precision :: bb
    if (has_dx(CC)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(CC, xcc, .false.)
       call number__with_shape(CC, dxcc, .true.)
       bb = 0
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dp_gemm(transA, transB, 1._dp_, dxa, xb, bb, dxcc)
          bb = 1
       end if
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dp_gemm(transA, transB, 1._dp_, xa, dxb, bb, dxcc)
       end if
       if (has_dx(C)) then
          CC%dv = CC%dv + C%dv
       end if
    end if
  end subroutine fw_dgemm__3

  !> Gemm 3 - backward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[inout] A 'number' of rank 2
  !! @param[inout] B 'number' of rank 2
  !! @param[inout] C 'number' of rank 2
  !! @param[in] CC 'number' of rank 2
  subroutine bw_dgemm__3 (transA, transB, A, B, C, CC)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(inout) :: A, B, C
    type(number), intent(in) :: CC
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xcc(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxcc(:,:)
    if (has_dx(CC)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(CC, xcc, .false.)
       call number__with_shape(CC, dxcc, .true.)
       !$omp parallel sections
       !$omp section
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dx_bw_dp_gemm_A(transA, transB, 1._dp_, xb, dxcc, dxa)
       end if
       !$omp section
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dx_bw_dp_gemm_B(transA, transB, 1._dp_, xa, dxcc, dxb)
       end if
       !$omp end parallel sections
       if (has_dx(C)) then
          !$omp parallel workshare
          C%dv = C%dv + CC%dv
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_dgemm__3
  !> @}

  !> @defgroup gemm_operators_gemm2_ Gemm 2
  !! alpha * op(A) . op(B)
  !! @{

  !> Gemm2 - operator
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] alpha 'number of rank 0
  !! @param[in] A 'number' of rank 2
  !! @param[in] B 'number' of rank 2
  !! @param[inout] C 'number' of rank 2
  subroutine op_dgemm__2 (alpha, transA, transB, A, B, C)
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: alpha
    type(number), intent(in) :: A, B
    type(number), intent(inout) :: C
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xc(:,:)
    call number__with_shape(A, xa, .false.)
    call number__with_shape(B, xb, .false.)
    call number__with_shape(C, xc, .false.)
    call dp_gemm(transA, transB, alpha%v(1), xa, xb, 0._dp_, xc)
  end subroutine op_dgemm__2

  !> Gemm 2 - forward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] alpha 'number of rank 0
  !! @param[in] A 'number' of rank 2
  !! @param[in] B 'number' of rank 2
  !! @param[inout] C 'number' of rank 2
  subroutine fw_dgemm__2 (alpha, transA, transB, A, B, C)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: A, B
    type(number), intent(inout) :: C
    type(number), intent(in) :: alpha
    real(kind=dp_) :: beta
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xc(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxc(:,:)
    if (has_dx(C)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(C, xc, .false.)
       call number__with_shape(C, dxc, .true.)
       beta = 0
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dp_gemm(transA, transB, alpha%v(1), dxa, xb, beta, dxc)
          beta = 1
       end if
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dp_gemm(transA, transB, alpha%v(1), xa, dxb, beta, dxc)
          beta = 1
       end if
       if (has_dx(alpha)) then
          call dp_gemm(transA, transB, alpha%dv(1), xa, xb, beta, dxc)
       end if
    end if
  end subroutine fw_dgemm__2

  !> Gemm 2 - backward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[inout] alpha 'number of rank 0
  !! @param[inout] A 'number' of rank 2
  !! @param[inout] B 'number' of rank 2
  !! @param[in] C 'number' of rank 2
  subroutine bw_dgemm__2 (alpha, transA, transB, A, B, C)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(inout) :: A, B
    type(number), intent(inout) :: alpha
    type(number), intent(in) :: C
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xc(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxc(:,:)
    if (has_dx(C)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(C, xc, .false.)
       call number__with_shape(C, dxc, .true.)
       !$omp parallel sections
       !$omp section
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dx_bw_dp_gemm_A(transA, transB, alpha%v(1), xb, dxc, dxa)
       end if
       !$omp section
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dx_bw_dp_gemm_B(transA, transB, alpha%v(1), xa, dxc, dxb)
       end if
       !$omp section
       if (has_dx(alpha)) then
          call DX_BW_DP_GEMM_ALPHA(transA, transB, xa, xb, dxc, alpha%dv(1))
       end if
       !$omp end parallel sections
    end if
  end subroutine bw_dgemm__2
  !> @}

  !> @defgroup gemm_operators_gemm4_ Gemm 4
  !! op(A) . op(B)
  !! @{

  !> Gemm 4 - operator
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[inout] A 'number' of rank 2
  !! @param[inout] B 'number' of rank 2
  !! @param[in] C 'number' of rank 2
  subroutine op_dgemm__4 (transA, transB, A, B, C)
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: A, B
    type(number), intent(inout) :: C
    real(kind=dp_), pointer :: xa(:,:), xb(:,:), xc(:,:)
    call number__with_shape(A, xa, .false.)
    call number__with_shape(B, xb, .false.)
    call number__with_shape(C, xc, .false.)
    call dp_gemm(transA, transB, 1._dp_, xa, xb, 0._dp_, xc)
  end subroutine op_dgemm__4

  !> Gemm 4 - forward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] A 'number' of rank 2
  !! @param[in] B 'number' of rank 2
  !! @param[inout] C 'number' of rank 2
  subroutine fw_dgemm__4 (transA, transB, A, B, C)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(in) :: A, B
    type(number), intent(inout) :: C
    real(kind=dp_), pointer :: xa(:,:), xb(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxc(:,:)
    double precision :: beta
    if (has_dx(C)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(C, dxc, .true.)
       beta = 0
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dp_gemm(transA, transB, 1._dp_, dxa, xb, beta, dxc)
          beta = 1
       end if
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dp_gemm(transA, transB, 1._dp_, xa, dxb, beta, dxc)
       end if
    end if
  end subroutine fw_dgemm__4

  !> Gemm 4 backward differentiation
  !! @param[in] transA integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB integer, if > 0 op(B) = B**T else op(B) = B
  !! @param[inout] A 'number' of rank 2
  !! @param[inout] B 'number' of rank 2
  !! @param[in] C 'number' of rank 2
  subroutine bw_dgemm__4 (transA, transB, A, B, C)
    implicit none
    integer, intent(in) :: transA, transB
    type(number), intent(inout) :: A, B
    type(number), intent(in) :: C
    real(kind=dp_), pointer :: xa(:,:), xb(:,:)
    real(kind=dp_), pointer :: dxa(:,:), dxb(:,:), dxc(:,:)
    if (has_dx(C)) then
       call number__with_shape(A, xa, .false.)
       call number__with_shape(B, xb, .false.)
       call number__with_shape(C, dxc, .true.)
       !$omp parallel sections
       !$omp section
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dx_bw_dp_gemm_A(transA, transB, 1._dp_, xb, dxc, dxa)
       end if
       !$omp section
       if (has_dx(B)) then
          call number__with_shape(B, dxb, .true.)
          call dx_bw_dp_gemm_B(transA, transB, 1._dp_, xa, dxc, dxb)
       end if
       !$omp end parallel sections
    end if
  end subroutine bw_dgemm__4
  !> @}
  !> @}

  !> @defgroup gemv_operators Gemv Operators
  !! Different kind of matrix-vector multiplication with general matrices.
  !! @{

  !> @defgroup gemv_operators_gemv1_ Gemv 1
  !! alpha * op(A) . x + beta * y
  !! @{

  !> Gemv 1 - operator
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] alpha 'number' of rank 0
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[in] beta 'number' of rank 0
  !! @param[in] y 'number' of rank 1
  !! @param[inout] yy 'number' of rank 1
  subroutine op_dp_gemv__1 (trans, alpha, x, A, beta, y, yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: alpha, x, A, beta, y
    type(number), intent(inout) :: yy
    real(kind=dp_), pointer :: xa(:,:)
    yy%v = y%v
    call number__with_shape(A, xa, .false.)
    call dp_gemv(trans, alpha%v(1), xa, x%v, beta%v(1), yy%v)
  end subroutine op_dp_gemv__1

  !> Gemv 1 - forward differentiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] alpha 'number' of rank 0
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[in] beta 'number' of rank 0
  !! @param[in] y 'number' of rank 1
  !! @param[inout] yy 'number' of rank 1
  subroutine fw_dp_gemv__1 (trans, alpha, x, A, beta, y, yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: alpha, x, A, y, beta
    type(number), intent(inout) :: yy
    real(kind=dp_), pointer :: xa(:,:)
    real(kind=dp_), pointer :: dxa(:,:)
    real(kind=dp_) :: bb
    if (has_dx(yy)) then
       call number__with_shape(A, xa, .false.)
       bb = 0._dp_
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dp_gemv(trans, alpha%v(1), dxa, x%v, bb, yy%dv)
          bb = 1._dp_
       end if
       if (has_dx(x)) then
          call dp_gemv(trans, alpha%v(1), xa, x%dv, bb, yy%dv)
          bb = 1._dp_
       end if
       if (has_dx(alpha)) then
          call dp_gemv(trans, alpha%dv(1), xa, x%v, bb, yy%dv)
       end if
       if (has_dx(y)) then
          yy%dv = yy%dv + beta%v(1) * y%dv
       end if
       if (has_dx(beta)) then
          yy%dv = yy%dv + beta%dv(1) * y%v
       end if
    end if
  end subroutine fw_dp_gemv__1

  !> Gemv 1 - backward differentiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[inout] alpha 'number' of rank 0
  !! @param[inout] x 'number' of rank 1
  !! @param[inout] A 'number' of rank 2
  !! @param[inout] beta 'number' of rank 0
  !! @param[inout] y 'number' of rank 1
  !! @param[in] yy 'number' of rank 1
  subroutine bw_dp_gemv__1 (trans, alpha, x, A, beta, y, yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(inout) :: x, A, y
    type(number), intent(inout) :: alpha, beta
    type(number), intent(in) :: yy
    real(kind=dp_), pointer :: xa(:,:), dxa(:,:) 
    if (has_dx(yy)) then
      call number__with_shape(A, xa, .false.)
      call number__with_shape(A, dxa, .true.)
      !$omp parallel sections
      !$omp section
      if (has_dx(A)) then
         call dx_bw_dp_gemv_A(trans, alpha%v(1), x%v, yy%dv, dxa)
      end if
      !$omp section
      if (has_dx(x)) then
         call dx_bw_dp_gemv_x(trans, alpha%v(1), xa, yy%dv, x%dv)
      end if
      !$omp section
      if (has_dx(alpha)) then
         call DX_BW_DP_GEMV_ALPHA(trans, xa, x%v, yy%dv, alpha%dv(1))
      end if
      !$omp end parallel sections
      if (has_dx(y)) then
         !$omp parallel workshare
         y%dv = y%dv + beta%v(1) * yy%dv
         !$omp end parallel workshare
      end if
      if (has_dx(beta)) then
         !$omp parallel workshare
         beta%dv(1) = beta%dv(1) + sum(y%v * yy%dv)
         !$omp end parallel workshare
      end if
    end if
  end subroutine bw_dp_gemv__1
  !> @}

  !> @defgroup gemv_operators_gemv2_ Gemv 2
  !! alpha * op(A) . x 
  !! @{

  !> Gemv 2 - operator
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] alpha 'number' of rank 0
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[inout] y 'number' of rank 1
  subroutine op_dp_gemv__2 (alpha, trans, x, A, y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: alpha, x, A
    type(number), intent(inout) :: y
    real(kind=dp_), pointer :: xa(:,:)
    call number__with_shape(A, xa, .false.)
    call dp_gemv(trans, alpha%v(1), xa, x%v, 0._dp_, y%v)
  end subroutine op_dp_gemv__2

  !> Gemv 2 - forward differenatiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] alpha 'number' of rank 0
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[inout] y 'number' of rank 1
  subroutine fw_dp_gemv__2 (alpha, trans, x, A, y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: alpha, x, A
    type(number), intent(inout) :: y
    real(kind=dp_), pointer :: xa(:,:)
    real(kind=dp_), pointer :: dxa(:,:)
    real(kind=dp_) :: bb
    if (has_dx(y)) then
      call number__with_shape(A, xa, .false.)
      bb = 0._dp_
      if (has_dx(A)) then
         call number__with_shape(A, dxa, .true.)
         call dp_gemv(trans, alpha%v(1), dxa, x%v, bb, y%dv)
         bb = 1._dp_
      end if
      if (has_dx(x)) then
         call dp_gemv(trans, alpha%v(1), xa, x%dv, bb, y%dv)
         bb = 1._dp_
      end if
      if (has_dx(alpha)) then
         call dp_gemv(trans, alpha%dv(1), xa, x%v, bb, y%dv)
      end if
   end if
  end subroutine fw_dp_gemv__2

  !> Gemv 2 - backward differentiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[inout] alpha 'number' of rank 0
  !! @param[inout] x 'number' of rank 1
  !! @param[inout] A 'number' of rank 2
  !! @param[in] y 'number' of rank 1
  subroutine bw_dp_gemv__2 (alpha, trans, x, A, y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(inout) :: x, A
    type(number), intent(inout) :: alpha
    type(number), intent(in) :: y
    real(kind=dp_), pointer :: xa(:,:)
    real(kind=dp_), pointer :: dxa(:,:)
    if (has_dx(y)) then
      call number__with_shape(A, xa, .false.)
      !$omp parallel sections
      !$omp section
      if (has_dx(A)) then
         call number__with_shape(A, dxa, .true.)
         call dx_bw_dp_gemv_A(trans, alpha%v(1), x%v, y%dv, dxa)
      end if
      !$omp section
      if (has_dx(x)) then
         call dx_bw_dp_gemv_x(trans, alpha%v(1), xa, y%dv, x%dv)
      end if
      !$omp section
      if (has_dx(alpha)) then
         call DX_BW_DP_GEMV_ALPHA(trans, xa, x%v, y%dv, alpha%dv(1))
      end if
      !$omp end parallel sections
   end if
  end subroutine bw_dp_gemv__2
  !> @}

  !> @defgroup gemv_operators_gemv3_ Gemv 3
  !! op(A) . x + y
  !! @{

  !> Gemv 3 - operator
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[in] y 'number' of rank 1
  !! @param[inout] yy 'number' of rank 1
  subroutine op_dp_gemv__3 (trans, x, A, y, yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: x, A, y
    type(number), intent(inout) :: yy
    real(kind=dp_), pointer :: xa(:,:)
    yy%v = y%v
    call number__with_shape(A, xa, .false.)
    call dp_gemv(trans, 1._dp_, xa, x%v, 1._dp_, yy%v)
  end subroutine op_dp_gemv__3

  !> Gemv 3 - forward differentiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[in] y 'number' of rank 1
  !! @param[inout] yy 'number' of rank 1
  subroutine fw_dp_gemv__3 (trans, x, A, y, yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: x, A, y
    type(number), intent(inout) :: yy
    real(kind=dp_), pointer :: xa(:,:)
    real(kind=dp_), pointer :: dxa(:,:)
    real(kind=dp_) :: bb
    if (has_dx(yy)) then
      call number__with_shape(A, xa, .false.)
      bb = 0._dp_
      if (has_dx(A)) then
         call number__with_shape(A, dxa, .true.)
         call dp_gemv(trans, 1._dp_, dxa, x%v, bb, yy%dv)
         bb = 1._dp_
      end if
      if (has_dx(x)) then
         call dp_gemv(trans, 1._dp_, xa, x%dv, bb, yy%dv)
         bb = 1._dp_
      end if
      if (has_dx(y)) then
         yy%dv = yy%dv + y%dv
      end if
   end if
  end subroutine fw_dp_gemv__3

  !> Gemv 3 - backward differentiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[inout] x 'number' of rank 1
  !! @param[inout] A 'number' of rank 2
  !! @param[inout] y 'number' of rank 1
  !! @param[in] yy 'number' of rank 1
  subroutine bw_dp_gemv__3 (trans, x, A, y, yy)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(inout) :: x, A, y
    type(number), intent(in) :: yy
    real(kind=dp_), pointer :: xa(:,:)
    real(kind=dp_), pointer :: dxa(:,:) 
    if (has_dx(yy)) then 
       call number__with_shape(A, xa, .false.)
       !$omp parallel sections
       !$omp section
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dx_bw_dp_gemv_A(trans, 1._dp_, x%v, yy%dv, dxa)
       end if
       !$omp section
       if (has_dx(x)) then
          call dx_bw_dp_gemv_x(trans, 1._dp_, xa, yy%dv, x%dv)
       end if
       !$omp end parallel sections
       if (has_dx(y)) then
          !$omp parallel workshare
          y%dv = y%dv + yy%dv
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_dp_gemv__3
  !> @}

  !> @defgroup gemv_operators_gemv4_ Gemv 4
  !! op(A) . x 
  !! @{

  !> Gemv 4 - operator
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[inout] y 'number' of rank 1
  subroutine op_dp_gemv__4 (trans, x, A, y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: x, A
    type(number), intent(inout) :: y
    real(kind=dp_), pointer :: xa(:,:)
    call number__with_shape(A, xa, .false.)
    call dp_gemv(trans, 1._dp_, xa, x%v, 0._dp_, y%v)
  end subroutine op_dp_gemv__4

  !> Gemv 4 - forward differentiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[inout] y 'number' of rank 1
  subroutine fw_dp_gemv__4 (trans, x, A, y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(in) :: x, A
    type(number), intent(inout) :: y
    real(kind=dp_), pointer :: xa(:,:)
    real(kind=dp_), pointer :: dxa(:,:)
    real(kind=dp_) :: bb
    if (has_dx(y)) then
      call number__with_shape(A, xa, .false.)
      bb = 0._dp_
      if (has_dx(A)) then
         call number__with_shape(A, dxa, .true.)
         call dp_gemv(trans, 1._dp_, dxa, x%v, bb, y%dv)
         bb = 1._dp_
      end if
      if (has_dx(x)) then
         call dp_gemv(trans, 1._dp_, xa, x%dv, bb, y%dv)
         bb = 1._dp_
      end if
   end if
  end subroutine fw_dp_gemv__4

  !> Gemv 4 - backward differentiation
  !! @param[in] trans integer, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] x 'number' of rank 1
  !! @param[in] A 'number' of rank 2
  !! @param[inout] y 'number' of rank 1
  subroutine bw_dp_gemv__4 (trans, x, A, y)
    implicit none
    integer, intent(in) :: trans
    type(number), intent(inout) :: x, A
    type(number), intent(in) :: y
    real(kind=dp_), pointer :: xa(:,:)
    real(kind=dp_), pointer :: dxa(:,:)
    if (has_dx(y)) then 
       call number__with_shape(A, xa, .false.)
       !$omp parallel sections
       !$omp section
       if (has_dx(A)) then
          call number__with_shape(A, dxa, .true.)
          call dx_bw_dp_gemv_A(trans, 1._dp_, x%v, y%dv, dxa)
       end if
       !$omp section
       if (has_dx(x)) then
          call dx_bw_dp_gemv_x(trans, 1._dp_, xa, y%dv, x%dv)
       end if
       !$omp end parallel sections
    end if
  end subroutine bw_dp_gemv__4
  !> @}
  !> @}
  
  !> @defgroup ger_operators_ General Outer Product Operators
  !! Different standard Outer products between vectors.
  !! @{

  !> @defgroup ger_operators_ger1_ Ger 1
  !! alpha * x . y**T + A
  !! @{

  !> Ger 1 - operator
  !! @param[in] alpha 'number' of rank 0
  !! @param[in] x 'number' of rank 1
  !! @param[in] y 'number' of rank 1
  !! @param[in] z 'number' of rank 2
  !! @param[inout] A 'number' of rank 2
  subroutine op_dp_ger__1 (alpha, x, y, z, A)
    implicit none
    type(number), intent(in) :: alpha, x, y, z
    type(number), intent(inout) :: A
    real(kind=dp_), pointer :: xA(:,:)
    A%v = z%v
    call number__with_shape(A, xA, .false.)
    call dp_ger(alpha%v(1), x%v, y%v, xA)
  end subroutine op_dp_ger__1

  !> Ger 1 - backward differentiation
  !! @param[in] alpha 'number' of rank 0
  !! @param[inout] 'number' of rank 1
  !! @param[inout] 'number' of rank 1
  !! @param[inout] 'number' of rank 2
  !! @param[in] A 'number' of rank 2
  subroutine bw_dp_ger__1 (alpha, x, y, z, A)
    implicit none
    type(number), intent(inout) :: alpha, x, y, z
    type(number), intent(in) :: A
    real(kind=dp_), pointer :: dxA(:,:)
    reAL(kind=dp_) :: s
    if (has_dx(A)) then
       call number__with_shape(A, dxA, .true.)
       !$omp parallel sections
       !$omp section
       if (has_dx(x)) then
          call dp_gemv(0, alpha%v(1), dxA, y%v, 1._dp_, x%dv)
       end if
       !$omp section
       if (has_dx(y)) then
          call dp_gemv(0, alpha%v(1), dxA, x%v, 1._dp_, y%dv)
       end if
       !$omp end parallel sections
       if (has_dx(alpha)) then
          !$omp parallel workshare
          !alpha%dv = alpha%dv + sum(outerprod(xx, xy) * dxA)
          s = sum((A%v - z%v) / alpha%v(1) * A%dv)
          alpha%dv = alpha%dv + s
          !$omp end parallel workshare
       end if
       if (has_dx(z)) then
          !$omp parallel workshare
          z%dv = z%dv + A%dv
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_dp_ger__1
  !> @}

  !> @defgroup ger_operators_ger2_ Ger 1
  !! x . y**T + A
  !! @{

  !> Ger 2 - operator
  !! @param[in] x 'number' of rank 1
  !! @param[in] y 'number' of rank 1
  !! @param[in] z 'number' of rank 2
  !! @param[inout] A 'number' of rank 2
  subroutine op_dp_ger__2 (x, y, z, A)
    implicit none
    type(number), intent(in) :: x, y, z
    type(number), intent(inout) :: A
    real(kind=dp_), pointer :: xA(:,:)
    A%v = z%v
    call number__with_shape(A, xA, .false.)
    call dp_ger(1._dp_, x%v, y%v, xA)
  end subroutine op_dp_ger__2

  !> Ger 2 - backward dfferentiation
  !! @param[inout 'number' of rank 1
  !! @param[inout 'number' of rank 1
  !! @param[inout 'number' of rank 2
  !! @param[in   ] A 'number' of rank 2
  subroutine bw_dp_ger__2 (x, y, z, A)
    implicit none
    type(number), intent(inout) :: x, y, z
    type(number), intent(in) :: A
    real(kind=dp_), pointer :: dxA(:,:)
    if (has_dx(A)) then
       call number__with_shape(A, dxA, .true.)
       !$omp parallel sections
       !$omp section
       if (has_dx(x)) then
          call dp_gemv(0, 1._dp_, dxA, y%v, 1._dp_, x%dv)
       end if
       !$omp section
       if (has_dx(y)) then
          call dp_gemv(0, 1._dp_, dxA, x%v, 1._dp_, y%dv)
       end if
       !$omp end parallel sections
       if (has_dx(z)) then
          !$omp parallel workshare
          z%dv = A%dv
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_dp_ger__2
  !> @}

  !> @defgroup ger_operators_ger3_ Ger 3
  !! x . y**T
  !! @{

  !> Ger 3 - operator
  !! @param[in] x 'number' of rank 1
  !! @param[in] y 'number' of rank 1
  !! @param[inout] A 'number' of rank 2
  subroutine op_dp_ger__3 (x, y, A)
    implicit none
    type(number), intent(in) :: x, y
    type(number), intent(inout) :: A
    real(kind=dp_), pointer :: xA(:,:)
    call number__with_shape(A, xA, .false.)
    call dp_ger(1._dp_, x%v, y%v, xA)
  end subroutine op_dp_ger__3

  !> Ger 3 - backward differentiation
  !! @param[inout 'number' of rank 1
  !! @param[inout 'number' of rank 1
  !! @param[in   ] A 'number' of rank 2
  subroutine bw_dp_ger__3 (x, y, A)
    implicit none
    type(number), intent(inout) :: x, y
    type(number), intent(in) :: A
    real(kind=dp_), pointer :: dxA(:,:)
    if (has_dx(A)) then
       call number__with_shape(A, dxA, .true.)
       !$omp parallel sections
       !$omp section
       if (has_dx(x)) then
          call dp_gemv(0, 1._dp_, dxA, y%v, 1._dp_, x%dv)
       end if
       !$omp section
       if (has_dx(y)) then
          call dp_gemv(0, 1._dp_, dxA, x%v, 1._dp_, y%dv)
       end if
       !$omp end parallel sections
    end if
  end subroutine bw_dp_ger__3
  !> @}
  !> @}

  !> @defgroup dot_operator_ Inner Product (dot) between Vectors
  !! x**T . y
  !! @{

  !> Dot - operator
  !! @param[in] x 'number' of rank 1
  !! @param[in] y 'number' of rank 1
  !! @param[inout] xz 'number' of rank 0
  subroutine op_dp_dot (x, y, z)
    implicit none
    type(number), intent(in) :: x, y
    type(number), intent(inout) :: z
    z%v(1) = dp_dot(x%v, y%v)
  end subroutine op_dp_dot

  !> Dot - backward differentiation
  !! @param[inout 'number' of rank 1
  !! @param[inout 'number' of rank 1
  !! @param[in   ] xz 'number' of rank 0
  subroutine bw_dp_dot (x, y, z)
    implicit none
    type(number), intent(inout) :: x, y
    type(number), intent(in) :: z
    if (has_dx(z)) then
       if (has_dx(x)) then
          !$omp parallel workshare
          x%dv = y%v * z%dv(1)
          !$omp end parallel workshare
       end if
       if (has_dx(y)) then
          !$omp parallel workshare
          y%dv = x%v * z%dv(1)
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_dp_dot
  !> @}

  !> @defgroup operators_unary_invMat_ Matrix Inversion
  !! @{

  !> General Matrix Inversion - operator
  !! @param[in] x 'number' of rank 2
  !! @param[inout] xi 'number' of rank 2
  subroutine op_invMat (x, xi)
    implicit none
    type(number), intent(in) :: x
    type(number), intent(inout) :: xi
    call do_within_critical('op_invMat', mod_operators_name_, private_op)
  contains
    subroutine private_op
      real(kind=dp_), pointer :: xx(:,:), xxi(:,:)
      integer :: info
      info = 0
      call number__with_shape(x, xx, .false.)
      call number__with_shape(xi, xxi, .false.)
      xxi = xx
      call invMat(xxi, info)
      call assert(info == 0, info, 'invMat')
    end subroutine private_op
  end subroutine op_invMat

  !> General Matrix Inversion - forward differentiation
  !! @param[in] x 'number' of rank 2
  !! @param[inout] xi 'number' of rank 2
  subroutine fw_invMat (x, xi)
    implicit none
    type(number), intent(in) :: x
    type(number), intent(inout) :: xi
    real(kind=dp_), pointer :: dxx(:,:), xxi(:,:), dxxi(:,:)
    if (has_dx(xi)) then
       call number__with_shape(x, dxx, .true.)
       call number__with_shape(xi, xxi, .false.)
       call number__with_shape(xi, dxxi, .true.)
       call dx_fw_invMat(dxx, xxi, dxxi)
    end if
  end subroutine fw_invMat

  !> General Matrix Inversion - backward differentiation
  !! @param[inout 'number' of rank 2
  !! @param[in] xi 'number' of rank 2
  subroutine bw_invMat (x, xi)
    implicit none
    type(number), intent(inout) :: x
    type(number), intent(in) :: xi
    real(kind=dp_), pointer :: dxx(:,:), xxi(:,:), dxxi(:,:)
    if (has_dx(xi)) then
       call number__with_shape(x, dxx, .true.)
       call number__with_shape(xi, xxi, .false.)
       call number__with_shape(xi, dxxi, .true.)
       call dx_bw_invMat(dxxi, xxi, dxx)
    end if
  end subroutine bw_invMat
  
  !> @defgroup reduction_operators_ Reduction Operators
  !! Take a 'number' with lower rank
  !! (e.g. Sum and Sum of Squares).
  !! @author Filippo Monari
  !! @{
  
  !> @defgroup reduction_operators_sum_ Sum
  !! @{

  !> Sum - operator
  !! @param[in] x1 'number' of rank > 0
  !! @param[inout] x2 'number' of rank 0
  subroutine op_sum__1 (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = sum(x1%v)
    !$omp end parallel workshare
  end subroutine op_sum__1

  !> Sum - forward differentiation
  !! @param[in] x1 'number' of rank > 0
  !! @param[inout] x2 'number' of rank 0
  subroutine fw_sum (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = sum(x1%dv)
  end subroutine fw_sum

  !> Sum - backward differentiation
  !! @param[inout] x1 'number' of rank > 0
  !! @param[in] x2 'number' of rank 0
  subroutine bw_sum__1 (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + x2%dv(1)
       !$omp end parallel workshare
    end if
  end subroutine bw_sum__1
  !> @}

  !> @defgroup reduction_operators_sum_ Sum Along a Dimension
  !! @{

  !> Sum Along Dimension - operator
  !! @param[in] x1 'number' of rank > 0
  !! @param[inout] x2 'number' of rank 0
  subroutine op_sum__2 (x1, x2, k)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in) :: k
    integer :: i, indexes(product(x1%shp))
    call number__slice_dim_indexes(x1%shp, k, indexes)
    do i = 1, x1%shp(k)
       !$omp parallel workshare
       x2%v(i) = sum(pack(x1%v, indexes == i))
       !$omp end parallel workshare
    end do
  end subroutine op_sum__2

  !> Sum Along Dimension - backward differentiation
  !! @param[inout] x1 'number' of rank > 0
  !! @param[in] x2 'number' of rank 0
  subroutine bw_sum__2 (x1, x2, k)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: k
    integer :: i, indexes(product(x1%shp))
    call number__slice_dim_indexes(x1%shp, k, indexes)
    do i = 1, x1%shp(k)
       !$omp parallel workshare
       where (indexes == i) x1%dv = x1%dv + x2%dv(i)
       !$omp end parallel workshare
    end do
  end subroutine bw_sum__2
  !> @}
  
  !> @defgroup reduction_operators_product Product

  !> Product - Operator
  !! @param[in] x1 'number' of rank > 0
  !! @param[inout] x2 'number' of rank 0
  subroutine op_product__1 (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = product(x1%v)
    !$omp end parallel workshare
  end subroutine op_product__1

  ! !> Product - forward differentiation
  ! !! @param[in] x1 'number' of rank > 0
  ! !! @param[inout] x2 'number' of rank 0
  ! subroutine fw_product (x1, x2)
  !   implicit none
  !   type(number), intent(in) :: x1
  !   type(number), intent(inout) :: x2
  !   if (has_dx(x2)) x2%dv = sum(dx_product(x1%v) * x1%dv)
  ! end subroutine fw_product

  !> Product - backward differentiation
  !! @param[inout] x1 'number' of rank > 0
  !! @param[in] x2 'number' of rank 0
  subroutine bw_product__1 (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + x2%v / x1%v * x2%dv(1)
       !$omp end parallel workshare
    end if
  end subroutine bw_product__1
  !> @}

  !> @defgroup reduction_operators_product Product Along a Dimension

  !> Product Along Dimension - Operator
  !! @param[in] x1 'number' of rank > 0
  !! @param[inout] x2 'number' of rank 0
  subroutine op_product__2 (x1, x2, k)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    integer, intent(in) :: k
    integer :: i, indexes(product(x1%shp))
    call number__slice_dim_indexes(x1%shp, k, indexes)
    do i = 1, x1%shp(k)
       !$omp parallel workshare
       x2%v(i) = product(pack(x1%v, indexes == i))
       !$omp end parallel workshare
    end do
  end subroutine op_product__2

  !> Product Along Dimension - backward differentiation
  !! @param[inout] x1 'number' of rank > 0
  !! @param[in] x2 'number' of rank 0
  subroutine bw_product__2 (x1, x2, k)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: k
    integer :: i, indexes(product(x1%shp))
    call number__slice_dim_indexes(x1%shp, k, indexes)
    do i = 1, x1%shp(k)
       !$omp parallel workshare
       where (indexes == i) x1%dv = x1%dv + x2%v(i) / x1%v * x1%dv(i)
       !$omp end parallel workshare
    end do
  end subroutine bw_product__2
  !> @}

  !> @defgroup reduction_operators_ssq_ Sum of Squares
  !! @{

  !> Sum of Squares - operator
  !! @param[in] x1 'number' of rank > 0
  !! @param[inout] x2 'number' of rank 0
  subroutine op_ssq (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    !$omp parallel workshare
    x2%v = sum(x1%v**2)
    !$omp end parallel workshare
  end subroutine op_ssq

  !> Sum of Squares - forward differentiation
  !! @param[in] x1 'number' of rank > 0
  !! @param[inout] x2 'number' of rank 0
  subroutine fw_ssq (x1, x2)
    implicit none
    type(number), intent(in) :: x1
    type(number), intent(inout) :: x2
    if (has_dx(x2)) x2%dv = sum(x1%dv)
  end subroutine fw_ssq

  !> Sum of Squares - backward differentiation
  !! @param[inout] x1 'number' of rank > 0
  !! @param[in] x2 'number' of rank 0
  subroutine bw_ssq (x1, x2)
    implicit none
    type(number), intent(inout) :: x1
    type(number), intent(in) :: x2
    if (has_dx(x2)) then
       !$omp parallel workshare
       x1%dv = x1%dv + dx_ssq(x1%v) * x2%dv(1)
       !$omp end parallel workshare
    end if
  end subroutine bw_ssq
  !> @}
  !> @}

  !> @defgroup dprob_operators_ Probability Density Function Operators
  !! @{

  !> @defgroup dprob_operators_ldexp_ Exponential Distribution - log-density (ldexp)
  !! @{

  !> ldexp - operator
  !! @param[in] y 'number', obeservations
  !! @param[in] lam 'number', rate parameter
  !! @param[inout] d 'number', log-density 
  subroutine op_ldexp (y, lam, ld)
    implicit none
    type(number), intent(in) :: y, lam
    type(number), intent(inout) :: ld
    if (mtrnk(lam) == 0) then
       !$omp parallel workshare
       ld%v = ldexp(y%v, lam%v(1))
       !$omp end parallel workshare
    else
       !$omp parallel workshare
       ld%v = ldexp(y%v, lam%v)
       !$omp end parallel workshare
    end if
  end subroutine op_ldexp

  !> ldexp - forward differentiation
  !! @param[in] y 'number', obeservations
  !! @param[in] lam 'number', rate parameter
  !! @param[inout] d 'number', log-density 
  subroutine fw_ldexp (y, lam, ld)
    implicit none
    type(number), intent(in) :: y, lam
    type(number), intent(inout) :: ld
    if (has_dx(ld)) then
       if (mtrnk(lam) == 0) then
          ld%dv = dx_ldexp(y%v, lam%v(1)) * lam%dv(1)
       else
          ld%dv = dx_ldexp(y%v, lam%v) * lam%dv
       end if
    end if
  end subroutine fw_ldexp

  !> ldexp - backward differentiation
  !! @param[in] y 'number', obeservations
  !! @param[inout] lam 'number', rate parameter
  !! @param[in] d 'number', log-density 
  subroutine bw_ldexp (y, lam, ld)
    implicit none
    type(number), intent(in) :: y, ld
    type(number), intent(inout) :: lam
    if (has_dx(ld)) then
       if (mtrnk(lam) == 0) then
          !$omp parallel workshare
          lam%dv = lam%dv + sum(dx_ldexp(y%v, lam%v(1)) * ld%dv)
          !$omp end parallel workshare
       else
          !$omp parallel workshare
          lam%dv = lam%dv + dx_ldexp(y%v, lam%v) * ld%dv
          !$omp end parallel workshare
       end if
    end if   
  end subroutine bw_ldexp
  !> @}

  !> @defgroup dprob_operators_ldlaplace_ Laplace Distribution - log-density (ldlaplace)
  !! @{

  !> ldlaplace - operator
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', location parameter
  !! @param[in] lam 'number', rate paramter
  !! @param[inout] ld 'number', log-density
  subroutine op_ldlaplace (y, mu, lam, ld)
    implicit none
    type(number), intent(in) :: y, mu, lam
    type(number), intent(inout) :: ld
    if (mtrnk(mu) == 0 .and. mtrnk(lam) == 0) then
       !$omp parallel workshare
       ld%v = ldlaplace(y%v, mu%v(1), lam%v(1))
       !$omp end parallel workshare
    else if (mtrnk(mu) == 0) then
       !$omp parallel workshare
       ld%v = ldlaplace(y%v, mu%v(1), lam%v)
       !$omp end parallel workshare
    else if (mtrnk(lam) == 0) then
       !$omp parallel workshare
       ld%v = ldlaplace(y%v, mu%v, lam%v(1))
       !$omp end parallel workshare
    else
       !$omp parallel workshare
       ld%v = ldlaplace(y%v, mu%v, lam%v)
       !$omp end parallel workshare
    end if 
  end subroutine op_ldlaplace

  !> ldlaplace - forward differentiation
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', location parameter
  !! @param[in] lam 'number', rate paramter
  !! @param[inout] ld 'number', log-density
  subroutine fw_ldlaplace (y, mu, lam, ld)
    implicit none
    type(number), intent(in) :: y, mu, lam
    type(number), intent(inout) :: ld
    if (has_dx(ld)) then
       if (has_dx(mu)) then
          if (mtrnk(mu) == 0 .and. mtrnk(lam) == 0) then
             ld%dv = ld%dv + dx_ldlaplace_mu(y%v, mu%v(1), lam%v(1)) * mu%dv(1)
          else if (mtrnk(mu) == 0) then
             ld%dv = ld%dv + dx_ldlaplace_mu(y%v, mu%v(1), lam%v) * mu%dv(1)
          else if (mtrnk(lam) == 0) then
             ld%dv = ld%dv + dx_ldlaplace_mu(y%v, mu%v(1), lam%v(1)) * mu%dv(1)
          else
             ld%dv = ld%dv + dx_ldlaplace_mu(y%v, mu%v, lam%v) * mu%dv
          end if
       end if
       if (has_dx(lam)) then
          if (mtrnk(mu) == 0 .and. mtrnk(lam) == 0) then
             ld%dv = ld%dv + dx_ldlaplace_lam(y%v, mu%v(1), lam%v(1)) * lam%dv
          else if (mtrnk(mu) == 0) then
             ld%dv = ld%dv + dx_ldlaplace_lam(y%v, mu%v(1), lam%v) * lam%dv
          else if (mtrnk(lam) == 0) then
             ld%dv = ld%dv + dx_ldlaplace_lam(y%v, mu%v, lam%v(1)) * lam%dv(1)
          else
             ld%dv = ld%dv + dx_ldlaplace_lam(y%v, mu%v, lam%v) * lam%dv
          end if
       end if
    end if
  end subroutine fw_ldlaplace

  !> ldlaplace - backward differentiation
  !! @param[in] y 'number', observations
  !! @param[inout] mu 'number', location parameter
  !! @param[inout] lam 'number', rate paramter
  !! @param[in] ld 'number', log-density
  subroutine bw_ldlaplace (y, mu, lam, ld)
    implicit none
    type(number), intent(in) :: y, ld
    type(number), intent(inout) :: mu, lam
    if (has_dx(lam)) then
       if (has_dx(mu)) then
          if (mtrnk(mu) == 0 .and. mtrnk(lam) == 0) then
             !$omp parallel workshare
             mu%dv = mu%dv + sum(dx_ldlaplace_mu(y%v, mu%v(1), lam%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(mu) == 0) then
             !$omp parallel workshare
             mu%dv = mu%dv + sum(dx_ldlaplace_mu(y%v, mu%v(1), lam%v) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(lam) == 0) then
             !$omp parallel workshare
             mu%dv = mu%dv + dx_ldlaplace_mu(y%v, mu%v, lam%v(1)) * ld%dv
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             mu%dv = mu%dv + dx_ldlaplace_mu(y%v, mu%v, lam%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
       if (has_dx(lam)) then
          if (mtrnk(mu) == 0 .and. mtrnk(lam) == 0) then
             !$omp parallel workshare
             lam%dv = lam%dv + sum(dx_ldlaplace_lam(y%v, mu%v(1), lam%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(mu) == 0) then
             !$omp parallel workshare
             lam%dv = lam%dv + dx_ldlaplace_lam(y%v, mu%v(1), lam%v) * ld%dv
             !$omp end parallel workshare
          else if (mtrnk(lam) == 0) then
             !$omp parallel workshare
             lam%dv = lam%dv + sum(dx_ldlaplace_lam(y%v, mu%v, lam%v(1)) * ld%dv)
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             lam%dv = lam%dv + dx_ldlaplace_lam(y%v, mu%v, lam%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
    end if
  end subroutine bw_ldlaplace
  !> @}

  !> @defgroup dprob_operators_ldbeta_ Beta Distribution - log-density (ldbeta)

  !> ldbeta - operator
  !! @param[in] y 'number', observations
  !! @param[in] a1,a2 'number', shape parameters
  !! @param[inout] ld 'number', log-density
  subroutine op_ldbeta (y, a1, a2, ld)
    implicit none
    type(number), intent(in) :: y, a1, a2
    type(number), intent(inout) :: ld
    if (mtrnk(a1) == 0 .and. mtrnk(a2) == 0) then
       !$omp parallel workshare
       ld%v = ldbeta(y%v, a1%v(1), a2%v(1))
       !$omp end parallel workshare
    else if (mtrnk(a1) == 0) then
       !$omp parallel workshare
       ld%v = ldbeta(y%v, a1%v(1), a2%v)
       !$omp end parallel workshare
    else if (mtrnk(a2) == 0) then
       !$omp parallel workshare
       ld%v = ldbeta(y%v, a1%v, a2%v(1))
       !$omp end parallel workshare
    else
       !$omp parallel workshare
       ld%v = ldbeta(y%v, a1%v, a2%v)
       !$omp end parallel workshare
    end if
  end subroutine op_ldbeta

  !> ldbeta - forward differentiation
  !! @param[in] y 'number', observations
  !! @param[in] a1,a2 'number', shape parameters
  !! @param[inout] ld 'number', log-density
  subroutine fw_ldbeta (y, a1, a2, ld)
    type(number), intent(in) :: y, a1, a2
    type(number), intent(inout) :: ld
    if (has_dx(ld)) then
       if (has_dx(a1)) then
          if (mtrnk(a1) == 0 .and. mtrnk(a2) == 0) then
             ld%dv = ld%dv + dx_ldbeta_a1(y%v, a1%v(1), a2%v(1)) * a1%dv(1)
          else if (mtrnk(a1) == 0) then
             ld%dv = ld%dv + dx_ldbeta_a1(y%v, a1%v(1), a2%v) * a1%dv(1)
          else if (mtrnk(a2) == 0) then
             ld%dv = ld%dv + dx_ldbeta_a1(y%v, a1%v, a2%v(1)) * a1%dv
          else
             ld%dv = ld%dv + dx_ldbeta_a1(y%v, a1%v, a2%v) * a1%dv
          end if
       end if
       if (has_dx(a2)) then
          if (mtrnk(a1) == 0 .and. mtrnk(a2) == 0) then
             ld%dv = ld%dv + dx_ldbeta_a2(y%v, a1%v(1), a2%v(1)) * a2%dv(1)
          else if (mtrnk(a1) == 0) then
             ld%dv = ld%dv + dx_ldbeta_a2(y%v, a1%v(1), a2%v) * a2%dv
          else if (mtrnk(a2) == 0) then
             ld%dv = ld%dv + dx_ldbeta_a2(y%v, a1%v, a2%v(1)) * a2%dv(1)
          else
             ld%dv = ld%dv + dx_ldbeta_a2(y%v, a1%v, a2%v) * a2%dv
          end if
       end if
    end if
  end subroutine fw_ldbeta

  !> ldbeta - backward differentiation
  !! @param[in] y 'number', observations
  !! @param[inout] a1,a2 'number', shape parameters
  !! @param[in] ld 'number', log-density
  subroutine bw_ldbeta (y, a1, a2, ld)
    implicit none
    type(number), intent(in) :: y, ld
    type(number), intent(inout) :: a1, a2
    if (has_dx(ld)) then
       if (has_dx(a1)) then
          if (mtrnk(a1) == 0 .and. mtrnk(a2) == 0) then
             !$omp parallel workshare
             a1%dv = a1%dv + sum(dx_ldbeta_a1(y%v, a1%v(1), a2%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(a1) == 0) then
             !$omp parallel workshare
             a1%dv = a1%dv + sum(dx_ldbeta_a1(y%v, a1%v(1), a2%v) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(a2) == 0) then
             !$omp parallel workshare
             a1%dv = a1%dv + dx_ldbeta_a1(y%v, a1%v, a2%v(1)) * ld%dv
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             a1%dv = a1%dv + dx_ldbeta_a1(y%v, a1%v, a2%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
       if (has_dx(a2)) then
          if (mtrnk(a1) == 0 .and. mtrnk(a2) == 0) then
             !$omp parallel workshare
             a2%dv = a2%dv + sum(dx_ldbeta_a2(y%v, a1%v(1), a2%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(a1) == 0) then
             !$omp parallel workshare
             a2%dv = a2%dv + dx_ldbeta_a2(y%v, a1%v(1), a2%v) * ld%dv
             !$omp end parallel workshare
          else if (mtrnk(a2) == 0) then
             !$omp parallel workshare
             a2%dv = a2%dv + sum(dx_ldbeta_a2(y%v, a1%v, a2%v(1)) * ld%dv)
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             a2%dv = a2%dv + dx_ldbeta_a2(y%v, a1%v, a2%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
    end if
  end subroutine bw_ldbeta
  !> @}

  !> @defgroup dprob_operators_ldgamma_ Gamma Distribution - log-density (ldgamma)

  !> ldgamma - operator
  !! @param[in] y 'number', observations
  !! @param[in] a 'number', shape parameter
  !! @param[in] b 'number', rate parameter
  !! @param[inout] ld 'number', log-density
  subroutine op_ldgamma (y, a, b, ld)
    implicit none
    type(number), intent(in) :: y, a, b
    type(number), intent(inout) :: ld
    if (mtrnk(a) == 0 .and. mtrnk(b) == 0) then
       !$omp parallel workshare
       ld%v = ldgamma(y%v, a%v(1), b%v(1))
       !$omp end parallel workshare
    else if (mtrnk(a) == 0) then
       !$omp parallel workshare
       ld%v = ldgamma(y%v, a%v(1), b%v)
       !$omp end parallel workshare
    else if (mtrnk(b) == 0) then
       !$omp parallel workshare
       ld%v = ldgamma(y%v, a%v, b%v(1))
       !$omp end parallel workshare
    else
       !$omp parallel workshare
       ld%v = ldgamma(y%v, a%v, b%v)
       !$omp end parallel workshare
    end if
  end subroutine op_ldgamma

  !> ldgamma - forward difference
  !! @param[in] y 'number', observations
  !! @param[in] a 'number', shape parameter
  !! @param[in] b 'number', rate parameter
  !! @param[inout] ld 'number', log-density
  subroutine fw_ldgamma (y, a, b, ld)
    implicit none
    type(number), intent(in) :: y, a, b
    type(number), intent(inout) :: ld
    if (has_dx(ld)) then
       if (has_dx(a)) then
          if (mtrnk(a) == 0 .and. mtrnk(b) == 0) then
             ld%dv = ld%dv + dx_ldgamma_a(y%v, a%v(1), b%v(1)) * a%dv(1)
          else if (mtrnk(a) == 0) then
             ld%dv = ld%dv + dx_ldgamma_a(y%v, a%v(1), b%v) * a%dv(1)
          else if (mtrnk(b) == 0) then
             ld%dv = ld%dv + dx_ldgamma_a(y%v, a%v, b%v(1)) * a%dv
          else
             ld%dv = ld%dv + dx_ldgamma_a(y%v, a%v, b%v) * a%dv
          end if
       end if
       if (has_dx(b)) then
          if (mtrnk(a) == 0 .and. mtrnk(b) == 0) then
             ld%dv = ld%dv + dx_ldgamma_b(y%v, a%v(1), b%v(1)) * b%dv(1)
          else if (mtrnk(a) == 0) then
             ld%dv = ld%dv + dx_ldgamma_b(y%v, a%v(1), b%v) * b%dv
          else if (mtrnk(b) == 0) then
             ld%dv = ld%dv + dx_ldgamma_b(y%v, a%v, b%v(1)) * b%dv(1)
          else
             ld%dv = ld%dv + dx_ldgamma_b(y%v, a%v, b%v) * b%dv
          end if
       end if
    end if
  end subroutine fw_ldgamma

  !> ldgamma - backward difference
  !! @param[in] y 'number', observations
  !! @param[inout] a 'number', shape parameter
  !! @param[inout] b 'number', rate parameter
  !! @param[in] ld 'number', log-density
  subroutine bw_ldgamma (y, a, b, ld)
    implicit none
    type(number), intent(in) :: y, ld
    type(number), intent(inout) :: a, b
    if (has_dx(ld)) then
       if (has_dx(a)) then
          if (mtrnk(a) == 0 .and. mtrnk(b) == 0) then
             !$omp parallel workshare
             a%dv = a%dv + sum(dx_ldgamma_a(y%v, a%v(1), b%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(a) == 0) then
             !$omp parallel workshare
             a%dv = a%dv + sum(dx_ldgamma_a(y%v, a%v(1), b%v) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(b) == 0) then
             !$omp parallel workshare
             a%dv = a%dv + dx_ldgamma_a(y%v, a%v, b%v(1)) * ld%dv
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             a%dv = a%dv + dx_ldgamma_a(y%v, a%v, b%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
       if (has_dx(b)) then
          if (mtrnk(a) == 0 .and. mtrnk(b) == 0) then
             !$omp parallel workshare
             b%dv = b%dv + sum(dx_ldgamma_b(y%v, a%v(1), b%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(a) == 0) then
             !$omp parallel workshare
             b%dv = b%dv + dx_ldgamma_b(y%v, a%v(1), b%v) * ld%dv
             !$omp end parallel workshare
          else if (mtrnk(b) == 0) then
             !$omp parallel workshare
             b%dv = b%dv + sum(dx_ldgamma_b(y%v, a%v, b%v(1)) * ld%dv)
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             b%dv = dx_ldgamma_b(y%v, a%v, b%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
    end if
  end subroutine bw_ldgamma
  !> @}

  !> @defgroup dprob_operators_ldnorm_ Normal Distribution - log-density
  !! @{

  !> ldnorm - operator
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number' mean
  !! @param[in] s 'number', standard deviation
  !! @param[inout] ld 'number', log-density
  subroutine op_ldnorm (y, mu, s, ld)
    implicit none
    type(number), intent(in) :: y, mu, s
    type(number), intent(inout) :: ld
    if (mtrnk(mu) == 0 .and. mtrnk(s) == 0) then
       !$omp parallel workshare
       ld%v = ldnorm(y%v, mu%v(1), s%v(1))
       !$omp end parallel workshare
    else if (mtrnk(mu) == 0) then
       !$omp parallel workshare
       ld%v = ldnorm(y%v, mu%v(1), s%v)
       !$omp end parallel workshare
    else if (mtrnk(s) == 0) then
       !$omp parallel workshare
       ld%v = ldnorm(y%v, mu%v, s%v(1))
       !$omp end parallel workshare
    else
       !$omp parallel workshare
       ld%v = ldnorm(y%v, mu%v, s%v)
       !$omp end parallel workshare
    end if
  end subroutine op_ldnorm

  !> ldnorm - forward differenatiation
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number' mean
  !! @param[in] s 'number', standard deviation
  !! @param[inout] ld 'number', log-density
  subroutine fw_ldnorm (y, mu, s, ld)
    implicit none
    type(number), intent(in) :: y, mu, s
    type(number), intent(inout) :: ld
    if (has_dx(ld)) then
       if (has_dx(mu)) then
          if (mtrnk(mu) == 0 .and. mtrnk(s) == 0) then
             ld%dv = ld%dv + dx_ldnorm_mu(y%v, mu%v(1), s%v(1)) * mu%dv(1)
          else if (mtrnk(mu) == 0) then
             ld%dv = ld%dv + dx_ldnorm_mu(y%v, mu%v(1), s%v) * mu%dv(1)
          else if (mtrnk(s) == 0) then
             ld%dv = ld%dv + dx_ldnorm_mu(y%v, mu%v, s%v(1)) * mu%dv
          end if
       end if
       if (has_dx(s)) then
          if (mtrnk(mu) == 0 .and. mtrnk(s) == 0) then
             ld%dv = ld%dv + dx_ldnorm_s(y%v, mu%v(1), s%v(1)) * s%dv
          else if (mtrnk(mu) == 0) then
             ld%dv = ld%dv + dx_ldnorm_s(y%v, mu%v(1), s%v) * s%dv
          else if (mtrnk(s) == 0) then
             ld%dv = ld%dv + dx_ldnorm_s(y%v, mu%v, s%v(1)) * s%dv(1)
          else
             ld%dv = ld%dv + dx_ldnorm_s(y%v, mu%v, s%v) * s%dv
          end if
       end if
    end if
  end subroutine fw_ldnorm

  !> ldnorm - backward differentiation
  !! @param[in] y 'number', observations
  !! @param[inout] mu 'number' mean
  !! @param[inout] s 'number', standard deviation
  !! @param[in] ld 'number', log-density
  subroutine bw_ldnorm (y, mu, s, ld)
    implicit none
    type(number), intent(in) :: y, ld
    type(number), intent(inout) :: mu, s
    if (has_dx(ld)) then
       if (has_dx(mu)) then
          if (mtrnk(mu) == 0 .and. mtrnk(s) == 0) then
             !$omp parallel workshare
             mu%dv = mu%dv + sum(dx_ldnorm_mu(y%v, mu%v(1), s%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(mu) == 0) then
             !$omp parallel workshare
             mu%dv = mu%dv + sum(dx_ldnorm_mu(y%v, mu%v(1), s%v) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(s) == 0) then
             !$omp parallel workshare
             mu%dv = mu%dv + dx_ldnorm_mu(y%v, mu%v, s%v(1)) * ld%dv
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             mu%dv = mu%dv + dx_ldnorm_mu(y%v, mu%v, s%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
       if (has_dx(s)) then
          if (mtrnk(mu) == 0 .and. mtrnk(s) == 0) then
             !$omp parallel workshare
             s%dv = s%dv + sum(dx_ldnorm_s(y%v, mu%v(1), s%v(1)) * ld%dv)
             !$omp end parallel workshare
          else if (mtrnk(mu) == 0) then
             !$omp parallel workshare
             s%dv = s%dv + dx_ldnorm_s(y%v, mu%v(1), s%v) * ld%dv
             !$omp end parallel workshare
          else if (mtrnk(s) == 0) then
             !$omp parallel workshare
             s%dv = s%dv + sum(dx_ldnorm_s(y%v, mu%v, s%v(1)) * ld%dv)
             !$omp end parallel workshare
          else
             !$omp parallel workshare
             s%dv = s%dv + dx_ldnorm_s(y%v, mu%v, s%v) * ld%dv
             !$omp end parallel workshare
          end if
       end if
    end if
  end subroutine bw_ldnorm
  !> @}

  !> @defgroup operators_lkh_norm__1_ Normal Likelohood for Independent Variables
  !! @{

  !> lkh_norm__1 - operator
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', mean
  !! @param[in] s 'number', standard deviation
  !! @param[inout] L 'number' of mtrnk 0, log-likelihood
  subroutine op_lkh_norm__1 (y, mu, s, L)
    implicit none
    type(number), intent(in) :: y, mu, s
    type(number), intent(inout) :: L
    !$omp parallel workshare
    L%v = sum(ldnorm(y%v, mu%v, s%v))
    !$omp end parallel workshare
  end subroutine op_lkh_norm__1

  !> lkh_norm__1 - forward differentiation
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', mean
  !! @param[in] s 'number', standard deviation
  !! @param[inout] L 'number' of rank 0, log-likelihood
  subroutine fw_lkh_norm__1 (y, mu, s, L)
    implicit none
    type(number), intent(in) :: y, mu, s
    type(number), intent(inout) :: L
    if (has_dx(L)) then
       if (has_dx(mu)) L%dv = L%dv + sum(dx_ldnorm_mu(y%v, mu%v, s%v) * mu%dv)
       if (has_dx(s)) L%dv = L%dv + sum(dx_ldnorm_s(y%v, mu%v, s%v) * s%dv)
    end if    
  end subroutine fw_lkh_norm__1

  !> lkh_norm__1 - backward differentiation
  !! @param[in] y 'number', observations
  !! @param[inout] mu 'number', mean
  !! @param[inout] s 'number', standard deviation
  !! @param[in] L 'number' of rank 0, log-likelihood
  subroutine bw_lkh_norm__1 (y, mu, s, L)
    implicit none
    type(number), intent(in) :: y, L
    type(number), intent(inout) :: mu, s
    if (has_dx(L)) then
       if (has_dx(mu)) then
          !$omp parallel workshare
          mu%dv = mu%dv + dx_ldnorm_mu(y%v, mu%v, s%v) * L%dv(1)
          !$omp end parallel workshare
       end if
       if (has_dx(s)) then
          !$omp parallel workshare
          s%dv = s%dv + dx_ldnorm_s(y%v, mu%v, s%v) * L%dv(1)
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_lkh_norm__1
  !> @}

  !> @defgroup operators_lkh_norm__2_ Weighted Normal Likelohood for Independent Variables
  !! @{
  
  !> lkh_norm__2 - operator
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', mean
  !! @param[in] s 'number', standard deviation
  !! @param[in] w 'number', weights
  !! @param[inout] L 'number' of rank 0, log-likelihood
  subroutine op_lkh_norm__2 (y, mu, s, w, L)
    implicit none
    type(number), intent(in) :: y, mu, s, w
    type(number), intent(inout) :: L
    !$omp parallel workshare
    L%v = sum(ldnorm(y%v, mu%v, s%v, w%v))
    !$omp end parallel workshare
  end subroutine op_lkh_norm__2

  !> lkh_norm__2 - forward differentiation
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', mean
  !! @param[in] s 'number', standard deviation
  !! @param[in] w 'number', weights
  !! @param[inout] L 'number' of rank 0, log-likelihood
  subroutine fw_lkh_norm__2 (y, mu, s, w, L)
    implicit none
    type(number), intent(in) :: y, mu, s, w
    type(number), intent(inout) :: L
    if (has_dx(L)) then
       if (has_dx(mu)) then
          !$omp parallel workshare
          L%dv = L%dv + sum(dx_ldnorm_mu(y%v, mu%v, s%v, w%v) * mu%dv)
          !$omp end parallel workshare
       end if
       if (has_dx(s)) then
          !$omp parallel workshare
          L%dv = L%dv + sum(dx_ldnorm_s(y%v, mu%v, s%v, w%v) * s%dv)
          !$omp end parallel workshare
       end if
    end if
  end subroutine fw_lkh_norm__2

  !> lkh_norm__2 - backward differentiation
  !! @param[in] y 'number', observations
  !! @param[inout] mu 'number', mean
  !! @param[inout] s 'number', standard deviation
  !! @param[inout] w 'number', weights
  !! @param[in] L 'number' of rank 0, log-likelihood
  subroutine bw_lkh_norm__2 (y, mu, s, w, L)
    implicit none
    type(number), intent(in) :: y, w, L
    type(number), intent(inout) :: mu, s
    if (has_dx(L)) then
       if (has_dx(mu)) then
          !$omp parallel workshare
          mu%dv = mu%dv + dx_ldnorm_mu(y%v, mu%v, s%v, w%v) * L%dv(1)
          !$omp end parallel workshare
       end if
       if (has_dx(s)) then
          !$omp parallel workshare
          s%dv = s%dv + dx_ldnorm_s(y%v, mu%v, s%v, w%v) * L%dv(1)
          !$omp end parallel workshare
       end if
    end if
  end subroutine bw_lkh_norm__2


  !> @defgroup operators_ldmvnorm__1 Multivariate Normal Probability Distribution - log-density
  !! @{

  !> ldmvnorm__1 - operator
  !! @param[in] y 'number', observations
  !! @param[in] mu 'number', mean
  !! @param[in] E 'number', covariance matrix
  !! @param[inout] ld 'number' of rank 0, log-density
  subroutine op_ldmvnorm__1 (y, mu, E, ld)
    implicit none
    type(number), intent(in) :: y, mu, E
    type(number), intent(inout) :: ld
    call do_within_critical('op_ldmvnorm__1', mod_operators_name_, private_do)
  contains
    subroutine private_do
      real(kind=dp_), pointer :: xE(:,:)
      integer :: info
      info = 0
      call number__with_shape(E, xE, .false.)
      call ldmvnorm__1(y%v, mu%v, xE, ld%v(1), info)
      call assert(info == 0, info, 'ldmvnorm__1')
    end subroutine private_do
  end subroutine op_ldmvnorm__1

  !> ldmvnorm__1 - backward differentiation
  !! @param[in] y 'number', observations
  !! @param[inout] mu 'number', mean
  !! @param[inout] E 'number', covariance matrix
  !! @param[in] ld 'number' of rank 0, log-density
  !! @todo think about parallel design
  subroutine bw_ldmvnorm__1 (y, mu, E, ld)
    implicit none
    type(number), intent(in) :: y, mu, E
    type(number), intent(inout) :: ld
    call do_within_critical('bw_ldmvnorm__1', mod_operators_name_, private_do)
  contains
    subroutine private_do
      real(kind=dp_), pointer :: xE(:,:)
      real(kind=dp_), pointer :: dxE(:,:)
      integer :: info
      info = 0
      call number__with_shape(E, xE, .false.)
      if (has_dx(mu) .and. has_dx(E)) then
         call number__with_shape(E, dxE, .true.)
         call dx_bw_ldmvnorm__1(y%v, mu%v, xE, ld%v(1), info, mu%dv, dxE)
      else if (has_dx(mu)) then
         call dx_bw_ldmvnorm__1(y%v, mu%v, xE, ld%v(1), info, mu%dv)
      else if (has_dx(E)) then
         call number__with_shape(E, dxE, .true.)
         call dx_bw_ldmvnorm__1(y%v, mu%dv, xE, ld%v(1), info, dE=dxE)
      end if   
      call assert(info == 0, info, 'ldmvnorm__1')
    end subroutine private_do
  end subroutine bw_ldmvnorm__1

  ! subroutine op_mvnorm_posterior (mu, y, A11, E12, E22, pmu, pE)
  !   implicit none
  !   type(number), intent(in) :: mu, y, A11, E12, E22
  !   type(number), intent(inout) :: pmu, pE
  !   real(kind=dp_), pointer, dimension(:,:) :: xA11, xE12, xpE
  !   call number__with_shape(A11, xA11, .false.)
  !   call number__with_shape(E12, xE12, .false.)
  !   call number__with_shape(pE, xpE, .false.)
  !   pmu%v = mu%v
  !   pE%v = E22%v
  !   call MVNORM_POSTERIOR(xA11, xE12, y%v, pmu%v, xpE)
  ! end subroutine op_mvnorm_posterior

  ! subroutine op_mvnormPosteriorMu (mu, y, A11, E12, pmu)
  !   implicit none
  !   type(number), intent(in) :: mu, y, A11, E12
  !   type(number), intent(inout) :: pmu
  !   real(kind=dp_), pointer, dimension(:,:) :: xA11, xE12
  !   call number__with_shape(A11, xA11, .false.)
  !   call number__with_shape(E12, xE12, .false.)
  !   pmu%v = mu%v
  !   call MVNORM_POSTERIOR(xA11, xE12, y%v, pmu%v)
  ! end subroutine op_mvnormPosteriorMu

  ! subroutine op_mvnormPosteriorE (A11, E12, E22, pE)
  !   implicit none
  !   type(number), intent(in) :: A11, E12, E22
  !   type(number), intent(inout) :: pE
  !   real(kind=dp_), pointer, dimension(:,:) :: xA11, xE12, xpE
  !   call number__with_shape(A11, xA11, .false.)
  !   call number__with_shape(E12, xE12, .false.)
  !   call number__with_shape(pE, xpE, .false.)
  !   pE%v = E22%v
  !   call MVNORM_POSTERIOR(xA11, xE12, E22=xPE)
  ! end subroutine op_mvnormPosteriorE
  
  !> @defgroup operators_kernels_ Kernel Operators
  !! @{

  !> @defgroup operators_kernels_ksqexp_ Square Exponential Kernel
  !! @{

  !> ksqexp - operator
  !! @param[in] x1 'number', feature vector
  !! @param[in] x2 'number', feature vector
  !! @param[in] a 'number', amplitude parameter
  !! @param[in] b 'number', rate parameter
  !! @param[inout] k 'number', kernel value
  subroutine op_ksqexp (x1, x2, a, b, k)
    implicit none
    type(number), intent(in) :: x1, x2, a, b
    type(number), intent(inout) :: k
    call do_within_critical('op_ksqexp', mod_operators_name_, private_ksqexp)
 contains
    subroutine private_ksqexp
      if (mtrnk(x1) == 1 .and. mtrnk(x2) == 1) then
         call private_ksqexp_1_1
      else if (mtrnk(x1) == 1 .and. mtrnk(x2) == 2) then
         call private_ksqexp_1_2
      else if (mtrnk(x1) == 2 .and. mtrnk(x2) == 2) then
         call private_ksqexp_2_2
      else
         call raise_error('x1 or x2 has wrong rank', err_generic_)
      end if
    end subroutine private_ksqexp
    subroutine private_ksqexp_1_1
      if (mtrnk(b) == 0) then
         !$omp parallel workshare
         k%v = product(sqexp(x1%v, x2%v, a%v(1), b%v(1)))
         !$omp end parallel workshare
      else if (mtrnk(b) == 1) then
         !$omp parallel workshare
         k%v = product(sqexp(x1%v, x2%v, a%v(1), b%v))
         !$omp end parallel workshare
      else
         call raise_error('b has worng rank', err_generic_)
      end if
    end subroutine private_ksqexp_1_1
    subroutine private_ksqexp_1_2
      if (mtrnk(b) == 0) then
         call private_ksqexp0_1_2
      else if (mtrnk(b) == 1) then
         call private_ksqexp1_1_2
      else
         call raise_error('b has wrong rank', err_generic_)
      end if
    end subroutine private_ksqexp_1_2
    subroutine private_ksqexp0_1_2
      real(kind=dp_), pointer :: xx2(:,:)
      integer :: i
      call number__with_shape(x2, xx2, .false.)
      !$omp parallel
      !$omp do
      do i = 1, size(xx2, 1)
         k%v(i) = ksqexp(x1%v, xx2(i,:), a%v(1), b%v(1))
      end do
      !$omp end do
      !$omp end parallel
    end subroutine private_ksqexp0_1_2
    subroutine private_ksqexp1_1_2
      real(kind=dp_), pointer :: xx2(:,:)
      integer :: i
      call number__with_shape(x2, xx2, .false.)
      !$omp parallel do
      do i = 1, mtsz(x1)
         k%v(i) = ksqexp(x1%v, xx2(i,:), a%v(1), b%v)
      end do
      !$omp end parallel do
    end subroutine private_ksqexp1_1_2
    subroutine private_ksqexp_2_2
      if (mtrnk(b) == 0) then
         call private_ksqexp0_2_2
      else if (mtrnk(b) == 1) then
         call private_ksqexp1_2_2
      else
         call raise_error('b has wrong rank', err_generic_)
      end if
    end subroutine private_ksqexp_2_2
    subroutine private_ksqexp0_2_2
      real(kind=dp_), pointer :: xx1(:,:), xx2(:,:), xk(:,:)
      integer :: i, j
      call number__with_shape(x1, xx1, .false.)
      call number__with_shape(x2, xx2, .false.)
      call number__with_shape(k, xk, .false.)
      !$omp parallel do
      do i = 1, size(xx1, 1)
         do j = 1, size(xx2, 1)
            xk(i,j) = ksqexp(xx1(i,:), xx2(j,:), a%v(1), b%v(1))
         end do
      end do
      !$omp end parallel do
    end subroutine private_ksqexp0_2_2
    subroutine private_ksqexp1_2_2
      real(kind=dp_), pointer :: xx1(:,:), xx2(:,:), xk(:,:)
      integer :: i, j
      call number__with_shape(x1, xx1, .false.)
      call number__with_shape(x2, xx2, .false.)
      call number__with_shape(k, xk, .false.)
      !$omp parallel do
      do i = 1, size(xx1, 1)
         do j = 1, size(xx2, 1)
            xk(i,j) = ksqexp(xx1(i,:), xx2(j,:), a%v(1), b%v)
         end do
      end do
      !$omp end parallel do
    end subroutine private_ksqexp1_2_2
  end subroutine op_ksqexp

  !> ksqexp - backward differentiation
  !! @param[in] x1 'number', feature vector
  !! @param[in] x2 'number', feature vector
  !! @param[in] a 'number', amplitude parameter
  !! @param[in] b 'number', rate parameter
  !! @param[inout] k 'number', kernel value
  subroutine bw_ksqexp (x1, x2, a, b, k)
    implicit none
    type(number), intent(in) :: k
    type(number), intent(inout) :: x1, x2, a, b
    call do_within_critical('bw_ksqexp', mod_operators_name_, private_ksqexp)
  contains
    subroutine private_ksqexp
      if (mtrnk(x1) == 1 .and. mtrnk(x2) == 1) then
         call private_ksqexp_1_1
      else if (mtrnk(x1) == 1 .and. mtrnk(x2) == 2) then
         call private_ksqexp_1_2
      else if (mtrnk(x1) == 2 .and. mtrnk(x2) == 2) then
         call private_ksqexp_2_2
      else
         call raise_error('x1 or x2 has wrong rank', err_generic_)
      end if
    end subroutine private_ksqexp
    subroutine private_ksqexp_1_1
      if (mtrnk(b) == 0) then
         call private_ksqexp0_1_1
      else if (mtrnk(b) == 1) then
         call private_ksqexp1_1_1
      else
         call raise_error('b has wrong rank', err_generic_)
      end if
    end subroutine private_ksqexp_1_1
    subroutine private_ksqexp0_1_1
      if (has_dx(k)) then
         if (has_dx(x1)) x1%dv = x1%dv + dx_ksqexp_x1(x1%v, x2%v, a%v(1), b%v(1)) * k%dv(1)
         if (has_dx(x2)) x2%dv = x2%dv + dx_ksqexp_x2(x1%v, x2%v, a%v(1), b%v(1)) * k%dv(1)
         if (has_dx(a)) a%dv = a%dv + dx_ksqexp_a(x1%v, x2%v, a%v(1), b%v(1)) * k%dv(1)
         if (has_dx(b)) b%dv = b%dv + dx_ksqexp_b(x1%v, x2%v, a%v(1), b%v(1)) * k%dv(1)
      end if
    end subroutine private_ksqexp0_1_1
    subroutine private_ksqexp1_1_1
      if (has_dx(k)) then
         if (has_dx(x1)) x1%dv = x1%dv + dx_ksqexp_x1(x1%v, x2%v, a%v(1), b%v) * k%dv(1)
         if (has_dx(x2)) x2%dv = x2%dv + dx_ksqexp_x2(x1%v, x2%v, a%v(1), b%v) * k%dv(1)
         if (has_dx(a)) a%dv = a%dv + dx_ksqexp_a(x1%v, x2%v, a%v(1), b%v) * k%dv(1)
         if (has_dx(b)) b%dv = b%dv + dx_ksqexp_b(x1%v, x2%v, a%v(1), b%v) * k%dv(1)
      end if
    end subroutine private_ksqexp1_1_1
    subroutine private_ksqexp_1_2
      if (mtrnk(b) == 0) then
         call private_ksqexp0_1_2
      else if (mtrnk(b) == 1) then
         call private_ksqexp1_1_2
      else
         call raise_error('b has wrong rank', err_generic_)
      end if
    end subroutine private_ksqexp_1_2
    subroutine private_ksqexp0_1_2
      real(kind=dp_), pointer :: xx2(:,:), dxx2(:,:)
      integer :: i
      call number__with_shape(x2, xx2, .false.)
      call number__with_shape(x2, dxx2, .true.)
      do i = 1, mtsz(x1)
         if (has_dx(x1)) x1%dv = x1%dv + dx_ksqexp_x1(x1%v, xx2(i,:), a%v(1), b%v(1)) * k%dv(i)
         if (has_dx(x2)) dxx2(i,:) = dxx2(i,:) + dx_ksqexp_x2(x1%v, xx2(i,:), a%v(1), b%v(1)) * k%dv(i)
         if (has_dx(a)) a%dv = a%dv(1) + dx_ksqexp_a(x1%v, xx2(i,:), a%v(1), b%v(1)) * k%dv(i)
         if (has_dx(b)) b%dv = b%dv(1) + dx_ksqexp_b(x1%v, xx2(i,:), a%v(1), b%v(1)) * k%dv(i)
      end do
    end subroutine private_ksqexp0_1_2
    subroutine private_ksqexp1_1_2
      real(kind=dp_), pointer :: xx2(:,:), dxx2(:,:)
      integer :: i
      call number__with_shape(x2, xx2, .true.)
      call number__with_shape(x2, dxx2, .true.)
      do i = 1, size(xx2, 1)
         if (has_dx(x1)) x1%dv = x1%dv + dx_ksqexp_x1(x1%dv, xx2(i,:), a%v(1), b%v) * k%dv(i)
         if (has_dx(x2)) dxx2(i,:) = dxx2(i,:) + dx_ksqexp_x2(x1%v, xx2(i,:), a%v(1), b%v) * k%dv(i)
         if (has_dx(a)) a%dv(1) = a%dv(1) + dx_ksqexp_a(x1%v, xx2(i,:), a%v(1), b%v) * k%dv(i)
         if (has_dx(b)) b%dv = b%dv + dx_ksqexp_b(x1%dv, xx2(i,:), a%v(1), b%v) * k%dv(i)
      end do
    end subroutine private_ksqexp1_1_2
    subroutine private_ksqexp_2_2
      if (mtrnk(b) == 0) then
         call private_ksqexp0_2_2
      else if (mtrnk(b) == 1) then
         call private_ksqexp1_2_2
      else
         call raise_error('b has wrong rank', err_generic_)
      end if
    end subroutine private_ksqexp_2_2
    subroutine private_ksqexp0_2_2
      real(kind=dp_), pointer :: xx1(:,:), xx2(:,:)
      real(kind=dp_), pointer :: dxx1(:,:), dxx2(:,:), dxk(:,:)
      integer :: i, j
      call number__with_shape(x1, xx1, .false.)
      call number__with_shape(x2, xx2, .false.)
      call number__with_shape(x1, dxx1, .true.)
      call number__with_shape(x2, dxx2, .true.)
      call number__with_shape(k, dxk, .true.)
      do i = 1, size(xx1, 1)
         do j = 1, size(xx2, 1)
            if (has_dx(x1)) dxx1(i,:) = dxx1(i,:) + dx_ksqexp_x1(xx1(i,:), xx2(j,:), a%v(1), b%v(1)) * dxk(i,j)
            if (has_dx(x2)) dxx2(j,:) = dxx2(j,:) + dx_ksqexp_x2(xx1(i,:), xx2(j,:), a%v(1), b%v(1)) * dxk(i,j)
            if (has_dx(a)) a%dv(1) = a%dv(1) + dx_ksqexp_a(xx1(i,:), xx2(j,:), a%v(1), b%v(1)) * dxk(i,j)
            if (has_dx(b)) b%dv(1) = b%dv(1) + dx_ksqexp_b(xx1(i,:), xx2(j,:), a%v(1), b%dv(1)) * dxk(i,j)
         end do
      end do
    end subroutine private_ksqexp0_2_2
    subroutine private_ksqexp1_2_2
      real(kind=dp_), pointer :: xx1(:,:), xx2(:,:)
      real(kind=dp_), pointer :: dxx1(:,:), dxx2(:,:), dxk(:,:)
      integer :: i, j
      call number__with_shape(x1, xx1, .false.)
      call number__with_shape(x2, xx2, .false.)
      call number__with_shape(x1, dxx1, .true.)
      call number__with_shape(x2, dxx2, .true.)
      call number__with_shape(k, dxk, .true.) 
      do i = 1, size(xx1, 1)
         do j = 1, size(xx2, 1)
            if (has_dx(x1)) dxx1(i,:) = dxx1(i,:) + dx_ksqexp_x1(xx1(i,:), xx2(j,:), a%v(1), b%v) * dxk(i,j)
            if (has_dx(x2)) dxx2(j,:) = dxx2(j,:) + dx_ksqexp_x2(xx1(i,:), xx2(j,:), a%v(1), b%v) * dxk(i,j)
            if (has_dx(a)) a%dv(1) = a%dv(1) + dx_ksqexp_a(xx1(i,:), xx2(j,:), a%v(1), b%v) * dxk(i,j)
            if (has_dx(b)) b%dv = b%dv + dx_ksqexp_b(xx1(i,:), xx2(j,:), a%v(1), b%v) * dxk(i,j)
         end do
      end do
    end subroutine private_ksqexp1_2_2
  end subroutine bw_ksqexp
  !> @}
  !> @}

end module operators






