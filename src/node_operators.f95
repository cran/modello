module node_operators

  use env
  use types
  use registers, only: NUMBERS_, NODES_
  use errwarn
  use operators

  implicit none

  private

  public &
       node__op, &
       node__fw, &
       node__bw, &
       node__bw_zero

  character(len=*), parameter :: mod_node_operators_name_ = 'node_operators'


contains

  !> Applies the node operators to the its inputs and store the result in the output.
  !! @author Filippo Monari
  !! @param[in] ndi integer, node index in the NODES_ array
  subroutine node__op (ndi)
    implicit none
    integer, intent(in) :: ndi
    call do_within_critical('node__op', mod_node_operators_name_, private_op)
  contains
    subroutine private_op
      type(node), pointer :: nd
      nd => NODES_(ndi)
      select case (nd%op)
      case (op_add_id_)
         call node__op_add(nd)
      case (op_sub_id_)
         call node__op_sub(nd)
      case (op_mult_id_)
         call node__op_mult(nd)
      case (op_pow_id_)
         call node__op_pow(nd)
      case (op_div_id_)
         call node__op_div(nd)
      case (op_abs_id_)
         call node__op_abs(nd)
      case (op_exp_id_)
         call node__op_exp(nd)
      case (op_log_id_)
         call node__op_log(nd)
      case (op_sin_id_)
         call node__op_sin(nd)
      case (op_cos_id_)
         call node__op_cos(nd)
      case (op_tan_id_)
         call node__op_tan(nd)
      case (op_sinh_id_)
         call node__op_sinh(nd)
      case (op_cosh_id_)
         call node__op_cosh(nd)
      case (op_tanh_id_)
         call node__op_tanh(nd)
      case (op_dgemm1_id_)
         call node__op_dgemm__1(nd)
      case (op_dgemm2_id_)
         call node__op_dgemm__2(nd)
      case (op_dgemm3_id_)
         call node__op_dgemm__3(nd)
      case (op_dgemm4_id_)
         call node__op_dgemm__4(nd)
      case (op_dp_gemv1_id_)
         call node__op_dp_gemv__1(nd)
      case (op_dp_gemv2_id_)
         call node__op_dp_gemv__2(nd)
      case (op_dp_gemv3_id_)
         call node__op_dp_gemv__3(nd)
      case (op_dp_gemv4_id_)
         call node__op_dp_gemv__4(nd)
      case (op_dp_ger1_id_)
         call node__op_dp_ger__1(nd)
      case (op_dp_ger2_id_)
         call node__op_dp_ger__2(nd)
      case (op_dp_ger3_id_)
         call node__op_dp_ger__3(nd)
      case (op_dp_dot_id_)
         call node__op_dp_dot(nd)
      case (op_invMat_id_)
         call node__op_invMat(nd)
      case (op_sum_id_)
         call node__op_sum(nd)
      case (op_sum2_id_)
         call node__op_sum__2(nd)
      case (op_product1_id_)
         call node__op_product__1(nd)
      case (op_product2_id_)
         call node__op_product__2(nd)
      case (op_ssq_id_)
         call node__op_ssq(nd)
      case (op_sigmoid_id_)
         call node__op_sigmoid(nd)
      case (op_relu_id_)
         call node__op_relu(nd)
      case (op_swish_id_)
         call node__op_swish(nd)
      case (op_elu_id_)
         call node__op_elu(nd)
      case (op_softmax1_id_)
         call node__op_softmax__1(nd)
      case (op_softmax2_id_)
         call node__op_softmax__2(nd)
      case (op_bin_entropy_id_)
         call node__op_bin_entropy(nd)
      case (op_cross_entropy1_id_)
         call node__op_cross_entropy(nd)
      case (op_slice_id_)
         call node__op_slice(nd)
      case (op_flat_slice_id_)
         call node__op_flat_slice(nd)
      case (op_bind_id_)
         call node__op_bind(nd)
      case (op_embeddings_id_)
         call node__op_embeddings(nd)
      case (op_mse_id_)
         call node__op_mse(nd)
      case (op_mae_id_)
         call node__op_mae(nd)
      case (op_ldexp_id_)
         call node__op_ldexp(nd)
      case (op_ldlaplace_id_)
         call node__op_ldlaplace(nd)
      case (op_ldbeta_id_)
         call node__op_ldbeta(nd)
      case (op_ldgamma_id_)
         call node__op_ldgamma(nd)
      case (op_ldnorm_id_)
         call node__op_ldnorm(nd)
      case (op_ldmvnorm1_id_)
         call node__op_ldmvnorm__1(nd)
      case (op_lkh_norm1_id_)
         call node__op_lkh_norm__1(nd)
      case (op_lkh_norm2_id_)
         call node__op_lkh_norm__2(nd)
      case (op_ksqexp_id_)
         call node__op_ksqexp(nd)
      case ( &
           op_contiguous_slice_id_, &
           op_reshape_id_, &
           op_drop_shape_id_ &
           )
         !pass
      case default
         call raise_error('nd%typ', err_unknwnVal_)
      end select
    end subroutine private_op
  end subroutine node__op

  !> Applies forward differentiation to a node depeding on its operators.
  !! @author Filippo Monari
  !! @param[in] ndi integer, node index in the NODES_ array
  !! @todo all operators have to be tested
  subroutine node__fw (ndi)
    implicit none
    integer, intent(in) :: ndi
    call do_within_critical('node_fw', mod_node_operators_name_, private_fw)
  contains
    subroutine private_fw
      type(node), pointer :: nd
      nd => NODES_(ndi)
      select case (nd%op)
      case (op_add_id_)
         call node__fw_add(nd)
      case (op_sub_id_)
         call node__fw_sub(nd)
      case (op_mult_id_)
         call node__fw_mult(nd)
      case (op_pow_id_)
         call node__fw_pow(nd)
      case (op_div_id_)
         call node__fw_div(nd)
      case (op_abs_id_)
         call node__fw_abs(nd)
      case (op_exp_id_)
         call node__fw_exp(nd)
      case (op_log_id_)
         call node__fw_log(nd)
      case (op_sin_id_)
         call node__fw_sin(nd)
      case (op_cos_id_)
         call node__fw_cos(nd)
      case (op_tan_id_)
         call node__fw_tan(nd)
      case (op_sinh_id_)
         call node__fw_sinh(nd)
      case (op_cosh_id_)
         call node__fw_cosh(nd)
      case (op_tanh_id_)
         call node__fw_tanh(nd)
      case (op_dgemm1_id_)
         call node__fw_dgemm__1(nd)
      case (op_dgemm2_id_)
         call node__fw_dgemm__2(nd)
      case (op_dgemm3_id_)
         call node__fw_dgemm__3(nd)
      case (op_dgemm4_id_)
         call node__fw_dgemm__4(nd)
      case (op_dp_gemv1_id_)
         call node__fw_dp_gemv__1(nd)
      case (op_dp_gemv2_id_)
         call node__fw_dp_gemv__2(nd)
      case (op_dp_gemv3_id_)
         call node__fw_dp_gemv__3(nd)
      case (op_dp_gemv4_id_)
         call node__fw_dp_gemv__4(nd)
         ! case (op_dp_ger1_id_)
         !    call node__fw_dp_ger__1(nd)
         ! case (op_dp_ger2_id_)
         !    call node__fw_dp_ger__2(nd)
         ! case (op_dp_ger3_id_)
         !    call node__fw_dp_ger__3(nd)
         ! case (op_dp_dot_id_)
         !    call node__fw_dp_dot(nd)
      case (op_invMat_id_)
         call node__fw_invMat(nd)
         ! case (op_invSymMat_id_)
         !    call node__fw_invSymMat(nd)
      case (op_sum_id_)
         call node__fw_sum(nd)
      case (op_ssq_id_)
         call node__fw_ssq(nd)
      case (op_sigmoid_id_)
         call node__fw_sigmoid(nd)
      case (op_relu_id_)
         call node__fw_relu(nd)
      case (op_swish_id_)
         call node__fw_swish(nd)
      case (op_elu_id_)
         call node__fw_elu(nd)
      case (op_softmax1_id_)
         call node__fw_softmax__1(nd)
      case (op_softmax2_id_)
         call node__fw_softmax__2(nd)
      case (op_bin_entropy_id_)
         call node__fw_bin_entropy(nd)
      case (op_cross_entropy1_id_)
         call node__fw_cross_entropy(nd)
      ! case (op_cross_entropy2_id_)
      !    call node__fw_cross_entropy__2(nd)
      case (op_slice_id_)
         call node__fw_slice(nd)
      case (op_flat_slice_id_)
         call node__fw_flat_slice(nd)
      case (op_bind_id_)
         call node__fw_bind(nd)
      case (op_embeddings_id_)
         call node__fw_embeddings(nd)
      case (op_mse_id_)
         call node__fw_mse(nd)
      case (op_mae_id_)
         call node__fw_mae(nd)
      case (op_ldexp_id_)
         call node__fw_ldexp(nd)
      case (op_ldlaplace_id_)
         call node__fw_ldlaplace(nd)
      case (op_ldbeta_id_)
         call node__fw_ldbeta(nd)
      case (op_ldgamma_id_)
         call node__fw_ldgamma(nd)
      case (op_ldnorm_id_)
         call node__fw_ldnorm(nd)
      case (op_lkh_norm1_id_)
         call node__fw_lkh_norm__1(nd)
      case (op_lkh_norm2_id_)
         call node__fw_lkh_norm__2(nd)
      case ( &
           op_contiguous_slice_id_, &
           op_reshape_id_, &
           op_drop_shape_id_ &
           )
         ! pass
      case default
         call raise_error('nd%op', err_unknwnVal_)
      end select
    end subroutine private_fw
  end subroutine node__fw

  !> Applies bakward differentiation to a node depending on its operator.
  !! @author Filippo Monari
  !! @param[in] ndi integer, node index in the NODES_ array
  subroutine node__bw (ndi)
    implicit none
    integer, intent(in) :: ndi
    call do_within('node__bw', mod_node_operators_name_, private_bw)
  contains
    subroutine private_bw
      type(node), pointer :: nd
      nd => NODES_(ndi)
      select case (nd%op)
      case (op_add_id_)
         call node__bw_add(nd)
      case (op_sub_id_)
         call node__bw_sub(nd)
      case (op_mult_id_)
         call node__bw_mult(nd)
      case (op_pow_id_)
         call node__bw_pow(nd)
      case (op_div_id_)
         call node__bw_div(nd)
      case (op_abs_id_)
         call node__bw_abs(nd)
      case (op_exp_id_)
         call node__bw_exp(nd)
      case (op_log_id_)
         call node__bw_log(nd)
      case (op_sin_id_)
         call node__bw_sin(nd)
      case (op_cos_id_)
         call node__bw_cos(nd)
      case (op_tan_id_)
         call node__bw_tan(nd)
      case (op_sinh_id_)
         call node__bw_sinh(nd)
      case (op_cosh_id_)
         call node__bw_cosh(nd)
      case (op_tanh_id_)
         call node__bw_tanh(nd)
      case (op_dgemm1_id_)
         call node__bw_dgemm__1(nd)
      case (op_dgemm2_id_)
         call node__bw_dgemm__2(nd)
      case (op_dgemm3_id_)
         call node__bw_dgemm__3(nd)
      case (op_dgemm4_id_)
         call node__bw_dgemm__4(nd)
      case (op_dp_gemv1_id_)
         call node__bw_dp_gemv__1(nd)
      case (op_dp_gemv2_id_)
         call node__bw_dp_gemv__2(nd)
      case (op_dp_gemv3_id_)
         call node__bw_dp_gemv__3(nd)
      case (op_dp_gemv4_id_)
         call node__bw_dp_gemv__4(nd)
      case (op_dp_ger1_id_)
         call node__bw_dp_ger__1(nd)
      case (op_dp_ger2_id_)
         call node__bw_dp_ger__2(nd)
      case (op_dp_ger3_id_)
         call node__bw_dp_ger__3(nd)
      case (op_dp_dot_id_)
         call node__bw_dp_dot(nd)
      case (op_invMat_id_)
         call node__bw_invMat(nd)
      case (op_sum_id_)
         call node__bw_sum(nd)
      case (op_sum2_id_)
         call node__bw_sum__2(nd)
      case (op_product1_id_)
         call node__bw_product__1(nd)
      case (op_product2_id_)
         call node__bw_product__2(nd)
      case (op_ssq_id_)
         call node__bw_ssq(nd)
      case (op_sigmoid_id_)
         call node__bw_sigmoid(nd)
      case (op_relu_id_)
         call node__bw_relu(nd)
      case (op_swish_id_)
         call node__bw_swish(nd)
      case (op_elu_id_)
         call node__bw_elu(nd)
      case (op_softmax1_id_)
         call node__bw_softmax__1(nd)
      case (op_softmax2_id_)
         call node__bw_softmax__2(nd)
      case (op_bin_entropy_id_)
         call node__bw_bin_entropy(nd)
      case (op_cross_entropy1_id_)
         call node__bw_cross_entropy(nd)
      case (op_slice_id_)
         call node__bw_slice(nd)
      case (op_flat_slice_id_)
         call node__bw_flat_slice(nd)
      case (op_bind_id_)
         call node__bw_bind(nd)
      case (op_embeddings_id_)
         call node__bw_embeddings(nd)
      case (op_mse_id_)
         call node__bw_mse(nd)
      case (op_mae_id_)
         call node__bw_mae(nd)
      case (op_ldexp_id_)
         call node__bw_ldexp(nd)
      case (op_ldlaplace_id_)
         call node__bw_ldlaplace(nd)
      case (op_ldbeta_id_)
         call node__bw_ldbeta(nd)
      case (op_ldgamma_id_)
         call node__bw_ldgamma(nd)
      case (op_ldnorm_id_)
         call node__bw_ldnorm(nd)
      case (op_ldmvnorm1_id_)
         call node__bw_ldmvnorm__1(nd)
      case (op_lkh_norm1_id_)
         call node__bw_lkh_norm__1(nd)
      case (op_lkh_norm2_id_)
         call node__bw_lkh_norm__2(nd)
      case (op_ksqexp_id_)
         call node__bw_ksqexp(nd)
      case ( &
           op_contiguous_slice_id_, &
           op_reshape_id_, &
           op_drop_shape_id_ &
           )
         ! pass
      case default
         call raise_error('nd%typ', err_unknwnVal_)
      end select
    end subroutine private_bw
  end subroutine node__bw

  !> Reset to 0 the derivative of a inputs in order to prepare a
  !! new bakward differentiation operation.
  !! @author Filippo Monari
  !! @param[in] ndi integer, node index in the NODES_ array
  subroutine node__bw_zero (ndi)
    implicit none
    integer, intent(in) :: ndi
    type(node), pointer :: nd
    integer :: i, ii
    nd => NODES_(ndi)
    do i = size(nd%in), 1, -1
       ii = nd%in(i)
       NUMBERS_(ii)%dv = 0
    end do
  end subroutine node__bw_zero

  !> @defgroup node_operators_modifiers_ Modifiers
  !! Slicing, reshaping, binding
  !! @{

  
  !> @defgroup node_op_modifiers_genericslice_ Generic Slice
  !! @{
  
  !> Slice - operator
  !! @param[in] nd 'node'
  subroutine node__op_slice (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate(x1 => NUMBERS_(nd%in(1)), x2 => NUMBERS_(nd%out))
      call op_slice(x1, x2, nd%flg)
    end associate
  end subroutine node__op_slice

  !> Slice - forward differentiation 
  !! @param[in] nd 'node'
  subroutine node__fw_slice (nd)
    implicit none
    type(node), target :: nd
    associate(x1 => NUMBERS_(nd%in(1)), x2 => NUMBERS_(nd%out))
      call fw_slice(x1, x2, nd%flg)
    end associate
  end subroutine node__fw_slice

  !> Slice - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_slice (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate(x1 => NUMBERS_(nd%in(1)), x2 => NUMBERS_(nd%out))
      call bw_slice(x1, x2, nd%flg)
    end associate
  end subroutine node__bw_slice
  !> @}

  !> @defgroup node_operators_modifiers_flatslice_ Flat slice
  !! @{

  !> Flat Slice - operator
  !! @param[in] nd 'node'
  subroutine node__op_flat_slice (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate(x1 => NUMBERS_(nd%in(1)), x2 => NUMBERS_(nd%out))
      call op_flat_slice(x1, x2, nd%flg)
    end associate
  end subroutine node__op_flat_slice

  !> Flat Slice - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_flat_slice (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate(x1 => NUMBERS_(nd%in(1)), x2 => NUMBERS_(nd%out))
      call fw_slice(x1, x2, nd%flg)
    end associate
  end subroutine node__fw_flat_slice

  !> Flat Slice - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_flat_slice (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate(x1 => NUMBERS_(nd%in(1)), x2 => NUMBERS_(nd%out))
      call bw_slice(x1, x2, nd%flg)
    end associate
  end subroutine node__bw_flat_slice
  !> @}

  !> @defgroup node_operators_modifiers_bind_ Bind
  !! @{
  
  !> Bind - operator
  !! @param[in] nd 'node'
  subroutine node__op_bind (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call op_bind(x1, x2, x3, nd%flg(1))
    end associate
  end subroutine node__op_bind

  !> Bind - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_bind (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => nd%in(1), &
         x2 => nd%in(2), &
         x3 => nd%out &
      )
      call fw_bind(NUMBERS_(x1), NUMBERS_(x2), NUMBERS_(x3), nd%flg(1))
    end associate
  end subroutine node__fw_bind

  !> Bind - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_bind (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => nd%in(1), &
         x2 => nd%in(2), &
         x3 => nd%out &
         )
      call bw_bind(NUMBERS_(x1), NUMBERS_(x2), NUMBERS_(x3), nd%flg(1))
    end associate
  end subroutine node__bw_bind
  !> @}

  !> @defgroup node_operators_modifiers_embeddings_ Bind
  !! @{

  !> Embeddings - operator
  !! @param[in] nd 'node'
  subroutine node__op_embeddings (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         f => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         e => NUMBERS_(nd%out) &
         )
      call op_embeddings(f, x, e, nd%flg(1))
    end associate
  end subroutine node__op_embeddings

  !> Embeddings - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_embeddings (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         f => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         e => NUMBERS_(nd%out) &
         )
      call fw_embeddings(f, x, e, nd%flg(1))
    end associate
  end subroutine node__fw_embeddings

  !> Embeddings - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_embeddings (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         f => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         e => NUMBERS_(nd%out) &
         )
      call bw_embeddings(f, x, e, nd%flg(1))
    end associate
  end subroutine node__bw_embeddings
  !> @}
  !> @}

  !> @defgroup node_operators_unary_ Unary Operators
  !! @{
  
  !> @defgroup node_operators_unary_abs_ Abs
  !! @{
  
  !> Abs - operator
  !! @param[in] nd 'node'
  subroutine node__op_abs (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_abs(x1, x2)
    end associate
  end subroutine node__op_abs

  !> Abs - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_abs (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_abs(x1, x2)
    end associate
  end subroutine node__fw_abs

  !> Abs - backward differenatiation
  !! @param[in] nd 'node'
  subroutine node__bw_abs (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_abs(x1, x2)
    end associate
  end subroutine node__bw_abs
  !> @}

  !> @defgroup node_operators_unary_exp_ Exponential
  !! @{
  
  !> Exponential - operator
  !! @param[in] nd 'node'
  subroutine node__op_exp (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_exp(x1, x2)
    end associate
  end subroutine node__op_exp

  !> Exponential - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_exp (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_exp(x1, x2)
    end associate
  end subroutine node__fw_exp

  !> Exponential - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_exp (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_exp(x1, x2)
    end associate
  end subroutine node__bw_exp
  !> @}

  !> @defgroup node_operators_unary_log_ Logarithm
  !! @{
  
  !> Logarithm - operator
  !! @param[in] nd 'node'
  subroutine node__op_log (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_log(x1, x2)
    end associate
  end subroutine node__op_log

  !> Logarithm - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_log (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_log(x1, x2)
    end associate
  end subroutine node__fw_log

  !> Logarithm - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_log (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_log(x1, x2)
    end associate
  end subroutine node__bw_log
  !> @}

  !> @defgroup node_operators_unary_sine_ Sine
  !! @{
  
  !> Sine - operator
  !! @param[in] nd 'node'
  subroutine node__op_sin (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_sin(x1, x2)
    end associate
  end subroutine node__op_sin

  !> Sine - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_sin (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_sin(x1, x2)
    end associate
  end subroutine node__fw_sin

  !> Sine - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_sin (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_sin(x1, x2)
    end associate
  end subroutine node__bw_sin
  !> @}

  !> @defgroup node_operators_unary_cosine_ cosine
  !! @{
  
  !> Cosine - operator
  !! @param[in] nd 'node'
  subroutine node__op_cos (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_cos(x1, x2)
    end associate
  end subroutine node__op_cos

  !> Cosine - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_cos (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_cos(x1, x2)
    end associate
  end subroutine node__fw_cos

  !> Cosine backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_cos (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_cos(x1, x2)
    end associate
  end subroutine node__bw_cos
  !> @}

  !> @defgroup node_operators_unary_tan_ Tangent
  !! @{
  
  !> Tangent - operator
  !! @param[in] nd 'node'
  subroutine node__op_tan (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_tan(x1, x2)
    end associate
  end subroutine node__op_tan

  !> Tangent - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_tan (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_tan(x1, x2)
    end associate
  end subroutine node__fw_tan

  !> Tangent - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_tan (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_tan(x1, x2)
    end associate
  end subroutine node__bw_tan
  !> @}

  !> @defgroup node_op_unary_sinh_ Hyperbolic Sine
  !! @{
  
  !> Hyperbolic Sine -  operator
  !! @param[in] nd 'node'
  subroutine node__op_sinh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_sinh(x1, x2)
    end associate
  end subroutine node__op_sinh

  !> Hyperbolic Sine - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_sinh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_sinh(x1, x2)
    end associate
  end subroutine node__fw_sinh

  !> Hyperbolic Sine - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_sinh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_sinh(x1, x2)
    end associate
  end subroutine node__bw_sinh
  !> @}

  !> @defgroup node_op_unary_cosh_ Hyperbolic Cosine
  !! @{
  
  !> Hyperbolic Cosine - operator
  !! @param[in] nd 'node'
  subroutine node__op_cosh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out)&
         )
      call op_cosh(x1, x2)
    end associate
  end subroutine node__op_cosh

  !> Hyperbolic Cosine - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_cosh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_cosh(x1, x2)
    end associate
  end subroutine node__fw_cosh

  !> Hyperbolic Cosine - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_cosh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_cosh(x1, x2)
    end associate
  end subroutine node__bw_cosh
  !> @}

  !> @defgroup node_op_unary_tanh_ Hyperbolic Tangent
  !! @{
  
  !> Hyperbolic Tangent - operator
  !! @param[in] nd 'node'
  subroutine node__op_tanh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_tanh(x1, x2)
    end associate
  end subroutine node__op_tanh

  !> Hyperbolic Tangent - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_tanh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_tanh(x1, x2)
    end associate
  end subroutine node__fw_tanh

  !> Hyperbolic Tangent - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_tanh (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_tanh(x1, x2)
    end associate
  end subroutine node__bw_tanh
  !> @}

  !> @defgroup node_op_unary_sigmoid_ Sigmoid
  !! @{

  !> Sigmoid - operator
  !! @param[in] nd 'node'
  subroutine node__op_sigmoid (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_sigmoid(x1, x2)
    end associate
  end subroutine node__op_sigmoid

  !> Sigmoid - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_sigmoid (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_sigmoid(x1, x2)
    end associate
  end subroutine node__fw_sigmoid

  !> Sigmoid - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_sigmoid (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_sigmoid(x1, x2)
    end associate
  end subroutine node__bw_sigmoid
  !> @}

  !> @defgroup node_op_unary_relu_ ReLU
  !! @{

  !> ReLU - operator
  !! @param[in] nd 'node'
  subroutine node__op_relu (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_relu(x1, x2)
    end associate
  end subroutine node__op_relu

  !> ReLU - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_relu (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_relu(x1, x2)
    end associate
  end subroutine node__fw_relu

  !> ReLU - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_relu (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_relu(x1, x2)
    end associate
  end subroutine node__bw_relu
  !> @}

  !> @defgroup node_op_unary_swish_ Swish
  !! @{

  !> Swish - operator
  !! @param[in] nd 'node'
  subroutine node__op_swish (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_swish(x1, x2)
    end associate
  end subroutine node__op_swish

  !> Swish - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_swish (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_swish(x1, x2)
    end associate
  end subroutine node__fw_swish

  !> Swish - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_swish (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_swish(x1, x2)
    end associate
  end subroutine node__bw_swish
  !> @}

  !> @defgroup node_op_unary_softmax1_ Standard Softmax
  !! Calculates the softmax of all the elements of the number (rank > 0)
  !! indempendently from the specific dimension
  !! @{
  
  !> Standard softmax - operator
  !! @param[in] nd 'node'
  subroutine node__op_softmax__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_softmax(x1, x2)
    end associate
  end subroutine node__op_softmax__1

  !> Standard softmax - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_softmax__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_softmax(x1, x2)
    end associate
  end subroutine node__fw_softmax__1

  !> Standard softmax - differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_softmax__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_softmax(x1, x2)
    end associate
  end subroutine node__bw_softmax__1
  !> @}

  !> @defgroup node_op_unary_softmax2_ Softmax by Dimension
  !! @{

  !> Softmax by dimension - operator
  !! @param[in] nd 'node'
  subroutine node__op_softmax__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_softmax(x1, x2, nd%flg(1))
    end associate
  end subroutine node__op_softmax__2

  !> Softmax by dimension - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_softmax__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_softmax(x1, x2, nd%flg(1))
    end associate
  end subroutine node__fw_softmax__2

  !> Sofmax by dimension - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_softmax__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_softmax(x1, x2, nd%flg(1))
    end associate
  end subroutine node__bw_softmax__2
  !> @}
  !> @}


  !> @defgroup nod_operators_binary_ Binary Operators
  !! All operators taking a 'node' with 2 inputs
  !! @{

  !> @defgroup node_operators_binary_addtion_ Addition
  !! @{
  
  !> Addition - operator
  !! @param[in] nd 'node
  subroutine node__op_add (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call op_add(x1, x2, x3)
    end associate
  end subroutine node__op_add

  !> Addition - forward differentiation
  !! @param[in] nd 'node
  subroutine node__fw_add (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call fw_add(x1, x2, x3)
    end associate
  end subroutine node__fw_add

  !> Addition - backward differentiation
  !! @param[in] nd 'node
  subroutine node__bw_add (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call bw_add(x1, x2, x3)
    end associate
  end subroutine node__bw_add
  !> @}

  !> @defgroup node_operators_binary_subtraction_ Subtraction
  !! @{
  
  !> Subtraction - operator
  !! @param[in] nd 'node'
  subroutine node__op_sub (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call op_sub(x1, x2, x3)
    end associate
  end subroutine node__op_sub

  !> Subtraction - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_sub (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call fw_sub(x1, x2, x3)
    end associate
  end subroutine node__fw_sub

  !> Subtraction - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_sub (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call bw_sub(x1, x2, x3)
    end associate
  end subroutine node__bw_sub
  !> @}

  !> @defgroup node_operators_binary_multiplication_ Multiplication
  !! @{
  
  !> Multipliction - operator
  !! @param[in] nd 'node'
  subroutine node__op_mult (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call op_mult(x1, x2, x3)
    end associate
  end subroutine node__op_mult

  !> Multiplication - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_mult (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call fw_mult(x1, x2, x3)
    end associate
  end subroutine node__fw_mult

  !> Multiplication - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_mult (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call bw_mult(x1, x2, x3)
    end associate
  end subroutine node__bw_mult
  !> @}

  !> @defgroup node_operators_binary_pow_ Power
  !! @{
  
  !> Power - operator
  !! @param[in] nd 'node'
  subroutine node__op_pow (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call op_pow(x1, x2, x3)
    end associate
  end subroutine node__op_pow

  !> Power - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_pow (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call fw_pow(x1, x2, x3)
    end associate
  end subroutine node__fw_pow

  !> Power - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_pow (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call bw_pow(x1, x2, x3)
    end associate
  end subroutine node__bw_pow
  !> @}

  !> @defgroup node_operators_binary_division_ Division
  !! @{
  
  !> Division - operator
  !! @param[in] nd 'node'
  subroutine node__op_div (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call op_div(x1, x2, x3)
    end associate
  end subroutine node__op_div

  !> Division - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_div (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call fw_div(x1, x2, x3)
    end associate
  end subroutine node__fw_div

  !> Division - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_div (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         x3 => NUMBERS_(nd%out) &
         )
      call bw_div(x1, x2, x3)
    end associate
  end subroutine node__bw_div
  !> @}

  !> @defgroup node_operators_binary_elu_ ELU
  !! @{
  
  !> ELU - operator
  !! @param[in] nd 'node'
  subroutine node__op_elu (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         a => NUMBERS_(nd%in(2)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_elu(x1, x2, a)
    end associate
  end subroutine node__op_elu

  !> ELU - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_elu (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         a => NUMBERS_(nd%in(2)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_elu(x1, x2, a)
    end associate
  end subroutine node__fw_elu

  !> ELU - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_elu (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         a => NUMBERS_(nd%in(2)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_elu(x1, x2, a)
    end associate
  end subroutine node__bw_elu
  !>@}

  !> @defgroup node_operators_binary_bin_entropy_ Binary Entropy
  !! @{
  
  !> Binary entropy - operator
  !! @param[in] nd 'node'
  subroutine node__op_bin_entropy (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call op_bin_entropy(y, yh, j)
    end associate
  end subroutine node__op_bin_entropy

  !> Binary entripy - forward difference
  !! @param[in] nd 'node'
  subroutine node__fw_bin_entropy (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call fw_bin_entropy(y, yh, j)
    end associate
  end subroutine node__fw_bin_entropy

  !> Binary entropy - backward difference
  !! @param[in] nd 'node'
  subroutine node__bw_bin_entropy (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call bw_bin_entropy(y, yh, j)
    end associate
  end subroutine node__bw_bin_entropy
  !> @}

  !> @defgroup node_operators_binary_cross_entropy1_ Standard Cross-Entropy
  !! Calculates the cross-entropy idependently from the particular dimension.
  !! @{
  
  !> Standard cross-entropy - operator
  !! @param[in] nd 'node'
  subroutine node__op_cross_entropy (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call op_cross_entropy(y, yh, j)
    end associate
  end subroutine node__op_cross_entropy

  !> Standard cross-entropy - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_cross_entropy (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call fw_cross_entropy(y, yh, j)
    end associate
  end subroutine node__fw_cross_entropy

  !> Standard cross-entropy - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_cross_entropy (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call bw_cross_entropy(y, yh, j)
    end associate
  end subroutine node__bw_cross_entropy
  !> @}

  !> @defgroup node_operators_binary_mse_ MeanSquared Error (MSE)
  !! @{

  !> MSE - operator
  !! @param[in] nd 'node'
  subroutine node__op_mse (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call op_mse(y, yh, j)
    end associate
  end subroutine node__op_mse

  !> MSE - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_mse (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call fw_mse(y, yh, j)
    end associate
  end subroutine node__fw_mse

  !> MSE - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_mse (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call bw_mse(y, yh, j)
    end associate
  end subroutine node__bw_mse
  !> @}

  !> @defgroup node_operators_mae_ Mean Absolute Error (MAE)
  !! @{

  !> MAE - operator
  !! @param[in] nd 'node'
  subroutine node__op_mae (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call op_mae(y, yh, j)
    end associate
  end subroutine node__op_mae

  !> MAE - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_mae (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call fw_mae(y, yh, j)
    end associate
  end subroutine node__fw_mae

  !> MAE - bakward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_mae (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         yh => NUMBERS_(nd%in(2)), &
         j => NUMBERS_(nd%out) &
         )
      call bw_mae(y, yh, j)
    end associate
  end subroutine node__bw_mae
  !> @}
  !> @}

  !> @defgroup node_operators_gemm_ General Matrix Multiplication
  !! @todo change name to dp_gemm
  !! @{

  !> @defgroup node_operators_gemm_gemm1_ Gemm 1
  !! alpha * op(A) . op(B) + beta * C
  !! @{
  
  !> Gemm 1 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dgemm__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         B => NUMBERS_(nd%in(3)), &
         beta => NUMBERS_(nd%in(4)), &
         C => NUMBERS_(nd%in(5)), &
         CC => NUMBERS_(nd%out) &
         )
      call op_dgemm(nd%flg(1), nd%flg(2), alpha, A, B, beta, C, CC)
    end associate
  end subroutine node__op_dgemm__1

  !> Gemm 1 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dgemm__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         B => NUMBERS_(nd%in(3)), &
         beta => NUMBERS_(nd%in(4)), &
         C => NUMBERS_(nd%in(5)), &
         CC => NUMBERS_(nd%out) &
         )
      call fw_dgemm(nd%flg(1), nd%flg(2), alpha, A, B, beta, C, CC)
    end associate
  end subroutine node__fw_dgemm__1

  !> Gemm 1 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dgemm__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         B => NUMBERS_(nd%in(3)), &
         beta => NUMBERS_(nd%in(4)), &
         C => NUMBERS_(nd%in(5)), &
         CC => NUMBERS_(nd%out) &
         )
      call bw_dgemm(nd%flg(1), nd%flg(2), alpha, A, B, beta, C, CC)
    end associate
  end subroutine node__bw_dgemm__1
  !> @}

  !> @defgroup node_operators_gemm_gemm2_ Gemm 2
  !! alpha * op(A) . op(B)
  !! @{
  
  !> Gemm 2 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dgemm__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         B => NUMBERS_(nd%in(3)), &
         C => NUMBERS_(nd%out)&
         )
      call op_dgemm(alpha, nd%flg(1), nd%flg(2), A, B, C)
    end associate
  end subroutine node__op_dgemm__2

  !> Gemm 2 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dgemm__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         B => NUMBERS_(nd%in(3)), &
         C => NUMBERS_(nd%out)&
         )
      call fw_dgemm(alpha, nd%flg(1), nd%flg(2), A, B, C)
    end associate
  end subroutine node__fw_dgemm__2

  !> Gemm 2 - backaward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dgemm__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         B => NUMBERS_(nd%in(3)), &
         C => NUMBERS_(nd%out)&
         )
      call bw_dgemm(alpha, nd%flg(1), nd%flg(2), A, B, C)
    end associate
  end subroutine node__bw_dgemm__2
  !> @}

  !> @defgroup node_operators_gemm_gemm3_ Gemm 3
  !! op(A) . op(B) + C
  !! @{
  
  !> Gemm 3 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dgemm__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
      A => NUMBERS_(nd%in(1)), &
      B => NUMBERS_(nd%in(2)), &
      C => NUMBERS_(nd%in(3)), &
      CC => NUMBERS_(nd%out) &
      )
      call op_dgemm(nd%flg(1), nd%flg(2), A, B, C, CC)
    end associate
  end subroutine node__op_dgemm__3

  !> Gemm 3 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dgemm__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
      A => NUMBERS_(nd%in(1)), &
      B => NUMBERS_(nd%in(2)), &
      C => NUMBERS_(nd%in(3)), &
      CC => NUMBERS_(nd%out) &
      )
      call fw_dgemm(nd%flg(1), nd%flg(2), A, B, C, CC)
    end associate
  end subroutine node__fw_dgemm__3

  !> Gemm 3 - backaward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dgemm__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         A => NUMBERS_(nd%in(1)), &
         B => NUMBERS_(nd%in(2)), &
         C => NUMBERS_(nd%in(3)), &
         CC => NUMBERS_(nd%out) &
         )
      call bw_dgemm(nd%flg(1), nd%flg(2), A, B, C, CC)
    end associate
  end subroutine node__bw_dgemm__3
  !> @}

  !> @defgroup node_operators_gemm_gemm4_ Gemm 4
  !! op(A) . op(B)
  !! @{
  
  !> Gemm 4 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dgemm__4 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         A => NUMBERS_(nd%in(1)), &
         B => NUMBERS_(nd%in(2)), &
         C => NUMBERS_(nd%out) &
         )
      call op_dgemm(nd%flg(1), nd%flg(2), A, B, C)
    end associate
  end subroutine node__op_dgemm__4

  !> Gemm 4 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dgemm__4 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         A => NUMBERS_(nd%in(1)), &
         B => NUMBERS_(nd%in(2)), &
         C => NUMBERS_(nd%out) &
         )
      call fw_dgemm(nd%flg(1), nd%flg(2), A, B, C)
    end associate
  end subroutine node__fw_dgemm__4

  !> Gemm 4 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dgemm__4 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         A => NUMBERS_(nd%in(1)), &
         B => NUMBERS_(nd%in(2)), &
         C => NUMBERS_(nd%out) &
         )
      call bw_dgemm(nd%flg(1), nd%flg(2), A, B, C)
    end associate
  end subroutine node__bw_dgemm__4
  !> @}
  !> @}

  !> @defgroup node_operators_gemv_ General Matrix-Vector Multiplication
  !! @{
  
  !> @defgroup node_operators_gemv_gemv1_ Gemv 1
  !! alpha * op(A) . x + beta * y
  !! @{
  
  !> Gemv 1 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_gemv__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%in(3)), &
         beta => NUMBERS_(nd%in(4)), &
         y => NUMBERS_(nd%in(5)), &
         yy => NUMBERS_(nd%out) &
         )
      call op_dp_gemv(nd%flg(1), alpha, x, A, beta, y, yy)
    end associate
  end subroutine node__op_dp_gemv__1

  !> Gemv 1 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dp_gemv__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%in(3)), &
         beta => NUMBERS_(nd%in(4)), &
         y => NUMBERS_(nd%in(5)), &
         yy => NUMBERS_(nd%out) &
         )
      call fw_dp_gemv(nd%flg(1), alpha, x, A, beta, y, yy)
    end associate
  end subroutine node__fw_dp_gemv__1

  !> Gemv 1 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_gemv__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%in(3)), &
         beta => NUMBERS_(nd%in(4)), &
         y => NUMBERS_(nd%in(5)), &
         yy => NUMBERS_(nd%out) &
         )
      call bw_dp_gemv(nd%flg(1), alpha, x, A, beta, y, yy)
    end associate
  end subroutine node__bw_dp_gemv__1
  !> @}

  !> @defgroup node_operators_gemv_gemv2_ Gemv 2
  !! alpha * op(A) . x + y
  !! @{
  
  !> Gemv 2 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_gemv__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%in(3)), &
         y => NUMBERS_(nd%out) &
         )
      call op_dp_gemv(alpha, nd%flg(1), x, A, y)
    end associate
  end subroutine node__op_dp_gemv__2

  !> Gemv 2 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dp_gemv__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%in(3)), &
         y => NUMBERS_(nd%out) &
         )
      call fw_dp_gemv(alpha, nd%flg(1), x, A, y)
    end associate
  end subroutine node__fw_dp_gemv__2

  !> Gemv 2 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_gemv__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%in(3)), &
         y => NUMBERS_(nd%out) &
         )
      call bw_dp_gemv(alpha, nd%flg(1), x, A, y)
    end associate
  end subroutine node__bw_dp_gemv__2
  !> @}
  
  !> @defgroup node_operators_gemv_gemv3_ Gemv 3
  !! op(A) . x + y
  !! @{
  
  !> Gemv 3 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_gemv__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%in(3)), &
         yy => NUMBERS_(nd%out) &
         )
      call op_dp_gemv(nd%flg(1), x, A, y, yy)
    end associate
  end subroutine node__op_dp_gemv__3

  !> Gemv 3 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dp_gemv__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%in(3)), &
         yy => NUMBERS_(nd%out) &
         )
      call fw_dp_gemv(nd%flg(1), x, A, y, yy)
    end associate
  end subroutine node__fw_dp_gemv__3

  !> Gemv 3 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_gemv__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%in(3)), &
         yy => NUMBERS_(nd%out) &
         )
      call bw_dp_gemv(nd%flg(1), x, A, y, yy)
    end associate
  end subroutine node__bw_dp_gemv__3
  !> @}

  !> @defgroup node_operators_gemv_gemv4_ Gemv 4
  !! alpha * op(A) . x
  !! @{

  !> Gemv 4 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_gemv__4 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%out) &
         )
      call op_dp_gemv(nd%flg(1), x, A, y)
    end associate
  end subroutine node__op_dp_gemv__4

  !> Gemv 4 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_dp_gemv__4 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%out) &
         )
      call fw_dp_gemv(nd%flg(1), x, A, y)
    end associate
  end subroutine node__fw_dp_gemv__4

  !> Gemv 4 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_gemv__4 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         A => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%out) &
         )
      call bw_dp_gemv(nd%flg(1), x, A, y)
    end associate
  end subroutine node__bw_dp_gemv__4
  !> @}
  !> @}

  !> @defgroup node_operators_ger_ General outer product between vectors (Ger)
  !! @todo implement fw
  !! @{
  
  !> @defgroup node_operators_ger_ger1_ Ger 1
  !! alpha * x . y**T + A
  !! @{
  
  !> Ger 1 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_ger__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%in(3)), &
         z => NUMBERS_(nd%in(4)), &
         A => NUMBERS_(nd%out) &
         )
      call op_dp_ger(alpha, x, y, z, A)
    end associate
  end subroutine node__op_dp_ger__1

  !> Ger 1 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_ger__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         alpha => NUMBERS_(nd%in(1)), &
         x => NUMBERS_(nd%in(2)), &
         y => NUMBERS_(nd%in(3)), &
         z => NUMBERS_(nd%in(4)), &
         A => NUMBERS_(nd%out) &
         )
      call bw_dp_ger(alpha, x, y, z, A)
    end associate
  end subroutine node__bw_dp_ger__1
  !> @}

  !> @defgroup node_operators_ger_ger2_ Ger 2
  !! x . y**T + A
  !! @{
  
  !> Ger 2 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_ger__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         y => NUMBERS_(nd%in(2)), &
         z => NUMBERS_(nd%in(3)), &
         A => NUMBERS_(nd%out) &
         )
      call bw_dp_ger(x, y, z, A)
    end associate
  end subroutine node__op_dp_ger__2

  !> Ger 2 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_ger__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         y => NUMBERS_(nd%in(2)), &
         z => NUMBERS_(nd%in(3)), &
         A => NUMBERS_(nd%out) &
         )
      call bw_dp_ger(x, y, z, A)
    end associate
  end subroutine node__bw_dp_ger__2
  !> @}

  !> @defgroup node_operators_ger_ger3_ Ger 3
  !! x . y**T 
  !! @{
  
  !> Ger 3 - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_ger__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         y => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%out) &
         )
      call bw_dp_ger(x, y, A)
    end associate
  end subroutine node__op_dp_ger__3

  !> Ger 3 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_ger__3 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         y => NUMBERS_(nd%in(2)), &
         A => NUMBERS_(nd%out) &
         )
      call bw_dp_ger(x, y, A)
    end associate
  end subroutine node__bw_dp_ger__3
  !> @}
  !> @}

  !> @defgroup node_operators_dot_ Inner product between vectors (dot)
  !! x**T . y
  !! @{
  
  !> Dot - operator
  !! @param[in] nd 'node'
  subroutine node__op_dp_dot (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         y => NUMBERS_(nd%in(2)), &
         z => NUMBERS_(nd%out) &
         )
      call op_dp_dot(x, y, z)
    end associate
  end subroutine node__op_dp_dot

  !> Dot - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_dp_dot (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x => NUMBERS_(nd%in(1)), &
         y => NUMBERS_(nd%in(2)), &
         z => NUMBERS_(nd%out) &
         )
      call bw_dp_dot(x, y, z)
    end associate
  end subroutine node__bw_dp_dot
  !> @}

  !> @defgroup node_operators_inv_mat_ Matrix inversion
  !! @{
  
  !> @defgroup node_operators_inv_mat_invmat_ Gemeral Matrix Inversion
  !! @{
  
  !> General matrix inversion - operator
  !! @param[in] nd 'node'
  subroutine node__op_invMat (nd)
    implicit none
    type(node), intent(in), target :: nd
    call do_within('node__op_invMat', mod_node_operators_name_, private_op)
  contains
    subroutine private_op
      associate( &
           A => NUMBERS_(nd%in(1)), &
           B => NUMBERS_(nd%out) &
           )
        call op_invMat(A, B)
      end associate
    end subroutine private_op
  end subroutine node__op_invMat

  !> General matrix inversion - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_invMat (nd)
    implicit none
    type(node), intent(in), target :: nd
    call do_within('node__op_invMat', mod_node_operators_name_, private_fw)
  contains
    subroutine private_fw
      associate( &
           A => NUMBERS_(nd%in(1)), &
           B => NUMBERS_(nd%out) &
           )
        call fw_invMat(A, B)
      end associate
    end subroutine private_fw
  end subroutine node__fw_invMat

  !> General matrix inversion - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_invMat (nd)
    implicit none
    type(node), intent(in), target :: nd
    call do_within('node__op_degemm0', mod_node_operators_name_, private_bw)
  contains
    subroutine private_bw
      associate( &
           A => NUMBERS_(nd%in(1)), &
           B => NUMBERS_(nd%out) &
           )
        call bw_invMat(A, B)
      end associate
    end subroutine private_bw
  end subroutine node__bw_invMat
  !> @}

  !> @defgroup node_operators_reductions_ Reduction Operators
  !! @{
  
  !> @defgroup node_operators_reductions_sum_ Sum
  !! @{
  
  !> Sum - operator
  !! @param[in] nd 'node'
  subroutine node__op_sum (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_sum(x1, x2)
    end associate
  end subroutine node__op_sum

  !> Sum - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_sum (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_sum(x1, x2)
    end associate
  end subroutine node__fw_sum

  !> Sum - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_sum (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_sum(x1, x2)
    end associate
  end subroutine node__bw_sum
  !> @}

  subroutine node__op_sum__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
      )
      call op_sum(x1, x2, nd%flg(1))
    end associate
  end subroutine node__op_sum__2

  subroutine node__bw_sum__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_sum(x1, x2, nd%flg(1))
    end associate
  end subroutine node__bw_sum__2

  subroutine node__op_product__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_product(x1, x2)
    end associate
  end subroutine node__op_product__1

  ! subroutine node__fw_product__1 (nd)
  !   implicit none
  !   type(node), intent(in), target :: nd
  !   associate( &
  !        x1 => NUMBERS_(nd%in(1)), &
  !        x2 => NUMBERS_(nd%out) &
  !        )
  !     call fw_product(x1, x2)
  !   end associate
  ! end subroutine node__fw_product__1

  subroutine node__bw_product__1 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_product(x1, x2)
    end associate
  end subroutine node__bw_product__1
  
  subroutine node__op_product__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
      )
      call op_product(x1, x2, nd%flg(1))
    end associate
  end subroutine node__op_product__2

  subroutine node__bw_product__2 (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_product(x1, x2, nd%flg(1))
    end associate
  end subroutine node__bw_product__2
  
  !> @defgroup node_operators_reductions_ssq_ Sum of Squares
  !! @{
  
  !> Sum of squares - operator
  !! @param[in] nd 'node'
  subroutine node__op_ssq (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call op_ssq(x1, x2)
    end associate
  end subroutine node__op_ssq

  !> Sum of squares - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_ssq (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call fw_ssq(x1, x2)
    end associate
  end subroutine node__fw_ssq

  !> Sum of squares - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_ssq (nd)
    implicit none
    type(node), intent(in), target :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%out) &
         )
      call bw_ssq(x1, x2)
    end associate
  end subroutine node__bw_ssq
  !> @}
  !> @}

  !> @defgroup node_operators_stats Statistics
  !! @{

  !> @defgroup node_operators_stats_ldexp ldexp
  !! Exponential Distribution - log-density
  !! @{

  !> ldexp - operator
  !! @param[in] nd 'node'
  subroutine node__op_ldexp (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         lam => NUMBERS_(nd%in(2)), &
         ld => NUMBERS_(nd%out) &
         )
      call op_ldexp(y, lam, ld)
    end associate
  end subroutine node__op_ldexp

  !> ldexp - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_ldexp (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         lam => NUMBERS_(nd%in(2)), &
         ld => NUMBERS_(nd%out) &
         )
      call fw_ldexp(y, lam, ld)
    end associate
  end subroutine node__fw_ldexp

  !> ldexp - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_ldexp (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         lam => NUMBERS_(nd%in(2)), &
         ld => NUMBERS_(nd%out) &
         )
      call bw_ldexp(y, lam, ld)
    end associate
  end subroutine node__bw_ldexp
  !> @}

  !> @defgroup node_operators_stats_ldlaplace ldlaplace
  !! Laplace Distribution - log-density
  !! @{

  !> ldlaplace - operator
  !! @param[in] nd 'node'
  subroutine node__op_ldlaplace (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         lam => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call op_ldlaplace(y, mu, lam, ld)
    end associate
  end subroutine node__op_ldlaplace

  !> ldlaplace - forward differentiation
  !! @param[in] nd 'node
  subroutine node__fw_ldlaplace (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         lam => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call fw_ldlaplace(y, mu, lam, ld)
    end associate
  end subroutine node__fw_ldlaplace

  !> ldlaplace - backward differentiation
  !! @param[in] nd 'node
  subroutine node__bw_ldlaplace (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         lam => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call bw_ldlaplace(y, mu, lam, ld)
    end associate
  end subroutine node__bw_ldlaplace
  !> @}

  !> @defgroup node_operators_stats_ldbeta ldbeta
  !! Beta Distribution - log-density
  !! @{

  !> ldbeta - operator
  !! @param[in] nd 'node'
  subroutine node__op_ldbeta (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         a1 => NUMBERS_(nd%in(2)), &
         a2 => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call op_ldbeta(y, a1, a2, ld)
    end associate
  end subroutine node__op_ldbeta

  !> ldbeta - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_ldbeta (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         a1 => NUMBERS_(nd%in(2)), &
         a2 => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call fw_ldbeta(y, a1, a2, ld)
    end associate
  end subroutine node__fw_ldbeta

  !> ldbeta - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_ldbeta (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         a1 => NUMBERS_(nd%in(2)), &
         a2 => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call bw_ldbeta(y, a1, a2, ld)
    end associate
  end subroutine node__bw_ldbeta
  !> @}

  !> @defgroup node_op_stats_ldgamma ldgamma
  !! Gamma Distribution - log-density
  !! @{

  !> ldgamma - operator
  !! @param[in] nd 'node'
  subroutine node__op_ldgamma (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         a => NUMBERS_(nd%in(2)), &
         b => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call op_ldgamma(y, a, b, ld)
    end associate
  end subroutine node__op_ldgamma

  !> ldgamma - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_ldgamma (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         a => NUMBERS_(nd%in(2)), &
         b => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call fw_ldgamma(y, a, b, ld)
    end associate
  end subroutine node__fw_ldgamma

  !> ldgamma - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_ldgamma (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         a => NUMBERS_(nd%in(2)), &
         b => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call bw_ldgamma(y, a, b, ld)
    end associate
  end subroutine node__bw_ldgamma
  !> @}

  !> @defgroup node_operators_stats_ldnorm ldnorm
  !! Normal Distribution - log-density
  !! @{

  !> ldnorm - operator
  !! @param[in] nd 'node'
  subroutine node__op_ldnorm (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call op_ldnorm(y, mu, s, ld)
    end associate
  end subroutine node__op_ldnorm

  !> ldnorm - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_ldnorm (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call fw_ldnorm(y, mu, s, ld)
    end associate
  end subroutine node__fw_ldnorm

  !> ldnorm - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_ldnorm (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call bw_ldnorm(y, mu, s, ld)
    end associate
  end subroutine node__bw_ldnorm
  !> @}

  !> @defgroup node_operators_stats_ldmvnorm ldmvnorm
  !! Multivariante Normal Distribution - log-density
  !! @todo implement fw
  !! @{

  !> ldmvnorm - operator
  !! @param[in] nd 'node'
  subroutine node__op_ldmvnorm__1 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         E => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call op_ldmvnorm__1(y, mu, E, ld)
    end associate
  end subroutine node__op_ldmvnorm__1

  !> ldmvnorm - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_ldmvnorm__1 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         E => NUMBERS_(nd%in(3)), &
         ld => NUMBERS_(nd%out) &
         )
      call bw_ldmvnorm__1(y, mu, E, ld)
    end associate
  end subroutine node__bw_ldmvnorm__1
  !! @}

  !> @defgroup node_operators_stats_lkh_norm__1 lkh_norm__1
  !! Normal log-likelyhood for indepent variables
  !! @{

  !> lkh_norm__1 - operator
  !! @param[in] nd 'node'
  subroutine node__op_lkh_norm__1 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         L => NUMBERS_(nd%out) &
         )
      call op_lkh_norm(y, mu, s, L)
    end associate
  end subroutine node__op_lkh_norm__1

  !> lkh_norm__1 - forward differentiation
  !! @param[in] nd 'node'
  subroutine node__fw_lkh_norm__1 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         L => NUMBERS_(nd%out) &
         )
      call fw_lkh_norm(y, mu, s, L)
    end associate
  end subroutine node__fw_lkh_norm__1

  !> lkh_norm__1 - backward differentiation
  !! @param[in] nd 'node'
  subroutine node__bw_lkh_norm__1 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), &
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         L => NUMBERS_(nd%out) &
         )
      call bw_lkh_norm(y, mu, s, L)
    end associate
  end subroutine node__bw_lkh_norm__1
  !> @}

  !> @defgroup node_operators_stats_lkh_norm__2 lkh_norm__2
  !! Weighted Normal log-likelyhood for indepent variables
  !! @{

  !> lkh_norm__2 - operator
  !! @param[in] nd 'node'
  subroutine node__op_lkh_norm__2 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), & 
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         w => NUMBERS_(nd%in(4)), &
         L => NUMBERS_(nd%out) &
         )
      call op_lkh_norm(y, mu, s, w, L)
    end associate
  end subroutine node__op_lkh_norm__2
  
  !> lkh_norm__2 - forward differentiation
  !! @param[in] nd 'node
  subroutine node__fw_lkh_norm__2 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), & 
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         w => NUMBERS_(nd%in(4)), &
         L => NUMBERS_(nd%out) &
         )
      call fw_lkh_norm(y, mu, s, w, L)
    end associate
  end subroutine node__fw_lkh_norm__2
  
  !> lkh_norm__2 - backward differentiation
  !! @param[in] nd 'node
  subroutine node__bw_lkh_norm__2 (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         y => NUMBERS_(nd%in(1)), & 
         mu => NUMBERS_(nd%in(2)), &
         s => NUMBERS_(nd%in(3)), &
         w => NUMBERS_(nd%in(4)), &
         L => NUMBERS_(nd%out) &
         )
      call bw_lkh_norm(y, mu, s, w, L)
    end associate
  end subroutine node__bw_lkh_norm__2
  !> @}
  !> @}

  !> @defgroup node_operators_kernels_ksqexp ksqexp
  !! Squared Exponential Kernel
  !! @todo implement fw
  !! @{

  !> ksqexp - operator
  !! @param[in] nd 'node
  subroutine node__op_ksqexp (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         a => NUMBERS_(nd%in(3)), &
         b => NUMBERS_(nd%in(4)), &
         k => NUMBERS_(nd%out) &
         )
      call op_ksqexp(x1, x2, a, b, k)
    end associate
  end subroutine node__op_ksqexp

  !> ksqexp - backward differentiation
  !! @param[in] nd 'node
  subroutine node__bw_ksqexp (nd)
    implicit none
    type(node), intent(in) :: nd
    associate( &
         x1 => NUMBERS_(nd%in(1)), &
         x2 => NUMBERS_(nd%in(2)), &
         a => NUMBERS_(nd%in(3)), &
         b => NUMBERS_(nd%in(4)), &
         k => NUMBERS_(nd%out) &
         )
      call bw_ksqexp(x1, x2, a, b, k)
    end associate
  end subroutine node__bw_ksqexp
  !> @}
   
end module node_operators
