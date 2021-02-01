module nodes_utils

  use env
  use types
  use registers, only: &
       NUMBERS_, &
       NUMNDS_, &
       NODEGRPHS_, &
       NODEi_, &
       GRAPHi_, &
       GRAPH_, &
       GRAPHS_
  use errwarn
  use utils, only: alloc, dealloc
  use numbers_utils
  
  implicit none

  character(len=*), parameter :: mod_nodes_utils_name_ = 'nodes_utils'


  !> Allocte a node given its 'number' inputs ans outputs.
  !! @author Filippo Monari
  !! @param[inout] x 'node' to allocate
  !! @param[in] op integer indicatin the node operator
  !! @param[in] x1,x2,... 'number', node inputs
  !! @param[in] ans 'number', node output
  !! @param[in] flg integer(:), flags to be passed to the operator
  interface node__allocate
     module procedure node__allocate__1_no
     module procedure node__allocate__1
     module procedure node__allocate__2
     module procedure node__allocate__3
     module procedure node__allocate__4
     module procedure node__allocate__5
  end interface node__allocate

contains

  !> Returns the number of inputs of a node given its class
  !! @author Filippo Monari
  !! @param[in] op integer, operator id
  function node__insz (op) result(sz)
    implicit none
    integer, intent(in) :: op
    integer :: sz
    call do_safe_within('node__insz', mod_nodes_utils_name_, private_insz)
  contains
    subroutine private_insz
      select case (op)
      case ( &
           op_abs_id_, &
           op_exp_id_, &
           op_log_id_, &
           op_sin_id_, &
           op_cos_id_, &
           op_tan_id_, &
           op_sinh_id_, &
           op_cosh_id_, &
           op_tanh_id_, &
           op_invMat_id_, &
           op_invSymMat_id_, &
           op_sigmoid_id_, &
           op_relu_id_, &
           op_swish_id_, &
           op_softmax1_id_, &
           op_softmax2_id_, &
           op_sum_id_, &
           op_sum2_id_, &
           op_product1_id_, &
           op_product2_id_, &
           op_ssq_id_, &
           op_slice_id_, &
           op_flat_slice_id_, &
           op_contiguous_slice_id_, &
           op_reshape_id_, &
           op_drop_shape_id_, &
           op_feed_id_ &
           )
         sz = 1
      case ( &
           op_add_id_, &
           op_sub_id_, &
           op_mult_id_, &
           op_pow_id_, &
           op_div_id_, &
           op_bin_entropy_id_, &
           op_cross_entropy1_id_, &
           op_cross_entropy2_id_, &
           op_mse_id_, &
           op_mae_id_, &
           op_dgemm4_id_ , &
           op_dp_gemv4_id_, &
           op_dp_ger3_id_, &
           op_dp_dot_id_, &
           op_bind_id_, &
           op_embeddings_id_, &
           op_ldexp_id_ &
           )
         sz = 2
      case ( &
           op_dgemm2_id_, &
           op_dgemm3_id_, &
           op_dp_gemv2_id_, &
           op_dp_gemv3_id_, &
           op_dp_ger2_id_, &
           op_lkh_norm1_id_, &
           op_ldlaplace_id_, &
           op_ldbeta_id_, &
           op_ldgamma_id_, &
           op_ldnorm_id_, &
           op_ldmvnorm1_id_ &
           )
         sz = 3
      case ( &
           op_dp_ger1_id_, &
           op_lkh_norm2_id_, &
           op_ksqexp_id_ &
           )
         sz = 4
      case (&
           op_dgemm1_id_, &
           op_dp_gemv1_id_ &
         )
         sz = 5
      case default
         call raise_error('op', err_unknwnVal_)
      end select
    end subroutine private_insz
  end function node__insz

  !> Returns the number of flags needed by the node operator.
  !! In the case that the size of the flg vector is not known,
  !! it returns -1 meaning that it is established upon allocation.
  !! @author Filippo Monari
  !! @param[in] op integer, 'node' operator
  function node__flgsz (op) result(sz)
    implicit none
    integer, intent(in) :: op
    integer :: sz
    call do_safe_within('node__flgsz', mod_nodes_utils_name_, private_flgsz)
  contains
    subroutine private_flgsz
      select case (op)
      case ( &
           op_add_id_, &
           op_sub_id_, &
           op_mult_id_, &
           op_pow_id_, &
           op_div_id_, &
           op_abs_id_, &
           op_exp_id_, &
           op_log_id_, &
           op_sin_id_, &
           op_cos_id_, &
           op_tan_id_, &
           op_sinh_id_, &
           op_cosh_id_, &
           op_tanh_id_, &
           op_sum_id_, &
           op_product1_id_, &
           op_ssq_id_, &
           op_invMat_id_, &
           op_invSymMat_id_, &
           op_sigmoid_id_, &
           op_relu_id_, &
           op_swish_id_, &
           op_softmax1_id_, &
           op_bin_entropy_id_, &
           op_cross_entropy1_id_, &
           op_mse_id_, &
           op_mae_id_, &
           op_drop_shape_id_, &
           op_dp_ger1_id_, &
           op_dp_ger2_id_, &
           op_dp_ger3_id_, &
           op_dp_dot_id_, &
           op_ldexp_id_, &
           op_ldlaplace_id_, &
           op_ldbeta_id_, &
           op_ldgamma_id_, &
           op_ldnorm_id_, &
           op_ldmvnorm1_id_, &
           op_lkh_norm1_id_, &
           op_lkh_norm2_id_, &
           op_ksqexp_id_, &
           op_feed_id_ &
           )
         sz = 0
      case ( &
           op_embeddings_id_, &
           op_dp_gemv1_id_, &
           op_dp_gemv2_id_, &
           op_dp_gemv3_id_, &
           op_dp_gemv4_id_, &
           op_cross_entropy2_id_, &
           op_softmax2_id_, &
           op_bind_id_, &
           op_sum2_id_, &
           op_product2_id_ &
           )
         sz = 1
      case ( &
           op_dgemm1_id_, &
           op_dgemm2_id_, &
           op_dgemm3_id_, &
           op_dgemm4_id_ &
           )
         sz = 2
      case ( &
           op_slice_id_, &
           op_flat_slice_id_, &
           op_contiguous_slice_id_, &
           op_reshape_id_ &
           )
         sz = -1                !in this case the flag vector size is indetermined and it
                                !is established when allocated 
      case default
         call raise_error('op', err_unknwnVal_)
      end select
    end subroutine private_flgsz
  end function node__flgsz

  !> Checks if there is a 'graph' on the stack
  function graphi_is_set () result(ans)
    implicit none
    logical :: ans
    ans = GRAPHi_ > 0 .and. associated(GRAPH_)
  end function graphi_is_set
  
  !> Returns the index of current graph (the one on the stack).
  !! @author Filippo Monari
  function get_graphi () result(ans)
    implicit none
    integer :: ans
    call do_safe_within('get_graphi', mod_nodes_utils_name_, private_graphi)
  contains
    subroutine private_graphi
      ans = 0
      call assert(graphi_is_set(), err_generic_, 'GRAPHi_ is not set.')
      if (err_free()) ans = GRAPHi_
    end subroutine private_graphi
  end function get_graphi

  !> Returns the index of the last node in the stack.
  !! @author Filippo Monari
  function get_nodei () result (ans)
    implicit none
    integer :: ans
    call do_safe_within('get_nodei', mod_nodes_utils_name_, private_nodei)
  contains
    subroutine private_nodei
      ans = 0
      call assert(NODEi_ > 0, err_generic_, 'NODEi_ not set.')
      if (err_free()) ans = NODEi_
    end subroutine private_nodei
  end function get_nodei

  !> Returns the index of the last node on the stack and
  !! flushes it (set the stack to 0)
  !! @author Filippo Monari
  function flush_nodei () result(ans)
    implicit none
    integer :: ans
    call do_safe_within('flush_nodei', mod_nodes_utils_name_, private_flush)
  contains
    subroutine private_flush
      ans = get_nodei()
      if (err_free()) NODEi_ = 0
    end subroutine private_flush
  end function flush_nodei

  !> Sets the current graph index for the node currently on the stack.
  !! @author Filippo Monari
  !! @todo some checkings?
  subroutine set_graphi ()
    implicit none
    call do_safe_within('set_graphi', mod_nodes_utils_name_, private_set_nodegrphs)
  contains
    subroutine private_set_nodegrphs
      NODEGRPHS_(get_nodei()) = get_graphi()
    end subroutine private_set_nodegrphs
  end subroutine set_graphi
  
  !> Base subroutine for allocating nodes. Not included in the interface.
  !! @param[inout] x 'node' to allocate
  !! @param[in] op interger, operator id
  !! @param[in] out integer, id of the output 'number'
  !! @param[in] in integer(:), ids of the input 'numbers'
  !! @param[in] flg integer(:), vector for flag parameters
  subroutine node__allocate__0 (x, op, out, in, flg)
    implicit none
    type(node), intent(inout) :: x
    integer, intent(in) :: op, out, in(:), flg(:)
    call do_safe_within('node__allocate__0', mod_nodes_utils_name_, private_do)
  contains
    subroutine private_do
      call assert(.not. is_allocated(x), err_alreadyInit_, 'x')
      call assert(node__insz(op) == size(in), err_wrngSz_, 'in')
      call assert(node__flgsz(op) < 0 .or. node__flgsz(op) == size(flg), err_wrngSz_, 'flg')
      call alloc(x%in, in, 'in')
      call alloc(x%flg, flg, 'flg')
      call err_safe(private_init)
      call node__update_inlock(x, '+')
      call set_graphi()
    end subroutine private_do
    subroutine private_init
      x%op = op
      x%out = out
      x%id = get_nodei()
    end subroutine private_init
  end subroutine node__allocate__0

  !> Allocates nodes with one input
  !! @param[inout] x 'node' to allocate
  !! @param[in] op interger, operator id
  !! @param[in] x1 input 'number'
  !! @param[in] ans, output 'number'
  !! @param[in] flg integer(:), vector for flag parameters
  subroutine node__allocate__1 (x, op, x1, ans, flg)
    implicit none
    type(node), intent(inout) :: x
    integer, intent(in) :: op
    type(number), intent(in) :: x1
    type(number), intent(inout) :: ans
    integer, intent(in) :: flg(:)
    call do_safe_within('node__allocate__1', mod_nodes_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call node__allocate__0(x, op, ans%id, [x1%id], flg)
      call number__set_nd(ans, get_nodei())
    end subroutine private_allocate
  end subroutine node__allocate__1

  !> Allocates nodes with one input and no output
  !! @param[inout] x 'node' to allocate
  !! @param[in] op interger, operator id
  !! @param[in] x1 input 'number'
  !! @param[in] flg integer(:), vector for flag parameters
  subroutine node__allocate__1_no (x, op, x1, flg)
    implicit none
    type(node), intent(inout) :: x
    integer, intent(in) :: op
    type(number), intent(in) :: x1
    integer, intent(in) :: flg(:)
    call do_safe_within('node__allocate__1_no', mod_nodes_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call node__allocate__0(x, op, -1, [x1%id], flg)
    end subroutine private_allocate
  end subroutine node__allocate__1_no

  !> Allocates nodes with two input
  !! @param[inout] x 'node' to allocate
  !! @param[in] op interger, operator id
  !! @param[in] x1 input 'number'
  !! @param[in] x2 input 'number'
  !! @param[in] ans, output 'number'
  !! @param[in] flg integer(:), vector for flag parameters
  subroutine node__allocate__2 (x, op, x1, x2, ans, flg)
    implicit none
    type(node), intent(inout) :: x
    integer, intent(in) :: op
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: ans
    integer, intent(in) :: flg(:)
    call do_safe_within('node__allocate__3', mod_nodes_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call node__allocate__0(x, op, ans%id, [x1%id, x2%id], flg)
      call number__set_nd(ans, get_nodei())
    end subroutine private_allocate
  end subroutine node__allocate__2
  
  !> Allocates nodes with three input
  !> Allocates nodes with two input
  !! @param[inout] x 'node' to allocate
  !! @param[in] op interger, operator id
  !! @param[in] x1 input 'number'
  !! @param[in] x2 input 'number'
  !! @param[in] x3 input 'number'
  !! @param[in] ans, output 'number'
  !! @param[in] flg integer(:), vector for flag parameters
  subroutine node__allocate__3 (x, op, x1, x2, x3, ans, flg)
    implicit none
    type(node), intent(inout) :: x
    integer, intent(in) :: op
    type(number), intent(in) :: x1, x2, x3
    type(number), intent(inout) :: ans
    integer, intent(in) :: flg(:)
    call do_safe_within('node__allocate__3', mod_nodes_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call node__allocate__0(x, op, ans%id, [x1%id, x2%id, x3%id], flg)
      call number__set_nd(ans, get_nodei())
    end subroutine private_allocate
  end subroutine node__allocate__3

  !> Allocates nodes with four input
  !! @param[inout] x 'node' to allocate
  !! @param[in] op interger, operator id
  !! @param[in] x1 input 'number'
  !! @param[in] x2 input 'number'
  !! @param[in] x3 input 'number'
  !! @param[in] x4 input 'number'
  !! @param[in] ans, output 'number'
  !! @param[in] flg integer(:), vector for flag parameters
  subroutine node__allocate__4 (x, op, x1, x2, x3, x4, ans, flg)
    implicit none
    type(node), intent(inout) :: x
    integer, intent(in) :: op
    type(number), intent(in) :: x1, x2, x3, x4
    type(number), intent(inout) :: ans
    integer, intent(in) :: flg(:)
    call do_safe_within('node__allocate__4', mod_nodes_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call node__allocate__0(x, op, ans%id, [x1%id, x2%id, x3%id, x4%id], flg)
      call number__set_nd(ans, get_nodei())
    end subroutine private_allocate
  end subroutine node__allocate__4
  
  !> Allocates nodes with five input
  !! @param[inout] x 'node' to allocate
  !! @param[in] op interger, operator id
  !! @param[in] x1 input 'number'
  !! @param[in] x2 input 'number'
  !! @param[in] x3 input 'number'
  !! @param[in] x4 input 'number'
  !! @param[in] x4 input 'number'
  !! @param[in] ans, output 'number'
  !! @param[in] flg integer(:), vector for flag parameters
  subroutine node__allocate__5 (x, op, x1, x2, x3, x4, x5, ans, flg)
    implicit none
    type(node), intent(inout) :: x
    integer, intent(in) :: op
    type(number), intent(in) :: x1, x2, x3, x4, x5
    type(number), intent(inout) :: ans
    integer, intent(in) :: flg(:)
    call do_safe_within('node5__allocate', mod_nodes_utils_name_, private_allocate)
  contains
    subroutine private_allocate
      call node__allocate__0(x, op, ans%id, [x1%id, x2%id, x3%id, x4%id, x5%id], flg)
      call number__set_nd(ans, get_nodei())
  end subroutine private_allocate
  end subroutine node__allocate__5

  !> Converts a 'node' to a vector of integers
  !! @author Filippo Monari
  !! @param[in] x 'node' to be converted
  function node__to_vec (x) result(v)
    implicit none
    type(node), intent(in) :: x
    integer, allocatable :: v(:)
    integer :: insz, flgsz
    call do_safe_within('node__to_vec', mod_nodes_utils_name_, private_to_vec)
  contains
    subroutine private_to_vec
      insz = size(x%in)
      flgsz = size(x%flg)
      call alloc(v, [insz, flgsz, x%id, x%op, x%out, x%in, x%flg], 'v')
    end subroutine private_to_vec
  end function node__to_vec

  !> Updates the inlock count for all the inputs to a 'node'.
  !! @author Filippo Monari
  !! @param[in] x 'node'
  !! @param[in] m character, update mode (i.e. '+' or '-')
  subroutine node__update_inlock (x, m)
    type(node), intent(in) :: x
    character(len=*), intent(in) :: m
    integer :: i, ii
    call do_safe_within('node__inlock_update', mod_nodes_utils_name_, private_update)
  contains
    subroutine private_update
      call assert(is_allocated(x), err_notInit_, 'x')
      if (err_free()) then
         do i = 1, size(x%in)
            ii = x%in(i)
            call number__inlock(NUMBERS_(ii), m)
         end do
      end if
    end subroutine private_update
  end subroutine node__update_inlock

  !> Pack a 'graph' by removing the index of the node from its node vector.
  !! @author Filippo Monari
  !! @param[in] x 'node' to be removed
  subroutine node__pack_graph (x)
    type(node), intent(inout) :: x
    integer, allocatable :: tmp(:)
    integer :: i
    call do_safe_within('node__pack_graph', mod_nodes_utils_name_, private_pack)
  contains
    subroutine private_pack
      call assert(is_allocated(x), err_notInit_, 'x')
      if (err_free()) i = NODEGRPHS_(x%id)
      call alloc(tmp, pack(GRAPHS_(i)%nodes, GRAPHS_(i)%nodes /= x%id), 'tmp')
      call dealloc(GRAPHS_(i)%nodes, 'GRAPH_(i)%nodes')
      call alloc(GRAPHS_(i)%nodes, tmp, 'GRAPHS_(i)%nodes')
    end subroutine private_pack
  end subroutine node__pack_graph

  !> Reset (set to -1) the 'NUMNDS_' position corresponding to the 'number'
  !! output of the 'node'.
  !! This means that the 'number' was output of a node that does not exists
  !! anymore.
  !! @author Filippo Monari
  !! @param[in] x 'node'
  subroutine node__reset_number (x)
    type(node), intent(in) :: x
    call do_safe_within('node__rest_number', mod_nodes_utils_name_, private_reset)
  contains
    subroutine private_reset
      NUMNDS_(x%out) = -1
    end subroutine private_reset
  end subroutine node__reset_number

  !> Deallocate a 'node'
  !! @author Filippo Monari
  !! @param[in] x 'node'
  subroutine node__deallocate (x)
    implicit none
    type(node), intent(inout) :: x
    call do_safe_within('node__deallocate', mod_nodes_utils_name_, private_do)
  contains
    subroutine private_do
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call dealloc(x%in, 'x%in')
      call dealloc(x%flg, 'x%flg')
      call err_safe(private_deinit)
    end subroutine private_do
    subroutine private_deinit
      x%id = 0
      x%op = 0
      x%out = 0
    end subroutine private_deinit
  end subroutine node__deallocate

end module nodes_utils
