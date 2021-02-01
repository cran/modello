module interfaces

  use env
  use registers
  use errwarn
  use numbers_utils
  use numbers
  use numbers_modifiers
  use numbers_math
  use nodes_utils
  use nodes
  use sgd
  use optim_utils
  use iso_c_binding
  use types

  character(len=*), parameter :: mod_interfaces_name_ = 'interfaces'
  
contains
  
  !> Allocates the NUMBERS_ array and it dependency registers
  !! @param[in] n c_int, size of the array
  subroutine intrf_f__allocate_numbers (n) bind(C, name='intrf_f__allocate_numbers_')
    implicit none
    integer(kind=c_int), intent(in) :: n
    call do_within('intrf_f__allocate_numbers', mod_interfaces_name_, private_allocate)
  contains
    subroutine private_allocate
      call allocate_numbers(n)
    end subroutine private_allocate
  end subroutine intrf_f__allocate_numbers

  !> Deallocates the NUMBERS_ array and its dependency registers
  subroutine intrf_f__deallocate_numbers () bind(C, name='intrf_f__deallocate_numbers_')
    implicit none
    call do_within('intrf_f__deallocate_numbers_', mod_interfaces_name_, private_deallocate)
  contains
    subroutine private_deallocate
      call deallocate_numbers
    end subroutine private_deallocate
  end subroutine intrf_f__deallocate_numbers

  !> Checks if a 'number' is allocated
  !! @param[in] id c_int, id of the 'number'
  !! @param[out] ans c_int, is equal to 1 if the 'number' is allocated, 0 otherwise
  subroutine intrf_f__number__is_allocated (id, ans) bind(C, name='intrf_f__number__is_allocated_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: ans
    call do_within('intrf_f__number_is_allocated', mod_interfaces_name_, private_is_allocated)
  contains
    subroutine private_is_allocated
      if (allocated(NUMBERS_)) then
         call assert(id > 0 .and. id <= size(NUMBERS_), err_oorng_, 'id')
         if (err_free()) ans = merge(1, 0, is_allocated(NUMBERS_(id)))
      else
         ans = 0
      end if
    end subroutine private_is_allocated
  end subroutine intrf_f__number__is_allocated

  !> Checks if a 'number' has dv allocated
  !! @param[in] id c_int, 'number' id
  !! @param[out] ans c_int, 1 if dv is allocated else 0
  subroutine intrf_f__number__has_dx (id, ans) bind(C, name='intrf_f__number__has_dx_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: ans
    call do_within('intrf_f__number__is_allocated', mod_interfaces_name_, private_has_dx)
  contains
    subroutine private_has_dx
      ans = merge(1, 0, has_dx(nnn(id)))
    end subroutine private_has_dx
  end subroutine intrf_f__number__has_dx
  
  !> Gets the rank of a 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[out] r c_int, rank of the number
  subroutine intrf_f__number__rank (id, r) bind(C, name='intrf_f__number__rank_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: r
    call do_within('intrf_f__number__rank', mod_interfaces_name_, private_rank)
  contains
    subroutine private_rank
      r = mtrnk(NUMBERS_(id))
    end subroutine private_rank
  end subroutine intrf_f__number__rank

  !> Append a 'number' of the given shape to the NUMBER_ array
  !! @param[out] id c_int, appended 'number' id
  !! @param[in] r c_in, rank of the 'number'
  !! @param[in] shp c_int, shape of the number
  !! @param[in] dx c_int, if > 0 the 'number' has a dx element
  subroutine intrf_f__number__append (id, r, shp, dx) bind(C, name='intrf_f__number__append_')
    implicit none
    integer(kind=c_int), intent(out) :: id
    integer(kind=c_int), intent(in) :: r, shp(r), dx
    type(number), pointer :: x
    call do_within('intrf_f__number__append', mod_interfaces_name_, private_append)
  contains
    subroutine private_append
      if (r == 0) then
         call number__append(x, [integer::], dx > 0)
      else
         call number__append(x, shp, dx > 0)
      end if
      if (err_free()) id = x%id
    end subroutine private_append
  end subroutine intrf_f__number__append

  !> Gets the size of a 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[out] sz c_int, size of the 'number'
  subroutine intrf_f__number__size (id, sz) bind(C, name='intrf_f__number__size_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: sz
    call do_within('intrf_f__number__size', mod_interfaces_name_, private_size)
  contains
    subroutine private_size
      sz = mtsz(NUMBERS_(id))
    end subroutine private_size
  end subroutine intrf_f__number__size

  !> Gets the shape of a 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[in] r c_int, rank of the 'number'
  !! @param[out] shp c_int, shape of the 'number'
  subroutine intrf_f__number__shape (id, r, shp) bind(C, name='intrf_f__number__shape_')
    implicit none
    integer(kind=c_int), intent(in) :: id, r
    integer(kind=c_int), intent(out) :: shp(r)
    call do_within('intrf_f__number__shape', mod_interfaces_name_, private_shape)
  contains
    subroutine private_shape
      shp = NUMBERS_(id)%shp
    end subroutine private_shape
  end subroutine intrf_f__number__shape

  !> Sets the value of a 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[in] n c_int, size of the 'number'
  !! @param[inout] x c_double, new value of the number
  !! @todo implement case where a scalar is provided
  subroutine intrf_f__number__set_v (id, n, x) bind(C, name='intrf_f__number__set_v_')
    implicit none
    integer(kind=c_int), intent(in) :: id, n
    real(kind=c_double), intent(inout) :: x(n)
    call do_within('intrf_f__number__set_v', mod_interfaces_name_, private_set_v)
  contains
    subroutine private_set_v
      type(number), pointer :: xx
      call get_number(id, xx)
      if (n > 1) then
         call number__set_v(xx, x)
      else
         call number__set_v(xx, x(1))
      end if
    end subroutine private_set_v
  end subroutine intrf_f__number__set_v

  !> Stes the derivative of 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[in] n c_int, size of the 'number'
  !! @param[inout] x c_double, new derivative value
  !! @todo implement case where a scalar is provided
  subroutine intrf_f__number__set_dv (id, n, x) bind(C, name='intrf_f__number__set_dv_')
    implicit none
    integer(kind=c_int), intent(in) :: id, n
    real(kind=c_double), intent(inout) :: x(n)
    call do_within('intrf_f__number__set_dv', mod_interfaces_name_, private_set_dv)
  contains
    subroutine private_set_dv
      type(number), pointer :: xx
      call get_number(id, xx)
      if (n > 1) then
         call number__set_dv(xx, x)
      else
         call number__set_dv(xx, x(1))
      end if
    end subroutine private_set_dv
  end subroutine intrf_f__number__set_dv

  subroutine intrf_f__number__set_slice_v (id, v, s, m, n, l) bind(C, name='intrf_f__number__set_slice_v_')
    implicit none
    integer(kind=c_int), intent(in) :: id, l, m, n, s(m,n)
    real(kind=c_double), intent(in) :: v(l)
    call do_within('intrf_f__number__set_slice_v', mod_interfaces_name_, private_set_slice_v)
  contains
    subroutine private_set_slice_v
      type(number), pointer :: x
      call get_number(id, x)
      if (l > 1) then
         call number__set_slice_v(x, s, v)
      else
         call number__set_slice_v(x, s, v(1))
      end if
    end subroutine private_set_slice_v
  end subroutine intrf_f__number__set_slice_v

  subroutine intrf_f__number__set_slice_dv (id, dv, s, m, n, l) bind(C, name='intrf_f__number__set_slice_dv_')
    implicit none
    integer(kind=c_int), intent(in) :: id, l, m, n, s(m,n)
    real(kind=c_double), intent(in) :: dv(l)
    call do_within('intrf_f__number__set_slice_dv', mod_interfaces_name_, private_set_slice_dv)
  contains
    subroutine private_set_slice_dv
      type(number), pointer :: x
      call get_number(id, x)
      if (l > 1) then
         call number__set_slice_dv(x, s, dv)
      else
         call number__set_slice_dv(x, s, dv(1))
      end if
    end subroutine private_set_slice_dv
  end subroutine intrf_f__number__set_slice_dv

  subroutine intrf_f__number__set_flat_slice_v (id, v, s, m, l) bind(C, name='intrf_f__number__set_flat_slice_v_')
    implicit none
    integer(kind=c_int), intent(in) :: id, l, m, s(m)
    real(kind=c_double), intent(in) :: v(l)
    call do_within('intrf_f__number__set_flat_slice_v', mod_interfaces_name_, private_set_flat_slice_v)
  contains
    subroutine private_set_flat_slice_v
      type(number), pointer :: x
      call get_number(id, x)
      if (l > 1) then
         call number__set_flat_slice_v(x, s, v)
      else
         call number__set_flat_slice_v(x, s, v(1))
      end if
    end subroutine private_set_flat_slice_v
  end subroutine intrf_f__number__set_flat_slice_v

  subroutine intrf_f__number__set_flat_slice_dv (id, dv, s, m, l) bind(C, name='intrf_f__number__set_flat_slice_dv_')
    implicit none
    integer(kind=c_int), intent(in) :: id, l, m, s(m)
    real(kind=c_double), intent(in) :: dv(l)
    call do_within('intrf_f__number__set_flat_slice_dv', mod_interfaces_name_, private_set_flat_slice_dv)
  contains
    subroutine private_set_flat_slice_dv
      type(number), pointer :: x
      call get_number(id, x)
      if (l > 1) then
         call number__set_flat_slice_dv(x, s, dv)
      else
         call number__set_flat_slice_dv(x, s, dv(1))
      end if
    end subroutine private_set_flat_slice_dv
  end subroutine intrf_f__number__set_flat_slice_dv
  
  
  !> Gets the value of a 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[in] n c_int, 'number' size
  !! @param[out] x c_double, where to store the 'number' value
  subroutine intrf_f__number__get_v (id, n, x) bind(C, name='intrf_f__number__get_v_')
    implicit none
    integer(kind=c_int), intent(in) :: id, n
    real(kind=c_double), intent(out) :: x(n)
    real(dp_), allocatable :: xx(:)
    call do_within('intrf_f__number__get_v', mod_interfaces_name_, private_get_x)
  contains
    subroutine private_get_x
      call number__get_v(NUMBERS_(id), xx)
      if (err_free()) x = xx
    end subroutine private_get_x
  end subroutine intrf_f__number__get_v

  !> Gets the derivative value of a 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[in] n c_int, 'number' size
  !! @param[out] x c_double, where to store the 'number' derivative value
  subroutine intrf_f__number__get_dv (id, n, x) bind(C, name='intrf_f__number__get_dv_')
    implicit none
    integer(kind=c_int), intent(in) :: id, n
    real(kind=c_double), intent(out) :: x(n)
    real(dp_), allocatable :: xx(:)
    call do_within('intrf_f__number__get_dv', mod_interfaces_name_, private_get_dx)
  contains
    subroutine private_get_dx
      call number__get_dv(NUMBERS_(id), xx)
      if (err_free()) x = xx
    end subroutine private_get_dx
  end subroutine intrf_f__number__get_dv

  !> Gets the 'node' id that has as output a certain 'number'
  !! @param[in] id c_int, 'number' id
  !! @param[out] ans c_int, 'node' id
  subroutine intrf_f__number__get_nd (id, ans) bind(C, name='intrf__number__get_nd_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: ans
    call do_within('intrf_f__number__get_nd', mod_interfaces_name_, private_get_nd)
  contains
    subroutine private_get_nd
      ans = number__get_nd(nnn(id))
    end subroutine private_get_nd
  end subroutine intrf_f__number__get_nd

  !> Checks if a number as input depencies
  !! @param[in] id c_int, 'number' id
  !! @param[out] ans c_int, 1 if the 'number' is free else 0
  subroutine intrf_f__number__inlock_free (id, ans) bind(C, name='intrf_f__number__inlock_free_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: ans
    call do_within('intrf_f_number__inlock_fee', mod_interfaces_name_, private_inlock_free)
  contains
    subroutine private_inlock_free
      if (allocated(NUMBERS_)) then
         ans = merge(1, 0, number__inlock_free(nnn(id)))
      else
         ans = 0
      end if
    end subroutine private_inlock_free
  end subroutine intrf_f__number__inlock_free

  !> Pop (removes) a 'number' from the NUMBERS_ array.
  !! @param[in] id c_int, 'number' id
  subroutine intrf_f__number__pop (id) bind(C, name='intrf_f__number__pop_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    call do_within('intrf_f__number__pop', mod_interfaces_name_, private_pop)
  contains
    subroutine private_pop
      type(number), pointer :: x
      call get_number(id, x)
      call number__pop(x)
    end subroutine private_pop
  end subroutine intrf_f__number__pop

  !> Calls the garbage collectors for 'numbers'
  subroutine intrf_f__number__gc () bind(C, name='intrf_f__number__gc_')
    implicit none
    call do_within('intrf_f__number__gc', mod_interfaces_name_, private_gc)
  contains
    subroutine private_gc
      call number__gc
    end subroutine private_gc
  end subroutine intrf_f__number__gc

  !> Allocates the NODES_ array
  !! @param[in] n c_int, size of the array
  subroutine intrf_f__nodes__allocate (n) bind(C, name='intrf_f__nodes__allocate_')
    implicit none
    integer(kind=c_int), intent(in) :: n
    call do_within('intrf_f__node__allocate', mod_interfaces_name_, private_allocate)
  contains
    subroutine private_allocate
      call nodes__allocate(n)
    end subroutine private_allocate
  end subroutine intrf_f__nodes__allocate

  !> Deallocates the NODES_ array
  subroutine intrf_f__deallocate_nodes () bind(C, name='intrf_f__deallocate_nodes_')
    implicit none
    call do_within('intrf_f_deallocate_nodes', mod_interfaces_name_, private_deallocate)
  contains
    subroutine private_deallocate
      call deallocate_nodes
    end subroutine private_deallocate
  end subroutine intrf_f__deallocate_nodes

  !> Allocates the GRAPHS_ array
  !! @param[in] n c_int, array size
  subroutine intrf_f__graphs__allocate (n) bind(C, name='intrf_f__graphs__allocate_')
    implicit none
    integer(kind=c_int), intent(in) :: n
    call do_within('intrf_f__graphs__allocate', mod_interfaces_name_, private_allocate)
  contains
    subroutine private_allocate
      call graphs__allocate(n)
    end subroutine private_allocate
  end subroutine intrf_f__graphs__allocate

  !> Checks if a 'graph' is allocated
  !! @param[in] id c_int, 'graph' id
  !! @param[out] ans c_int, if is allocated equal to 1 otherwise to 0
  subroutine intrf_f__graph__is_allocated (id, ans) bind(C, name='intrf_f__graph__is_allocated_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: ans
    call do_within('intrf_f__graph_is_allocated', mod_interfaces_name_, private_is_allocated)
  contains
    subroutine private_is_allocated
      if (allocated(GRAPHS_)) then
         call assert(id > 0 .and. id <= size(GRAPHS_), err_oorng_, 'id')
         if (err_free()) ans = merge(1, 0, is_allocated(GRAPHS_(id)))
      else
         ans = 0
      end if
    end subroutine private_is_allocated
  end subroutine intrf_f__graph__is_allocated

  !> Deallocates the GRAPHS_ array
  subroutine intrf_f__deallocate_graphs () bind(C, name='intrf_f__deallocate_graphs_')
    implicit none
    call do_within('intrf_f_deallocate_ndoes', mod_interfaces_name_, private_deallocate)
  contains
    subroutine private_deallocate
      call deallocate_graphs
    end subroutine private_deallocate
  end subroutine intrf_f__deallocate_graphs

  !> Open an existing or new 'graph', and puts it on the stack
  !! @param[in] i c_inf, 'graph' id, if < 1 it opens a new 'graph'
  subroutine intrf_f__graph__open (i) bind(C, name='intrf_f__graph__open_')
    implicit none
    integer(kind=c_int), intent(in) :: i
    call do_within('intrf_f__with_graph', mod_interfaces_name_, private_open)
  contains
    subroutine private_open
      if (i > 1) then
         call graph__open(i)
      else
         call graph__open()
      end if
    end subroutine private_open
  end subroutine intrf_f__graph__open

  !> Closes the 'graph' on the stack
  subroutine intrf_f__graph__close () bind(C, name='intrf_f__graph__close_')
    implicit none
    call do_within('intrf_f__graph__close', mod_interfaces_name_, private_close)
  contains
    subroutine private_close
      call graph__close
    end subroutine private_close
  end subroutine intrf_f__graph__close

  !> Returns the 'nodes' indexes within a 'graph'
  !! @param[in] i 'graph' indexes
  !! @param[in] n size of the graph
  !! @param[in] sz flag, > 0 returns the graph size as x(1)
  !! @param[out] x integer(:), stores the results
  subroutine intrf_f__ggg (i, n, x, sz) bind(C, name='intrf_f__ggg_')
    implicit none
    integer(kind=c_int), intent(in) :: i, n, sz
    integer(kind=c_int), intent(out) :: x(n)
    call do_within('intrf_f__ggg', mod_interfaces_name_, private_get)
  contains
    subroutine private_get
      integer :: ii
      type(graph), pointer :: xx
      ii = ggg(i)
      if (err_free()) then
         if (sz > 0 .and. err_free()) then
            x(1) = mtsz(GRAPHS_(ii))
         else
            xx => GRAPHS_(ii)
            call assert(mtsz(xx) == n, err_wrngSz_, 'x')
            if (err_free()) x = xx%nodes
         end if
      end if
    end subroutine private_get
  end subroutine intrf_f__ggg

  !> Pops (removes) a 'graph' from the GRAPHS_ array
  !! @param[in] gi c_int, 'graph' id
  subroutine intrf_f__graph__pop (gi) bind(C, name='intrf_f__graph__pop_')
    implicit none
    integer(kind=c_int), intent(in) :: gi
    call do_within('intrf_f__graph__pop', mod_interfaces_name_, private_pop)
  contains
    subroutine private_pop
      call graph__pop(gi)
    end subroutine private_pop
  end subroutine intrf_f__graph__pop

  !> Calls the garbage collector for 'graphs'
  subroutine intrf_f__graph__gc () bind(C, name='intrf_f__graph__gc_')
    implicit none
    call do_within('intrf_f__graph__gc', mod_interfaces_name_, private_gc)
  contains
    subroutine private_gc
      call graph__gc
    end subroutine private_gc
  end subroutine intrf_f__graph__gc

  !> Gets the indes of the 'graph' on the stack
  !! @param[out] i c_int, index of the 'graph' on the stack
  subroutine intrf_f__get_graphi (i) bind(C, name='intrf_f__get_graphi_')
    implicit none
    integer(kind=c_int) :: i
    call do_within('intrf_f__get_graphi', mod_interfaces_name_, private_get)
  contains
    subroutine private_get
      i = get_graphi()
    end subroutine private_get
  end subroutine intrf_f__get_graphi

  !> Performs the 'node' operator that generated a certain 'number'
  !! @param[in] id c_int, 'number' id
  subroutine intrf_f__number__op (id) bind(C, name='intrf_f__number__op_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    call do_within("intrf_f__number__op", mod_interfaces_name_, private_op)
  contains
    subroutine private_op
      type(number), pointer :: x
      call assert(id > 0 .and. id < size(NUMBERS_), err_oorng_, "id")
      if (err_free()) then
         x => NUMBERS_(id)
         call number__op(x)
      end if
    end subroutine private_op
  end subroutine intrf_f__number__op

  !> Perfroms the forward differentiation relative to the node
  !! that generated a certain 'number'
  !! @param[in] id c_int, 'number' id
  subroutine intrf_f__number__fw (id) bind(C, name='intrf_f__number__fw_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    call do_within("intrf_f__number__fw", mod_interfaces_name_, private_fw)
  contains
    subroutine private_fw
      type(number), pointer :: x
      call assert(id > 0 .and. id < size(NUMBERS_), err_oorng_, "id")
      if (err_free()) then
         x => NUMBERS_(id)
         call number__fw(x)
      end if
    end subroutine private_fw
  end subroutine intrf_f__number__fw

  !> Reset the dx of the 'node' gnerating a certain 'number' according to the
  !! backward differentiation schema.
  !! @param[in] id c_int, 'number' id
  subroutine intrf_f__number__bw_zero (id) bind(C, name='intrf_f__number__bw_zero_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    call do_within("intrf_f__number__bw_zero", mod_interfaces_name_, private_bw_zero)
  contains
    subroutine private_bw_zero
      type(number), pointer :: x
      call assert(id > 0 .and. id < size(NUMBERS_), err_oorng_, "id")
      if (err_free()) then
         x => NUMBERS_(id)
         call number__bw_zero(x)
      end if
    end subroutine private_bw_zero
  end subroutine intrf_f__number__bw_zero

  !> Perfroms the backward differentiation relative to the node
  !! that generated a certain 'number'
  !! @param[in] id c_int, 'number' id
  subroutine intrf_f__number__bw (id) bind(C, name='intrf_f__number__bw_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    call do_within("intrf_f__number__op", mod_interfaces_name_, private_bw)
  contains
    subroutine private_bw
      type(number), pointer :: x
      call get_number(id, x)
      call number__bw(x)
    end subroutine private_bw
  end subroutine intrf_f__number__bw

  !> Performs all the 'node' operators within a certain graph
  !! @param[in] gi c_int, 'graph' id
  subroutine intrf_f__graph__op (gi) bind(C, name='intrf_f__graph__op_')
    implicit none
    integer(kind=c_int), intent(in) :: gi
    call do_within('intrf_f__graph__op', mod_interfaces_name_, private_op)
  contains
    subroutine private_op
      call graph__op(ggg(gi))
    end subroutine private_op
  end subroutine intrf_f__graph__op

  !> Performs all the 'node' forward differentiation operators
  !! within a certain graph
  !! @param[in] gi c_int, 'graph' id
  subroutine intrf_f__graph__fw (gi) bind(C, name='intrf_f__graph__fw_')
    implicit none
    integer(kind=c_int), intent(in) :: gi
    call do_within('intrf_f__graph__op', mod_interfaces_name_, private_fw)
  contains
    subroutine private_fw
      call graph__fw(ggg(gi))
    end subroutine private_fw
  end subroutine intrf_f__graph__fw
  
  !> Reset all the dx within a certain 'graph' according to the
  !! backward differentiation schema
  !! @param[in] gi c_int, 'grph' id
  subroutine intrf_f__graph__bw_zero (gi) bind(C, name='intrf_f__graph__bw_zero_')
    implicit none
    integer(kind=c_int), intent(in) :: gi
    call do_within('intrf_f__graph__bw_zero', mod_interfaces_name_, private_bw_zero)
  contains
    subroutine private_bw_zero
      call graph__bw_zero(ggg(gi))
    end subroutine private_bw_zero
  end subroutine intrf_f__graph__bw_zero

  !> Performs all the 'node' backward differentiation operators
  !! within a certain graph
  !! @param[in] gi c_int, 'graph' id
  subroutine intrf_f__graph__bw (gi) bind(C, name='intrf_f__graph__bw_')
    implicit none
    integer(kind=c_int), intent(in) :: gi
    call do_within('intrf_f__graph__bw', mod_interfaces_name_, private_bw)
  contains
    subroutine private_bw
      call graph__bw(ggg(gi))
    end subroutine private_bw
  end subroutine intrf_f__graph__bw

  !> NUMBERS_MATH

  !> Stores the id of the output (ans) 'number' if no error was found.
  !! @param[in] ans output 'number'
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__ans_id (ans, idout)
    implicit none
    type(number), intent(in) :: ans
    integer(kind=c_int), intent(out) :: idout
    if (err_free()) idout = ans%id
  end subroutine intrf_f__ans_id
  
  !> Interface for Slice
  !! Slices a 'number' according to the provided set od indexes.
  !! @param[in] id c_int, id of the 'number' to slice
  !! @param[in] n c_int, size of the slice
  !! @param[in] s c_int, indexes of the slice
  !! @param[out] idoout c_int, id of the slice 'number'
  subroutine intrf_f__number__slice (id, n, s, idout) bind(C, name='intrf_f__number__slice_')
    implicit none
    integer(kind=c_int), intent(in) :: id, n
    integer(kind=c_int), intent(in), target :: s(n)
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__slice", mod_interfaces_name_, private_slice)
  contains
    subroutine private_slice
      integer, pointer :: ss(:,:)
      call assert(modulo(n, mtrnk(nnn(id))) == 0, err_wrngArg_, 's')
      if (err_free()) then
         ss(1:mtrnk(nnn(id)),1:(n/mtrnk(nnn(id)))) => s
         call intrf_f__ans_id(number__slice(NUMBERS_(id), ss), idout)
      end if
    end subroutine private_slice
  end subroutine intrf_f__number__slice

  !> Interface for Flat Slice
  !! Gets the flat slice of 'number' given the slice indexes
  !! @param[in] id c_int, if of the number to slice
  !! @param[in] n c_int, size of the slice
  !! @param[in] s c_int, indexes of the flat slice
  !! @param[out] idout c_int, id of the flat slice 'number'
  subroutine intrf_f__number__flat_slice (id, n, s, idout) bind(C, name='intrf_f__number__flat_slice_')
    implicit none
    integer(kind=c_int), intent(in) :: id, n, s(n)
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__flat_slice", mod_interfaces_name_, private_slice)
  contains
    subroutine private_slice
      call intrf_f__ans_id(number__flat_slice(NUMBERS_(id), s), idout)
    end subroutine private_slice
  end subroutine intrf_f__number__flat_slice

  !> Interface for Contiguous Slice
  !! Takes a contiguos slice from  a 'number'
  !! @param[in] id c_int, id of the 'number' to slice
  !! @param[in] s c_int, vector of two element: start indexes and final index of the slice
  !! @param[out] idout c_int, id of the 'number' containing conguos slice
  subroutine intrf_f__number__contiguous_slice (id, s1, s2, idout) bind(C, name='intrf_f__number__contiguous_slice_')
    implicit none
    integer(kind=c_int), intent(in) :: id, s1, s2
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__contiguous_slice", mod_interfaces_name_, private_slice)
  contains
    subroutine private_slice
      call intrf_f__ans_id(number__contiguous_slice(NUMBERS_(id), s1, s2), idout)
    end subroutine private_slice
  end subroutine intrf_f__number__contiguous_slice

  !> Interface for Reshape
  !! Reshapes a number according to the given shape
  !! @param[in] id c_int, id of the 'number' to reshape
  !! @param[in] n c_int, size of the shape
  !! @param[in] shp c_int, new shape vector
  !! @param[out] idput c_int, id of the reshaped 'number'
  subroutine intrf_f__number__reshape (id, n, shp, idout) bind(C, name='intrf_f__number__reshape_')
    implicit none
    integer(kind=c_int), intent(in) :: id, n, shp(n)
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__reshape', mod_interfaces_name_, private_reshape)
  contains
    subroutine private_reshape
      call intrf_f__ans_id(number__reshape(NUMBERS_(id), shp), idout)
    end subroutine private_reshape
  end subroutine intrf_f__number__reshape

  !> Interface for Drop
  !! Reshape a 'number' by dropping the collapsed dimensions of a 'number'
  !! @param[in] id c_int, id of the 'number' to reshape
  !! @param[out] idout c_int, id of the reshaped 'number'
  subroutine intrf_f__number__drop_shape (id, idout) bind(C, name='intrf_f__number__drop_shape_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__drop_shape', mod_interfaces_name_, private_drop_shape)
  contains
    subroutine private_drop_shape
      call intrf_f__ans_id(number__drop_shape(NUMBERS_(id)), idout)
    end subroutine private_drop_shape
  end subroutine intrf_f__number__drop_shape

  !> Interface for Bind
  !! Binds two 'numbers' along the given dimension
  !! @param[in] id1 c_int, id of the first 'number'
  !! @param[in] id2 c_int, id of the second 'number'
  !! @param[in] k c_int, dimension index
  !! @param[out] idout c_int, id of the binded 'number'
  subroutine intrf_f__number__bind (id1, id2, k, idout) bind(C, name='intrf_f__number__bind_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2, k
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__bind', mod_interfaces_name_, private_bind)
  contains
    subroutine private_bind
      call intrf_f__ans_id(number__bind(NUMBERS_(id1), NUMBERS_(id2), k), idout)
    end subroutine private_bind
  end subroutine intrf_f__number__bind

  !> Interface for Embeddings
  !! Returns the embedding matrix corresponding the the provided number of fectors
  !! @param[in] idf c_int, id of the factor 'number'
  !! @param[in] idx c_int, id of the 'number' containing the embeddings
  !! @param[in] n c_int, size (number of embeddings in) of the output embedding 'number'
  !! @param[out] idout c_int, id of the output embedding 'number'
  subroutine intrf_f__number__embeddings (idf, idx, n, idout) bind(C, name='intrf_f__number__embeddings_')
    implicit none
    integer(kind=c_int), intent(in) :: idf, idx, n
    integer(kind=c_int), intent(inout) :: idout
    call do_within('intrf_f__number__embeddings', mod_interfaces_name_, private_emb)
  contains
    subroutine private_emb
      call intrf_f__ans_id(number__embeddings(NUMBERS_(idf), NUMBERS_(idx), n), idout)
    end subroutine private_emb
  end subroutine intrf_f__number__embeddings
  
  !> Interface for Addition
  !! @param[in] id1 c_int, id of the first input 'number'
  !! @param[in] id2 c_int, id of the second input 'number'
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__add (id1, id2, idout) bind(C, name='intrf_f__number__add_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__add', mod_interfaces_name_, private_add)
  contains
    subroutine private_add
      call intrf_f__ans_id(NUMBERS_(id1) + NUMBERS_(id2), idout)
    end subroutine private_add
  end subroutine intrf_f__number__add

  !> Interface for Subtraction
  !! @param[in] id1 c_int, id of the first input 'number'
  !! @param[in] id2 c_int, id of the second input 'number'
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__sub (id1, id2, idout) bind(C, name='intrf_f__number__sub_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__sub', mod_interfaces_name_, private_sub)
  contains
    subroutine private_sub
      call intrf_f__ans_id(NUMBERS_(id1) - NUMBERS_(id2), idout)
    end subroutine private_sub
  end subroutine intrf_f__number__sub

  !> Interface for Multiplication
  !! @param[in] id1 c_int, id of the first input 'number'
  !! @param[in] id2 c_int, id of the second input 'number'
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__mult (id1, id2, idout) bind(C, name='intrf_f__number__mult_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__mult', mod_interfaces_name_, private_mult)
  contains
    subroutine private_mult
      call intrf_f__ans_id(NUMBERS_(id1) * NUMBERS_(id2), idout)
    end subroutine private_mult    
  end subroutine intrf_f__number__mult

  !> Interface for Power
  !! @param[in] id1 c_int, id of the first input 'number'
  !! @param[in] id2 c_int, id of the second input 'number'
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__pow (id1, id2, idout) bind(C, name='intrf_f__number__pow_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__pow', mod_interfaces_name_, private_pow)
  contains
    subroutine private_pow
      call intrf_f__ans_id(NUMBERS_(id1) ** NUMBERS_(id2), idout)
    end subroutine private_pow
  end subroutine intrf_f__number__pow

  !> Interface for Division
  !! @param[in] id1 c_int, id of the first input 'number'
  !! @param[in] id2 c_int, id of the second input 'number'
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__div (id1, id2, idout) bind(C, name='intrf_f__number__div_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__div', mod_interfaces_name_, private_div)
  contains
    subroutine private_div
      call intrf_f__ans_id(NUMBERS_(id1) / NUMBERS_(id2), idout)
    end subroutine private_div    
  end subroutine intrf_f__number__div

  !> Interface for Binay Entropy
  !! @param[in] id1 c_int, id of the target 'number'
  !! @param[in] id2 c_int, id of the prediction 'number'
  !! @param[out] idout c_int, id of objective 'number'
  subroutine intrf_f__number__bin_entropy (id1, id2, idout) bind(C, name='intrf_f__number__bin_entropy_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__bin_entropy', mod_interfaces_name_, private_bin_entropy)
  contains
    subroutine private_bin_entropy
      call intrf_f__ans_id(number__bin_entropy(nnn(id1), nnn(id2)), idout)
    end subroutine private_bin_entropy
  end subroutine intrf_f__number__bin_entropy

  !> Interface for Cross-entropy
  !! @param[in] id1 c_int, id of the target 'number'
  !! @param[in] id2 c_int, id of the prediction 'number'
  !! @param[out] idout c_int, id of objective 'number
  !! @todo remove k argument
  subroutine intrf_f__number__cross_entropy (id1, id2, k, idout) bind(C, name='intrf_f__number__cross_entropy_')
    implicit none
    integer(kind=c_int), intent(in) :: id1, id2, k
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__cross_entropy', mod_interfaces_name_, private_cross_entropy)
  contains
    subroutine private_cross_entropy
      integer :: z
      z = k + 1 !to remove
      call intrf_f__ans_id(number__cross_entropy(NUMBERS_(id1), NUMBERS_(id2)), idout)
    end subroutine private_cross_entropy
  end subroutine intrf_f__number__cross_entropy

  !> Interface for Mean Squared Error
  !! @param[in] id1 c_int, id of the target 'number'
  !! @param[in] id2 c_int, id of the prediction 'number'
  !! @param[out] idout c_int, id of objective 'number
  subroutine intrf_f__number__mse (idy, idyh, idout) bind(C, name='intrf_f__number__mse_')
    integer(kind=c_int), intent(in) :: idy, idyh
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__mse', mod_interfaces_name_, private_mse)
  contains
    subroutine private_mse
      call intrf_f__ans_id(number__mse(nnn(idy), nnn(idyh)), idout)
    end subroutine private_mse
  end subroutine intrf_f__number__mse

  !> Interface for Absolute Mean Error
  !! @param[in] id1 c_int, id of the target 'number'
  !! @param[in] id2 c_int, id of the prediction 'number'
  !! @param[out] idout c_int, id of objective 'number
  subroutine intrf_f__number__mae (idy, idyh, idout) bind(C, name='intrf_f__number__mae_')
    integer(kind=c_int), intent(in) :: idy, idyh
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__mae', mod_interfaces_name_, private_mae)
  contains
    subroutine private_mae
      call intrf_f__ans_id(number__mae(nnn(idy), nnn(idyh)), idout)
    end subroutine private_mae
  end subroutine intrf_f__number__mae

  !> Interface for Abs
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__abs (id, idout) bind(C, name='intrf_f__number__abs_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__abs', mod_interfaces_name_, private_abs)
  contains
    subroutine private_abs
      call intrf_f__ans_id(abs(nnn(id)), idout)
    end subroutine private_abs
  end subroutine intrf_f__number__abs

  !> Interface for Exp
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__exp (id, idout) bind(C, name='intrf_f__number__exp_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__exp', mod_interfaces_name_, private_exp)
  contains
    subroutine private_exp
      call intrf_f__ans_id(exp(NUMBERS_(id)), idout)
    end subroutine private_exp
  end subroutine intrf_f__number__exp

  !> Interface for Log
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__log (id, idout) bind(C, name='intrf_f__number__log_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__log', mod_interfaces_name_, private_log)
  contains
    subroutine private_log
      call intrf_f__ans_id(log(NUMBERS_(id)), idout)
    end subroutine private_log    
  end subroutine intrf_f__number__log

  !> Interface for Sin
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__sin (id, idout) bind(C, name='intrf_f__number__sin_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__sin', mod_interfaces_name_, private_sin)
  contains
    subroutine private_sin
      call intrf_f__ans_id(sin(NUMBERS_(id)), idout)
    end subroutine private_sin    
  end subroutine intrf_f__number__sin

  !> Interface for Cos
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__cos (id, idout) bind(C, name='intrf_f__number__cos_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__cos', mod_interfaces_name_, private_cos)
  contains
    subroutine private_cos
      call intrf_f__ans_id(cos(NUMBERS_(id)), idout)
    end subroutine private_cos    
  end subroutine intrf_f__number__cos

  !> Interface for Tan
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__tan (id, idout) bind(C, name='intrf_f__number__tan_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__tan', mod_interfaces_name_, private_tan)
  contains
    subroutine private_tan
      call intrf_f__ans_id(tan(NUMBERS_(id)), idout)
    end subroutine private_tan
  end subroutine intrf_f__number__tan

  !> Interface for Sinh
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__sinh (id, idout) bind(C, name='intrf_f__number__sinh_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__sinh', mod_interfaces_name_, private_sinh)
  contains
    subroutine private_sinh
      call intrf_f__ans_id(sinh(NUMBERS_(id)), idout)
    end subroutine private_sinh
  end subroutine intrf_f__number__sinh

  !> Interface for Cosh
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__cosh (id, idout) bind(C, name='intrf_f__number__cosh_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__cosh', mod_interfaces_name_, private_cosh)
  contains
    subroutine private_cosh
      call intrf_f__ans_id(cosh(NUMBERS_(id)), idout)
    end subroutine private_cosh
  end subroutine intrf_f__number__cosh

  !> Interface for Tanh
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__tanh (id, idout) bind(C, name='intrf_f__number__tanh_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__tanh', mod_interfaces_name_, private_tanh)
  contains
    subroutine private_tanh
      call intrf_f__ans_id(tanh(NUMBERS_(id)), idout)
    end subroutine private_tanh
  end subroutine intrf_f__number__tanh

  !> Interface for Sigmoid
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__sigmoid (id, idout) bind(C, name='intrf_f__number__sigmoid_')
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__sigmoid', mod_interfaces_name_, private_sigmoid)
  contains
    subroutine private_sigmoid
      call intrf_f__ans_id(number__sigmoid(NUMBERS_(id)), idout)
    end subroutine private_sigmoid
  end subroutine intrf_f__number__sigmoid

  !> Interface for ReLU
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__relu (id, idout) bind(C, name="intrf_f__number__relu_")
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__relu", mod_interfaces_name_, private_relu)
  contains
    subroutine private_relu
      call intrf_f__ans_id(number__relu(NUMBERS_(id)), idout)
    end subroutine private_relu
  end subroutine intrf_f__number__relu

  !> Interface for Swish
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__swish (id, idout) bind(C, name="intrf_f__number__swish_")
    implicit none
    integer(kind=c_int), intent(in) :: id
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__swish", mod_interfaces_name_, private_swish)
  contains
    subroutine private_swish
      call intrf_f__ans_id(number__swish(NUMBERS_(id)), idout)
    end subroutine private_swish
  end subroutine intrf_f__number__swish

  !> Interface for ELU
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__elu (id, ida, idout) bind(C, name="intrf_f__number__elu_")
    implicit none
    integer(kind=c_int), intent(in) :: id, ida
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__elu", mod_interfaces_name_, private_elu)
  contains
    subroutine private_elu
      call intrf_f__ans_id(number__elu(NUMBERS_(id), NUMBERS_(ida)), idout)
    end subroutine private_elu
  end subroutine intrf_f__number__elu

  !> Interface for Softmax
  !! @param[in] id c_int, id of the input 'number'
  !! @param[out] idout c_int, id of output 'number
  subroutine intrf_f__number__softmax (id, k, idout) bind(C, name='intrf_f__number__softmax_')
    implicit none
    integer(kind=c_int), intent(in) :: id, k
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__softmax', mod_interfaces_name_, private_softmax)
  contains
    subroutine private_softmax
      if (k > 0) then
         call intrf_f__ans_id(number__softmax(NUMBERS_(id), k), idout)
      else
         call intrf_f__ans_id(number__softmax(NUMBERS_(id)), idout)
      end if
    end subroutine private_softmax
  end subroutine intrf_f__number__softmax

  !> Interface for dp_gemm__0
  !! @param[in] transA c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB c_int, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] idAlpha c_int, id of the 'number' representing alpha
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idB c_int, if of 'number' representing B
  !! @param[in] idBeta c_int, if of 'number' representing beta
  !! @param[in] idC c_int, if of 'number' representing C
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dgemm0 (transA, transB, idAlpha, idA, idB, idBeta, &
       idC, idout) bind(C, name='intrf_f__number__dgemm0_')
    implicit none
    integer(kind=c_int), intent(in) :: transA, transB
    integer(kind=c_int), intent(in) :: idAlpha, idA, idB, idBeta, idC
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__dgemm0', mod_interfaces_name_, private_dgemm)
  contains
    subroutine private_dgemm
      call intrf_f__ans_id(number__dgemm( &
           transA, transB, &
           NUMBERS_(idAlpha), &
           NUMBERS_(idA), &
           NUMBERS_(idB), &
           NUMBERS_(idBeta), &
           NUMBERS_(idC)&
           ), idout)
    end subroutine private_dgemm
  end subroutine intrf_f__number__dgemm0

  !> Interface for dp_gemm__1
  !! @param[in] transA c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB c_int, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] idAlpha c_int, id of the 'number' representing alpha
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idB c_int, if of 'number' representing B
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dgemm1 (transA, transB, idAlpha, idA, idB, idout) &
       bind(C, name='intrf_f__number__dgemm1_')
    implicit none
    integer(kind=c_int), intent(in) :: transA, transB
    integer(kind=c_int), intent(in) :: idAlpha, idA, idB
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__dgemm1', mod_interfaces_name_, private_dgemm)
  contains
    subroutine private_dgemm
      call intrf_f__ans_id(number__dgemm(NUMBERS_(idAlpha), transA, transB, &
           NUMBERS_(idA), NUMBERS_(idB)), idout)
    end subroutine private_dgemm
  end subroutine intrf_f__number__dgemm1

  !> Interface for dp_gemm__0
  !! @param[in] transA c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB c_int, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idB c_int, if of 'number' representing B
  !! @param[in] idC c_int, if of 'number' representing C
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dgemm15 (transA, transB, idA, idB, idC, idout) &
       bind(c, name='intrf_f__number__dgemm15_')
    implicit none
    integer(kind=c_int), intent(in) :: transA, transB
    integer(kind=c_int), intent(in) :: idA, idB, idC
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__dgemm15', mod_interfaces_name_, private_dgemm)
  contains
    subroutine private_dgemm
      call intrf_f__ans_id( &
           number__dgemm(transA, transB, &
           NUMBERS_(idA), &
           NUMBERS_(idB), &
           NUMBERS_(idC)), &
           idout)
    end subroutine private_dgemm
  end subroutine intrf_f__number__dgemm15

  !> Interface for dp_gemm__0
  !! @param[in] transA c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] transB c_int, if > 0 op(B) = B**T else op(B) = B
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idB c_int, if of 'number' representing B
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dgemm2 (transA, transB, idA, idB, idout) &
       bind(C, name='intrf_f__number__dgemm2_')
    implicit none
    integer(kind=c_int), intent(in) :: transA, transB
    integer(kind=c_int), intent(in) :: idA, idB
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__dgemm2', mod_interfaces_name_, private_dgemm)
  contains
    subroutine private_dgemm
      call intrf_f__ans_id(number__dgemm(transA, transB, &
           NUMBERS_(idA), NUMBERS_(idB)), idout)
    end subroutine private_dgemm
  end subroutine intrf_f__number__dgemm2

  !> Interface for dp_gemv__1
  !! @param[in] trans c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] idAlpha c_int, id of the 'number' representing alpha
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idx c_int, if of 'number' representing x
  !! @param[in] idBeta c_int, if of 'number' representing beta
  !! @param[in] idy c_int, if of 'number' representing y
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dp_gemv__1 (trans, idAlpha, idA, idx, idBeta, idy, idout) &
       bind(C, name="intrf_f__number__dp_gemv__1_")
    implicit none
    integer(kind=c_int), intent(in) :: trans, idAlpha, idA, idx, idBeta, idy
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_gemv__1", mod_interfaces_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      call intrf_f__ans_id(number__dp_gemv(trans, NUMBERS_(idAlpha), NUMBERS_(idA), NUMBERS_(idx), &
           NUMBERS_(idBeta), NUMBERS_(idy)), idout)
    end subroutine private_dp_gemv
  end subroutine intrf_f__number__dp_gemv__1

  !> Interface for dp_gemv__2
  !! @param[in] trans c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] idAlpha c_int, id of the 'number' representing alpha
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idx c_int, if of 'number' representing x
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dp_gemv__2 (trans, idAlpha, idA, idx, idout) &
       bind(C, name="intrf_f__number__dp_gemv__2_")
    implicit none
    integer(kind=c_int), intent(in) :: trans, idAlpha, idA, idx
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_gemv__2", mod_interfaces_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      call intrf_f__ans_id(number__dp_gemv(NUMBERS_(idAlpha), trans, NUMBERS_(idA), NUMBERS_(idx)), idout)
    end subroutine private_dp_gemv
  end subroutine intrf_f__number__dp_gemv__2

  !> Interface for dp_gemv__3
  !! @param[in] trans c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idx c_int, if of 'number' representing x
  !! @param[in] idy c_int, if of 'number' representing y
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dp_gemv__3 (trans, idA, idx, idy, idout) &
       bind(C, name="intrf_f__number__dp_gemv__3_")
    implicit none
    integer(kind=c_int), intent(in) :: trans, idA, idx, idy
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_gemv__3", mod_interfaces_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      call intrf_f__ans_id(number__dp_gemv(trans, NUMBERS_(idA), NUMBERS_(idx), NUMBERS_(idy)), idout)
    end subroutine private_dp_gemv
  end subroutine intrf_f__number__dp_gemv__3

  !> Interface for dp_gemv__4
  !! @param[in] trans c_int, if > 0 op(A) = A**T else op(A) = A
  !! @param[in] idA c_int, if of 'number' representing A
  !! @param[in] idx c_int, if of 'number' representing x
  !! @param[out] idout c_int, id of the outpput 'number'
  subroutine intrf_f__number__dp_gemv__4 (trans, idA, idx, idout) &
       bind(C, name="intrf_f__number__dp_gemv__4_")
    implicit none
    integer(kind=c_int), intent(in) :: trans, idA, idx
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_gemv__4", mod_interfaces_name_, private_dp_gemv)
  contains
    subroutine private_dp_gemv
      call intrf_f__ans_id(number__dp_gemv(trans, NUMBERS_(idA), NUMBERS_(idx)), idout)
    end subroutine private_dp_gemv
  end subroutine intrf_f__number__dp_gemv__4

  !> Interface for dp_ger__1
  !! @param[in] idAlpah c_int, id of the 'number' representing alpha
  !! @param[in] idx c_int, id of the 'number' representing x
  !! @param[in] idy c_int, id of the 'number' representing y
  !! @param[in] idz c_int, id of the 'number' representing z
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__dp_ger__1 (idAlpha, idx, idy, idz, idout) &
       bind(C, name="intrf_f__number__dp_ger__1_")
    implicit none
    integer(kind=c_int), intent(in) :: idAlpha, idx, idy, idz
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_ger__1", mod_interfaces_name_, private_ger)
  contains
    subroutine private_ger
      call intrf_f__ans_id(number__dp_ger(NUMBERS_(idAlpha), NUMBERS_(idx), NUMBERS_(idy), &
           NUMBERS_(idz)), idout)
    end subroutine private_ger
  end subroutine intrf_f__number__dp_ger__1

  !> Interface for dp_ger__2
  !! @param[in] idx c_int, id of the 'number' representing x
  !! @param[in] idy c_int, id of the 'number' representing y
  !! @param[in] idz c_int, id of the 'number' representing z
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__dp_ger__2 (idx, idy, idz, idout) &
       bind(C, name="intrf_f__number__dp_ger__2_")
    implicit none
    integer(kind=c_int), intent(in) :: idx, idy, idz
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_ger__2", mod_interfaces_name_, private_ger)
  contains
    subroutine private_ger
      call intrf_f__ans_id(number__dp_ger(NUMBERS_(idx), NUMBERS_(idy), &
           NUMBERS_(idz)), idout)
    end subroutine private_ger
  end subroutine intrf_f__number__dp_ger__2

  !> Interface for dp_ger__3
  !! @param[in] idx c_int, id of the 'number' representing x
  !! @param[in] idy c_int, id of the 'number' representing y
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__dp_ger__3 (idx, idy, idout) &
       bind(C, name="intrf_f__number__dp_ger__3_")
    implicit none
    integer(kind=c_int), intent(in) :: idx, idy
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_ger__3", mod_interfaces_name_, private_ger)
  contains
    subroutine private_ger
      call intrf_f__ans_id(number__dp_ger(NUMBERS_(idx), NUMBERS_(idy)), idout)
    end subroutine private_ger
  end subroutine intrf_f__number__dp_ger__3

  !> Interface for dp_dot
  !! @param[in] idx c_int, id of the 'number' representing x
  !! @param[in] idy c_int, id of the 'number' representing y
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__dp_dot (idx, idy, idout) bind(C, name="intrf_f__number__dp_dot_")
    implicit none
    integer(kind=c_int), intent(in) :: idx, idy
    integer(kind=c_int), intent(out) :: idout
    call do_within("intrf_f__number__dp_dot", mod_interfaces_name_, private_dot)
  contains
    subroutine private_dot
      call intrf_f__ans_id(number__dp_dot(NUMBERS_(idx), NUMBERS_(idy)), idout)
    end subroutine private_dot
  end subroutine intrf_f__number__dp_dot
  
  !> Interface for invMat
  !! @param[in] idx c_int, id of the 'number' containing the matrix to invert
  !! @param[out] idout c_int, id of the 'number' containing the inverted matrix
  subroutine intrf_f__number__invMat (idx, idout) bind(C, name='intrf_f__number__invMat_')
    implicit none
    integer(kind=c_int), intent(in) :: idx
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__invMat', mod_interfaces_name_, private_inv)
  contains
    subroutine private_inv
      call intrf_f__ans_id(number__invMat(NUMBERS_(idx)), idout)
    end subroutine private_inv    
  end subroutine intrf_f__number__invMat

  !> Interface for Sum
  !! @param[in] idx c_int, id of the input 'number'
  !! @param[out] idout c_int, id of the reduced 'number'
  subroutine intrf_f__number__sum (idx, k, idout) bind(C, name='intrf_f__number__sum_')
    implicit none
    integer(kind=c_int), intent(in) :: idx, k
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__sum', mod_interfaces_name_, private_sum)
  contains
    subroutine private_sum
      if (k > 0) then
         call intrf_f__ans_id(number__sum(NUMBERS_(idx), k), idout)
      else
         call intrf_f__ans_id(number__sum(NUMBERS_(idx)), idout)
      end if
    end subroutine private_sum
  end subroutine intrf_f__number__sum

  subroutine intrf_f__number__product (idx, k, idout) bind(C, name='intrf_f__number__product_')
    implicit none
    integer(kind=c_int), intent(in) :: idx, k
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__product', mod_interfaces_name_, private_product)
  contains
    subroutine private_product
      if (k > 0) then
         call intrf_f__ans_id(number__product(nnn(idx), k), idout)
      else
         call intrf_f__ans_id(number__product(nnn(idx)), idout)
      end if
    end subroutine private_product
  end subroutine intrf_f__number__product
  
  !> Interface for Sum Of Squares
  !! @param[in] idx c_int, id of the input 'number'
  !! @param[out] idout c_int, id of the reduced 'number'
  subroutine intrf_f__number__ssq (idx, idout) bind(C, name='intrf_f__number__ssq_')
    implicit none
    integer(kind=c_int), intent(in) :: idx
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ssq', mod_interfaces_name_, private_ssq)
  contains
    subroutine private_ssq
      call intrf_f__ans_id(number__ssq(NUMBERS_(idx)), idout)
    end subroutine private_ssq
  end subroutine intrf_f__number__ssq

  !> Interface for ldexp
  !! @param[in] idy c_int, id of the observation 'number'
  !! @param[in] idlam c_int, id of the rate parameter 'number'
  !! @param[out] idout c_int, id of the log-density 'number'
  subroutine intrf_f__number__ldexp (idy, idlam, idout) bind(C, name='intrf_f__number__ldexp_')
    implicit none
    integer(kind=c_int), intent(in) :: idy, idlam
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ldexp', mod_interfaces_name_, private_ldexp)
  contains
    subroutine private_ldexp
      call intrf_f__ans_id(number__ldexp(nnn(idy), nnn(idlam)), idout)
    end subroutine private_ldexp
  end subroutine intrf_f__number__ldexp

  !> Interface for ldlaplace
  !! @param[in] idy c_int, id of the observation 'number'
  !! @param[in] idmu c_int, mean 'number'
  !! @param[in] idlam c_int, id of the rate parameter 'number'
  !! @param[out] idout c_int, id of the log-density 'number'
  subroutine intrf_f__number__ldlaplace (idy, idmu, idlam, idout) &
       bind(C, name='intrf_f__number__ldlaplace_')
    implicit none
    integer(kind=c_int), intent(in) :: idy, idmu, idlam
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ldlaplace', mod_interfaces_name_, private_ldlaplace)
  contains
    subroutine private_ldlaplace
      call intrf_f__ans_id(number__ldlaplace(nnn(idy), nnn(idmu), nnn(idlam)), idout)
    end subroutine private_ldlaplace
  end subroutine intrf_f__number__ldlaplace

  !> Interface for ldexp
  !! @param[in] idy c_int, id of the observation 'number'
  !! @param[in] ida1 c_int, id of the shape 1 'number'
  !! @param[in] ida2 c_int, id of the shape 2 'number'
  !! @param[out] idout c_int, id of the log-density 'number'
  subroutine intrf_f__number__ldbeta (idy, ida1, ida2, idout) &
       bind(C, name='intrf_f__number__ldbeta_')
    implicit none
    integer(kind=c_int), intent(in) :: idy, ida1, ida2
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ldbeta', mod_interfaces_name_, private_ldbeta)
  contains
    subroutine private_ldbeta
      call intrf_f__ans_id(number__ldbeta(nnn(idy), nnn(ida1), nnn(ida2)), idout)
    end subroutine private_ldbeta
  end subroutine intrf_f__number__ldbeta

  !> Interface for ldgamma
  !! @param[in] idy c_int, id of the observation 'number'
  !! @param[in] ida c_int, id of the shape parameter 'number'
  !! @param[in] idb c_int, id of the rate parameter 'number'
  !! @param[out] idout c_int, id of the log-density 'number'
  subroutine intrf_f__number__ldgamma (idy, ida, idb, idout) &
       bind(C, name='intrf_f__number__ldgamma_')
    implicit none
    integer(kind=c_int), intent(in) :: idy, ida, idb
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ldgamma', mod_interfaces_name_, private_ldgamma)
  contains
    subroutine private_ldgamma
      call intrf_f__ans_id(number__ldgamma(nnn(idy), nnn(ida), nnn(idb)), idout)
    end subroutine private_ldgamma
  end subroutine intrf_f__number__ldgamma

  !> Interface for ldnorm
  !! @param[in] idy c_int, id of the observation 'number'
  !! @param[in] idmu c_int, id of the mean 'number'
  !! @param[in] ids c_int, id of the standard deviation 'number'
  !! @param[out] idout c_int, id of the log-density 'number'
  subroutine intrf_f__number__ldnorm (idy, idmu, ids, idout) &
       bind(C, name='intrf_f__number__ldnorm_')
    implicit none
    integer(kind=c_int), intent(in) :: idy, idmu, ids
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ldnorm', mod_interfaces_name_, private_ldnorm)
  contains
    subroutine private_ldnorm
      call intrf_f__ans_id(number__ldnorm(nnn(idy), nnn(idmu), nnn(ids)), idout)
    end subroutine private_ldnorm
  end subroutine intrf_f__number__ldnorm

  !> Interface for ldmvnorm
  !! @param[in] idy c_int, id of the observation 'number'
  !! @param[in] idmu c_int, id of the mean 'number'
  !! @param[in] idE c_int, id of the covariance matrix 'number'
  !! @param[out] idout c_int, id of the log-density 'number'
  subroutine intrf_f__number__ldmvnorm__1 (idy, idmu, idE, idout) &
       bind(C, name='intrf_f__number__ldmvnorm__1_')
    implicit none
    integer(kind=c_int), intent(in) :: idy, idmu, idE
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ldmvnorm__1', mod_interfaces_name_, private_do)
  contains
    subroutine private_do
      call intrf_f__ans_id(number__ldmvnorm__1(nnn(idy), nnn(idmu), nnn(idE)), idout)
    end subroutine private_do
  end subroutine intrf_f__number__ldmvnorm__1

  !> Interfate for lkh_norm
  !! @param[in] idy c_int, id of the observation 'number'
  !! @param[in] idmu c_int, id of the mea 'number'
  !! @param[in] ids c_int, id of the standard deviation 'number'
  !! @param[in] idw c_int, id of the weight 'number' (if < 1 no weighting)
  !! @param[out] idout c_int, id of the log-likelihood
  subroutine intrf_f__number__lkh_norm (idy, idmu, ids, idw, idout) &
       bind(C, name='intrf_f__number__lkh_norm_')
    implicit none
    integer(kind=c_int), intent(in) :: idy, idmu, ids, idw
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__lkh_norm', mod_interfaces_name_, private_lkh)
  contains
    subroutine private_lkh
      if (idw > 0) then
         call intrf_f__ans_id(number__lkh_norm(nnn(idy), nnn(idmu), nnn(ids), nnn(idw)), idout)
      else
         call intrf_f__ans_id(number__lkh_norm(nnn(idy), nnn(idmu), nnn(ids)), idout)
      end if
    end subroutine private_lkh
  end subroutine intrf_f__number__lkh_norm

  !> Interface for ksqexp
  !! @param[in] idx1 c_int, id of the first feature vector
  !! @param[in] idx2 c_int, id of the second feature vector
  !! @param[in] ids c_int, id of the amplitude parameter 'number'
  !! @param[in] idb c_int, id of the rate parameter 'number'
  !! @param[out] idout c_int, id of the output 'number'
  subroutine intrf_f__number__ksqexp (idx1, idx2, ida, idb, idout) &
       bind(C, name='intrf_f__number__ksqexp_')
    implicit none
    integer(kind=c_int), intent(in) :: idx1, idx2, ida, idb
    integer(kind=c_int), intent(out) :: idout
    call do_within('intrf_f__number__ksqexp', mod_interfaces_name_, private_ksqexp)
  contains
    subroutine private_ksqexp
      call intrf_f__ans_id(number__ksqexp(nnn(idx1), nnn(idx2), nnn(ida), nnn(idb)), idout)
    end subroutine private_ksqexp
  end subroutine intrf_f__number__ksqexp

  ! !> Interface for amoeba_opt
  ! !! @param[in] g c_int, 'graph' id
  ! !! @param[in] nin c_int, number of parameter 'numbers'
  ! !! @param[in] xin c_int, ids of the parameter 'numbers'
  ! !! @param[in] xout c_int, id of the 'graph' output 'number'
  ! !! @param[out] iter c_int, number of performed iterations
  ! !! @param[in] ftol c_double, fractionsl tolerance for convergence
  ! !! @param[in] itmax c_int, maximum number of iterations
  ! !! @param[in] ndim c_int, total input dimension
  ! !! @param[in] sx c_double, characteristic length for initialising the simplex
  ! subroutine intrf_f__amoeba_opt (g, nin, xin, xout, iter, ftol, itmax, ndim, sx) &
  !      bind(C, name='intrf_f__amoeba_opt_')
  !   implicit none
  !   integer(kind=c_int) :: g, nin, xin(nin), xout, itmax, ndim
  !   real(kind=c_double) :: ftol, sx(ndim)
  !   integer(kind=c_int), intent(out) :: iter
  !   call do_within('intrf_f__amoeba__opt', mod_interfaces_name_, private_opt)
  ! contains
  !   subroutine private_opt
  !     call amoeba_opt(g, xin, xout, iter, ftol, itmax, sx)
  !   end subroutine private_opt
  ! end subroutine intrf_f__amoeba_opt

  ! !> Interface for brent_op
  ! !! @param[in] g c_int, 'graph' id
  ! !! @param[in] xin c_int, id of the input 'number' (scalar)
  ! !! @param[in] xout c_int, id of the 'graph' outout 'number'
  ! !! @param[in] tol c_double, tolerace for convergence
  ! !! @param[in] lw c_double, lower paramter bound
  ! !! @param[in] up c_double, upper parameter bound
  ! !! @param[in] dx c_int, if > 0 a line-search using derivatives if adopted else derivative-free line-search is used
  ! subroutine intrf_f__brent_opt (g, xin, xout, tol, lw, up, dx) bind(C, name='intrf_f__brent_opt_')
  !   implicit none
  !   integer(kind=c_int), intent(in) :: g, xin, xout, dx
  !   real(kind=c_double), intent(in) :: tol, lw, up
  !   call do_within('intrf_f__brent_opt', mod_interfaces_name_, private_opt)
  ! contains
  !   subroutine private_opt
  !     call brent_opt(g, xin, xout, tol, lw, up, dx > 0)
  !   end subroutine private_opt
  ! end subroutine intrf_f__brent_opt

  ! !> Interface for frprmn_opt
  ! !! @param[in] gi c_int, 'graph' id
  ! !! @param[in] nin c_int, number of inoput 'numbers'
  ! !! @param[in] xin c_int, input 'number' ids
  ! !! @param[in] xout c_int, id of the 'graph' output 'number'
  ! !! @param[out] iter c_int, number of performed iterations
  ! !! @param[in] ftol c_double, fractional toelrance for convergence
  ! !! @param[in] itmax c_int, maximum number of allowed iterations
  ! !! @param[in] dx c_int, if > 0 a line-search using derivatives if adopted else derivative-free line-search is used
  ! !! @param[in] ng c_int, normalisation to apply to the gradient
  ! subroutine intrf_f__frprmn_opt (gi, nin, xin, xout, iter, ftol, itmax, dx, ng) bind(C, name='intrf_f__frprmn_opt_')
  !   implicit none
  !   integer(kind=c_int), intent(in) :: gi, nin, xin(nin), xout, itmax, ng, dx
  !   integer(kind=c_int), intent(out) :: iter
  !   real(kind=c_double), intent(in) :: ftol
  !   call do_within('intrf_f__frprmn_opt', mod_interfaces_name_, private_op)
  ! contains
  !   subroutine private_op
  !     call frprmn_opt(gi, xin, xout, iter, ftol, itmax, dx > 0, ng)
  !   end subroutine private_op
  ! end subroutine intrf_f__frprmn_opt

  ! !> Interface for frprmn_opt
  ! !! @param[in] gi c_int, 'graph' id
  ! !! @param[in] nin c_int, number of inoput 'numbers'
  ! !! @param[in] xin c_int, input 'number' ids
  ! !! @param[in] xout c_int, id of the 'graph' output 'number'
  ! !! @param[out] iter c_int, number of performed iterations
  ! !! @param[in] gtol c_double, fractional toelrance for convergence
  ! !! @param[in] itmax c_int, maximum number of allowed iterations
  ! !! @param[in] ng c_int, normalisation to apply to the gradient
  ! subroutine intrf_f__dfpmin_op (gi, nin, xin, xout, gtol, iter, itmax, ng) bind(C, name='intrf_f__dfpmin_opt_')
  !   implicit none
  !   integer(kind=c_int), intent(in) :: gi, nin, xin(nin), xout, itmax, ng
  !   integer(kind=c_int), intent(out) :: iter
  !   real(kind=c_double), intent(in) :: gtol
  !   call do_within('intrf_f__dfpmin_opt', mod_interfaces_name_, private_opt)
  ! contains
  !   subroutine private_opt
  !     call dfpmin_opt(gi, xin, xout, gtol, iter, itmax, ng)
  !   end subroutine private_opt
  ! end subroutine intrf_f__dfpmin_op

  !> Allocates the GOPTS_ register
  !! @param[in] n integer, array size
  subroutine intrf_f__allocate_gopts (n) bind(C, name='intrf_f__allocate_gopts_')
    implicit none
    integer(c_int), intent(in) :: n
    call do_within('intrf_f__allocate_gopts', mod_interfaces_name_, private_allocate)
  contains
    subroutine private_allocate
      call allocate_gopts(n)
    end subroutine private_allocate
  end subroutine intrf_f__allocate_gopts

  !> Deallocates the GOPTS_ register
  subroutine intrf_f__deallocate_gopts () bind(C, name='intrf_f__deallocate_gopts_')
    implicit none
    call do_within('intrf_f__deallocate_gopts', mod_interfaces_name_, private_deallocate)
  contains
    subroutine private_deallocate
      call deallocate_gopts()
    end subroutine private_deallocate
  end subroutine intrf_f__deallocate_gopts

  !> Append a sgd optimiser to the GOPTS_ register
  !! @param[out] i integer, 'opt' index
  !! @param[in] nin integer, size of the parameter vector
  !! @param[in] xin intger(:), parameter vector containing the indexes of the parameter 'numbers' 
  subroutine intrf_f__sgd__append (i, nin, xin) bind(C, name='intrf_f__sgd__append_')
    implicit none
    integer(kind=c_int), intent(out) :: i
    integer(kind=c_int), intent(in) :: nin, xin(nin)
    call do_within('intrf_f__sgd__append', mod_interfaces_name_, private_append)
  contains
    subroutine private_append
      call sgd__append(i, xin)
    end subroutine private_append
  end subroutine intrf_f__sgd__append

  !> Append a sgdwm optimiser to the GOPTS_ register
  !! @param[out] i integer, 'opt' index
  !! @param[in] nin integer, size of the parameter vector
  !! @param[in] xin intger(:), parameter vector containing the indexes of the parameter 'numbers' 
  subroutine intrf_f__sgdwm__append (i, nin, xin) bind(C, name='intrf_f__sgdwm__append_')
    implicit none
    integer(kind=c_int), intent(out) :: i
    integer(kind=c_int), intent(in) :: nin, xin(nin)
    call do_within('intrf_f__sgdwm__append', mod_interfaces_name_, private_append)
  contains
    subroutine private_append
      call sgdwm__append(i, xin)
    end subroutine private_append
  end subroutine intrf_f__sgdwm__append

  !> Append an Adam optimiser to the GOPTS_ register
  !! @param[out] i integer, 'opt' index
  !! @param[in] nin integer, size of the parameter vector
  !! @param[in] xin intger(:), parameter vector containing the indexes of the parameter 'numbers' 
  subroutine intrf_f__adam__append (i, nin, xin) bind(C, name='intrf_f__adam__append_')
    implicit none
    integer(kind=c_int), intent(out) :: i
    integer(kind=c_int), intent(in) :: nin, xin(nin)
    call do_within('intrf_f__adam__append', mod_interfaces_name_, private_append)
  contains
    subroutine private_append
      call adam__append(i, xin)
    end subroutine private_append
  end subroutine intrf_f__adam__append

  !> Pops (removes) an optimiser fromt he GOPTS_ register
  !! @param[in] i integer, 'opt' index
  subroutine intrf_f__gopt__pop (i) bind(C, name='intrf_f__gopt__pop_')
    implicit none
    integer(kind=c_int), intent(in) :: i
    call do_within('intrf_f__gopt__pop', mod_interfaces_name_, private_pop)
  contains
    subroutine private_pop
      call gopt__pop(i)
    end subroutine private_pop
  end subroutine intrf_f__gopt__pop

  !> Performs niter iterations of a sgd optimiser step
  !! @param[in] xoi integer, 'opt' index
  !! @param[in] gi integer, 'graph' index
  !! @param[in] xout integer, output 'number' index
  !! @param[in] lr dpuble precision, lerning rate
  !! @param[in] niter integer, number of iterations to perform
  subroutine intrf_f__sgd__step (xoi, gi, xout, lr, niter) bind(C, name='intrf_f__sgd__step_')
    implicit none
    integer(kind=c_int), intent(in) :: xoi, gi, xout, niter
    real(kind=c_double), intent(in) :: lr
    call do_within('intrf_f__sgd__step', mod_interfaces_name_, private_step)
  contains
    subroutine private_step
      type(number), pointer :: xxout 
      real(kind=dp_) :: ftrain
      call get_number(xout, xxout)
      call assert(mtrnk(xxout) == 0, err_generic_, 'NUMBERS_(xout) rank /= 0')
      call assert(has_dx(xxout), err_generic_, 'xout has no dx')
      if (err_free()) call sgd_step(opi(xoi),  ggg(gi), xxout, lr, niter, ftrain)
    end subroutine private_step
  end subroutine intrf_f__sgd__step

  !> Performs niter iterations of a sgdwm optimiser step
  !! @param[in] xoi integer, 'opt' index
  !! @param[in] gi integer, 'graph' index
  !! @param[in] xout integer, output 'number' index
  !! @param[in] lr dpouble precision, lerning rate
  !! @param[in] alpha double precision,  momentum parameter
  !! @param[in] niter integer, number of iterations to perform
  subroutine intrf_f__sgdwm__step (xoi, gi, xout, lr, alpha, niter) bind(C, name='intrf_f__sgdwm__step_')
    implicit none
    integer(kind=c_int), intent(in) :: xoi, gi, xout, niter
    real(kind=c_double), intent(in) :: lr, alpha
    call do_within('intrf_f__sgd__step', mod_interfaces_name_, private_step)
  contains
    subroutine private_step
      type(number), pointer :: xxout 
      real(kind=dp_) :: ftrain
      call get_number(xout, xxout)
      call assert(mtrnk(xxout) == 0, err_generic_, 'NUMBERS_(xout) rank /= 0')
      call assert(has_dx(xxout), err_generic_, 'xout has no dx')
      if (err_free()) call sgdwm_step(opi(xoi), ggg(gi), xxout, lr, alpha, niter, ftrain)
    end subroutine private_step
  end subroutine intrf_f__sgdwm__step

  !> Performs niter iterations of an Adam optimiser step
  !! @param[in] xoi integer, 'opt' index
  !! @param[in] gi integer, 'graph' index
  !! @param[in] xout integer, output 'number' index
  !! @param[in] lr double precision, lerning rate
  !! @param[in] beta1 double precision, first order momentum parameter
  !! @param[in] beta2 double precision, second order momentum parameter
  !! @param[in] niter integer, number of iterations to perform
  subroutine intrf_f__adam__step (xoi, gi, xout, lr, beta1, beta2, niter) &
       bind(C, name='intrf_f__adam__step_')
    implicit none
    integer(kind=c_int), intent(in) :: xoi,gi, xout,  niter
    real(kind=c_double), intent(in) :: lr, beta1, beta2
    call do_within('intrf_f__sgd__step', mod_interfaces_name_, private_step)
  contains
    subroutine private_step
      type(number), pointer :: xxout
      real(kind=dp_) :: ftrain
      call get_number(xout, xxout)
      call assert(mtrnk(xxout) == 0, err_generic_, 'NUMBERS_(xout) rank /= 0')
      call assert(has_dx(xxout), err_generic_, 'xout has no dx')
      if (err_free()) call adam_step(opi(xoi), ggg(gi), xxout, lr, beta1, beta2, niter, ftrain)
    end subroutine private_step
  end subroutine intrf_f__adam__step
  
end module interfaces
