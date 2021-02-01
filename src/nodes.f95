module nodes
  use env
  use types
  use registers, only: &
       NUMBERS_, &       
       NODES_, &
       NODEGRPHS_, &
       NODEi_, &
       GRAPHS_, &
       GRAPHi_, &
       GRAPH_
  use errwarn
  use utils
  use nodes_utils
  use numbers_utils
  use node_operators

  implicit none

  private

  public &
       nodes__allocate, &
       deallocate_nodes, &
       node__append, &
       node__pop, &
       graphs__allocate, &
       deallocate_graphs, &
       graph__open, &
       graph__close, &
       ggg, &
       graph__append, &
       graph__pop, &
       graph__gc, &
       graph__op, &
       graph__fw, &
       graph__bw_zero, &
       graph__bw    

  character(len=*), parameter :: mod_nodes_name_ = 'nodes'

  !> Appends a new node to the NODES_ register.
  !! @author Filippo Monari
  !! @param[in] op integer, operator index
  !! @param[in] x1,x2,... 'number', inputs
  !! @param[in] ans 'number' output
  !! @param[in] flg integer(:), flags to be passed to the operator
  interface node__append
     module procedure node__append__1_no
     module procedure node__append__1
     module procedure node__append__2
     module procedure node__append__3
     module procedure node__append__4
     module procedure node__append__5
  end interface node__append

contains

  !> Gets the next free slot in the NODES_ register,
  !! and stores it into the stack NODEi_.
  !! @author Filippo Monari
  subroutine node__next ()
    implicit none
    call do_safe_within('node__next', mod_nodes_name_, private_do)
  contains
    subroutine private_do
      call assert(NODEi_ == 0, err_generic_, 'stack NODEi_ not empty.')
      call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
      call err_safe(private_next)
    end subroutine private_do
    subroutine private_next
      integer :: info
      info = 1 
      NODEi_ = 0
      do while (NODEi_ <= size(NODES_))
         NODEi_ = NODEi_ + 1
         if (is_deallocated(NODES_(NODEi_))) then
            info = 0
            exit
         end if
      end do
      call assert(info == 0, err_generic_, 'NODES_ register is full.')
    end subroutine private_next
  end subroutine node__next

  !> Gets the next free slot in the GRAPHS_ register and
  !! stores it into GRAPHi_,
  !! the current graph index
  !! @author Filippo Monari
  subroutine graph__next ()
    implicit none
    call do_safe_within('graph__next', mod_nodes_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(GRAPHS_), err_notAlloc_, 'GRAPHS_')
      call err_safe(private_next)
    end subroutine private_do
    subroutine private_next
      integer :: info
      info = 1
      GRAPHi_ = 0
      do while (GRAPHi_ <= size(GRAPHS_))
         GRAPHi_ = GRAPHi_ + 1
         if (is_deallocated(GRAPHS_(GRAPHi_))) then
            info = 0
            exit
         end if
      end do
      call assert(info == 0, err_generic_, 'GRAPHS_ register is full.')
    end subroutine private_next
  end subroutine graph__next

  !> Allocates the 'NODES_' and 'NODEGRAPHS_' registers.
  !! @author Filippo Monari
  !! @param[in] n integer, register dimension
  !! @todo rename allocate_nodes
  subroutine nodes__allocate (n)
    implicit none
    integer, intent(in) :: n
    call do_safe_within('node__allocate', mod_nodes_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      info = 0
      call assert(.not. allocated(NODES_), err_alreadyAlloc_, 'NODES_')
      call assert(.not. allocated(NODEGRPHS_), err_alreadyAlloc_, 'NODEGRPHS_')
      if (err_free()) allocate(NODES_(n), stat=info)
      call assert(info == 0, err_alloc_, 'NODES_')
      call alloc(NODEGRPHS_, n, 'NODEGRPHS_')
    end subroutine private_allocate
  end subroutine nodes__allocate

  !> Deallocates the 'NODES_' and 'NODEGRAPHS_' registers
  !! @author Filippo Monari
  subroutine deallocate_nodes ()
    implicit none
    call do_safe_within("deallocate_nodes", mod_nodes_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
      call assert(allocated(NODEGRPHS_), err_notAlloc_, 'NODEGRPHS_')
      call err_safe(private_deallocate)
    end subroutine private_do
    subroutine private_deallocate
      integer :: i, info
      info = 0
      do i = 1, size(NODES_)
         if (is_allocated(NODES_(i))) call node__deallocate(NODES_(i))
         if (.not. err_free()) exit
      end do
      if (err_free()) deallocate(NODES_, stat=info)
      call assert(info == 0, err_dealloc_, 'NODES_')
      call dealloc(NODEGRPHS_, 'NODEGRPHS_')
    end subroutine private_deallocate
  end subroutine deallocate_nodes

  !> Appends 'nodes' with one input.
  !! @param[in] op integer, operator index
  !! @param[in] x1 'number', input
  !! @param[in] ans 'number', output
  !! @param[in] flg integer, flags
  subroutine node__append__1 (op, x1, ans, flg)
    implicit none
    integer, intent(in) :: op, flg(:)
    type(number), intent(in) :: x1
    type(number), intent(inout) :: ans
    call do_safe_within('node__append__1', mod_nodes_name_, private_append)
  contains
    subroutine private_append
      if (graphi_is_set()) then
         call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
         call node__next()
         call node__allocate(NODES_(get_nodei()), op, x1, ans, flg)
      else
         call number__set_nd(ans, -1)
      end if
    end subroutine private_append
  end subroutine node__append__1

  !> Appends 'nodes' with one input and no output.
  !! @param[in] op integer, operator index
  !! @param[in] x1 'number', input
  !! @param[in] flg integer, flags
  subroutine node__append__1_no (op, x1, flg)
    implicit none
    integer, intent(in) :: op, flg(:)
    type(number), intent(in) :: x1
    call do_safe_within('node__append__1_no', mod_nodes_name_, private_append)
  contains
    subroutine private_append
      if (graphi_is_set()) then
         call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
         call node__next()
         call node__allocate(NODES_(get_nodei()), op, x1, flg)
      end if
    end subroutine private_append
  end subroutine node__append__1_no
  
  !> Appends 'nodes' with two input.
  !! @param[in] op integer, operator index
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[in] ans 'number', output
  !! @param[in] flg integer, flags
  subroutine node__append__2 (op, x1, x2, ans, flg)
    implicit none
    integer, intent(in) :: op, flg(:)
    type(number), intent(in) :: x1, x2
    type(number), intent(inout) :: ans
    call do_safe_within('node__append__2', mod_nodes_name_, private_append)
  contains
    subroutine private_append
      if (graphi_is_set()) then
         call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
         call node__next()
         call node__allocate(NODES_(get_nodei()), op, x1, x2, ans, flg)
      else
         call number__set_nd(ans, -1)
      end if
    end subroutine private_append
  end subroutine node__append__2
  
  !> Appends 'nodes' with three input.
  !! @param[in] op integer, operator index
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[in] x3 'number', input
  !! @param[in] ans 'number', output
  !! @param[in] flg integer, flags
  subroutine node__append__3 (op, x1, x2, x3, ans, flg)
    implicit none
    integer, intent(in) :: op, flg(:)
    type(number), intent(in) :: x1, x2, x3
    type(number), intent(inout) :: ans
    call do_safe_within('node__append__3', mod_nodes_name_, private_append)
  contains
    subroutine private_append
      if (graphi_is_set()) then
         call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
         call node__next()
         call node__allocate(NODES_(get_nodei()), op, x1, x2, x3, ans, flg)
      else
         call number__set_nd(ans, -1)
      end if
    end subroutine private_append
  end subroutine node__append__3

  !> Appends 'nodes' with four input.
  !! @param[in] op integer, operator index
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[in] x3 'number', input
  !! @param[in] x4 'number', input
  !! @param[in] ans 'number', output
  !! @param[in] flg integer, flags
  subroutine node__append__4 (op, x1, x2, x3, x4, ans, flg)
    implicit none
    integer, intent(in) :: op, flg(:)
    type(number), intent(in) :: x1, x2, x3, x4
    type(number), intent(inout) :: ans
    call do_safe_within('node__append__3', mod_nodes_name_, private_append)
  contains
    subroutine private_append
      if (graphi_is_set()) then
         call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
         call node__next()
         call node__allocate(NODES_(get_nodei()), op, x1, x2, x3, x4, ans, flg)
      else
         call number__set_nd(ans, -1)
      end if
    end subroutine private_append
  end subroutine node__append__4
  
  !> Appends 'nodes' with five input.
  !! @param[in] op integer, operator index
  !! @param[in] x1 'number', input
  !! @param[in] x2 'number', input
  !! @param[in] x3 'number', input
  !! @param[in] x4 'number', input
  !! @param[in] x5 'number', input
  !! @param[in] ans 'number', output
  !! @param[in] flg integer, flags
  subroutine node__append__5 (op, x1, x2, x3, x4, x5, ans, flg)
    implicit none
    integer, intent(in) :: op, flg(:)
    class(number), intent(in) :: x1, x2, x3, x4, x5
    class(number), intent(inout) :: ans
    call do_safe_within('node__append__5', mod_nodes_name_, private_append)
  contains
    subroutine private_append
      if (graphi_is_set()) then
         call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
         call node__next()
         call node__allocate(NODES_(get_nodei()), op, x1, x2, x3, x4, x5, ans, flg)
      else
         call number__set_nd(ans, -1)
      end if
    end subroutine private_append
  end subroutine node__append__5

  !> Pops (removes) a 'node' from the 'NODES_' register.
  !! Updates all the other registers accordingly.
  !! @details Nodes are poped through their id. This
  !! has been deemed simple, since this function has not to be called,
  !! directly, but by the corresponding routined popping 'numbers'
  !! and graphs.
  !! @author Filippo Monari
  !! @param[in] i integer, 'node' index (position in 'NODES_')
  subroutine node__pop (i)
    implicit none
    integer, intent(in) :: i
    call do_safe_within("node__pop", mod_nodes_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(NODES_), err_notAlloc_, 'NODES_')
      if (err_free()) call assert(0 < i .and. i <= size(NODES_), err_oorng_, 'i')
      if (err_free()) call assert(is_allocated(NODES_(i)), err_notAlloc_, 'x')
      call err_safe(private_pop)
    end subroutine private_do
    subroutine private_pop
      call node__update_inlock(NODES_(i), '-') !< updates the INLOCK_ register
      call node__reset_number(NODES_(i))       !< updates the NUMNDS_ register
      call node__pack_graph(NODES_(i))         !< remove the node index from the 'graph'
      call node__deallocate(NODES_(i))
    end subroutine private_pop
  end subroutine node__pop

  !> Sets the current graph index GRAPHi_.
  !! @author Filippo Monari
  !! @param[in] i integer, optional, if present GRAPHi_ = i else GRAPHi_ = next free slot
  subroutine graph__open (i)
    implicit none
    integer, intent(in), optional :: i
    call do_safe_within('with_graph', mod_nodes_name_, private_do)
  contains
    subroutine private_do
      if (present(i)) then
         call assert(allocated(GRAPHS_), err_notAlloc_, 'GRAPHS_')
         call assert(0 > i .and. i < size(GRAPHS_), err_oorng_, 'GRAPH_(i)')
         call err_safe(private_open_existing)
      else
         call graph__next()
         if (err_free()) then
            GRAPH_ => GRAPHS_(GRAPHi_)
            call alloc(GRAPH_%nodes, [integer::], "GRAPH_%nodes")
         end if
      end if
    end subroutine private_do
    subroutine private_open_existing
      GRAPH_ => GRAPHS_(i)
      GRAPHi_ = i
    end subroutine private_open_existing
  end subroutine graph__open

  !> Reset 'GRAPHi_' and 'GRPAH_', so that no 'graph' is on the stack anymore
  !! @author Filippo Monari
  subroutine graph__close ()
    implicit none
    call do_safe_within("graph__close", mod_nodes_name_, private_close)
  contains
    subroutine private_close
      call assert(graphi_is_set(), err_generic_, "no graph is open.")
      if (err_free()) then
         GRAPHi_ = 0
         nullify(GRAPH_)
      end if
    end subroutine private_close
  end subroutine graph__close
  
  !> Appends the node on the stack to the current graph and flushes it.
  !! @author Filippo Monari
  subroutine graph__append ()
    implicit none
    call do_safe_within('graph__append', mod_nodes_name_, private_append)
  contains
    subroutine private_append
      if (graphi_is_set()) then
         call assert(associated(GRAPH_), err_notAssoc_, 'GRAPH_')
         call append_to_array(GRAPH_%nodes, flush_nodei(), 'GRAPH_%ndes')
      end if
    end subroutine private_append
  end subroutine graph__append

  !> Pops (removes) a 'graph' from 'GRAPHS_'. All its 'nodes' are removed as well
  !! from 'NODES'. The registers are update accordingly.
  !! @author Filippo Monari
  !! @param[in] gi integer, 'graph' index
  subroutine graph__pop (gi)
    implicit none
    integer, intent(in) :: gi
    call do_safe_within('graph__pop', mod_nodes_name_, private_do)
  contains
    subroutine private_do
      if (graphi_is_set()) then
         call assert(gi /= get_graphi(), err_generic_, 'GRAPHS_(gi) is open.')
      end if
      call err_safe(private_pop)
    end subroutine private_do
    recursive subroutine private_pop
      integer :: nd
      do while(mtsz(GRAPHS_(gi)) > 0)
         nd = GRAPHS_(gi)%nodes(1)
         call node__pop(nd)
         if (.not. err_free()) exit
      end do
      call dealloc(GRAPHS_(gi)%nodes, 'GRAPHS_(gi)%nodes')
    end subroutine private_pop
  end subroutine graph__pop

  !> Garbage collector for Graphs. An empty 'graph' in 'GRAPHS_'
  !! is considered garbage and is deallocated.
  subroutine graph__gc ()
    implicit none
    call do_safe_within("graph__gc", mod_nodes_name_, private_gc)
  contains
    subroutine private_gc
      integer :: i
      do i = 1, size(GRAPHS_)
         if (is_empty(GrAPHS_(i))) call dealloc(GRAPHS_(i)%nodes, 'GRAPHS_(i)%nodes')
      end do
    end subroutine private_gc
  end subroutine graph__gc  

  !> Check that an integer index is a valid graph id.
  !! @param[in] i integer, index
  function ggg (i) result(ans)
    implicit none
    integer, intent(in) :: i
    integer :: ans
    call do_safe_within('ggg', mod_nodes_name_, private_ggg)
  contains
    subroutine private_ggg
      ans = -1
      call assert(allocated(GRAPHS_), err_notAlloc_, 'GRAPHS_')
      if (err_free()) call assert(i > 0 .and. i <= size(GRAPHS_), err_oorng_, 'gi')
      if (err_free()) call assert(is_allocated(GRAPHS_(i)), err_notAlloc_, 'GRAPHS_(gi)')
      ans = i
    end subroutine private_ggg
  end function ggg
 
  !> Allocates the GRAPHS_ register.
  !! @author Filippo Monari
  !! @param[in] n integer, register dimension
  !! @todo rename allocate_graphs
  subroutine graphs__allocate (n)
    implicit none
    integer, intent(in) :: n
    integer :: info
    info = 0
    call do_safe_within('graphs__allocate', mod_nodes_name_, private_allocate)
  contains
    subroutine private_allocate
      call assert(.not. allocated(GRAPHS_), err_alreadyAlloc_, 'GRAPHS_')
      if (err_free()) allocate(GRAPHS_(n), stat=info)
      call assert(info == 0, err_alloc_, 'GRAPHS_')
    end subroutine private_allocate
  end subroutine graphs__allocate

  !> Deallocate the 'GRAPHS_' register.
  !! @author Filippo Monari
  subroutine deallocate_graphs ()
    implicit none
    call do_safe_within("deallocate_graphs", mod_nodes_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: i, info
      info = 0
      do i = 1, size(GRAPHS_)
         if (is_allocated(GRAPHS_(i))) call dealloc(GRAPHS_(i)%nodes, 'GRAPHS_(i)%nodes')
         if (.not. err_free()) exit
      end do
      if (err_free()) deallocate(GRAPHS_, stat=info)
      call assert(info == 0, err_dealloc_, 'GRAPHS_')
    end subroutine private_deallocate
  end subroutine deallocate_graphs          

  !> Apply the operators contained in the graph nodes.
  !! Not included in the interface
  !! @param[in] gi integer, graph index
  subroutine graph__op (gi)
    implicit none
    integer, intent(in) :: gi
    integer :: i, ii
    type(graph), pointer :: g
    g => GRAPHS_(gi)
    do i = 1, size(g%nodes)
       ii = g%nodes(i)
       call node__op(ii)
    end do
  end subroutine graph__op
  
  ! subroutine graph__fw_zero0 (nds)
  !   implicit none
  !   integer, intent(in) :: nds(:)
  ! end subroutine graph__fw_zero0

  !> Apply forward differentiation relatively to the operators
  !! contained in the graph nodes.
  !! @param[in] gi integer, graph index
  subroutine graph__fw (gi)
    implicit none
    integer, intent(in) :: gi
    integer :: i, ii
    type(graph), pointer :: g
    g => GRAPHS_(gi)
    do i = 1, size(g%nodes)
       ii = g%nodes(i)
       call node__fw(ii)
    end do
  end subroutine graph__fw

  !> Reset all the 'number' gradients ('dx') w.r.t.
  !! backward differentiation for all the graph nodes.
  !! Not included in the intereface.
  !! @param[in] gi integer, graph index
  subroutine graph__bw_zero (gi)
    implicit none
    integer, intent(in) :: gi
    integer :: i, ii
    type(graph), pointer :: g
    g => GRAPHS_(gi)
    do i = 1, size(g%nodes)
       ii = g%nodes(i)
       call node__bw_zero(ii)
    end do
  end subroutine graph__bw_zero

  !> Apply bacward differentiation relatively to the
  !! operators contained in the graph nodes.
  !! Not included in the interface.
  !! @param[in] gi integer, graph index
  subroutine graph__bw (gi)
    implicit none
    integer, intent(in) :: gi
    integer :: i, ii
    type(graph), pointer :: g
    g => GRAPHS_(gi)
    do i = size(g%nodes), 1, -1
       ii = g%nodes(i)
       call node__bw(ii)
    end do
  end subroutine graph__bw
               
end module nodes
