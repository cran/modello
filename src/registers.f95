module registers

  use types

  implicit none

  !> @defgroup registers_errorTracking Error Traceback
  !! The following registers are used to track and handle errors.
  !! @{
  type(prc_call), target :: PRC_CALL_HEAD_ !< head of the call stack
  type(prc_call), pointer :: PRC_CALL_NEXT_ => PRC_CALL_HEAD_ !< next stack position
  integer :: INFO_ = 0             !< error flag (error => /= 0)
  logical :: ISCRITICAL_ = .false. !< critical error flag
  integer :: WINFO_ = 0            !< warning flag (warning => /=0)
  !> @}
  
  !> @defgroup registers_numbersRegisters Numbers' Registers
  !! The following registers are used to store and manage the 'numbers' in the calculations.
  !! All these three arrays mus be allocated woth the same length as a 'number' id identify the
  !! a slot in each of three registers.
  !! @{
  type(number), allocatable, target :: NUMBERS_(:) !< array storing all the numbers in ths calculations
  integer, allocatable :: INLOCKS_(:) !< array coounting for each number how many times is used as calculation input
  integer, allocatable :: NUMNDS_(:) !< array storing the index of the node of which a certain number is output (if any)
  !> @}
  
  !> @defgroup registers_nodesRegisters_ Nodes' Registers
  !! The following registers are used to keep track and manage nodes representing calculation steps.
  !! 'NODES_' and 'NODEGRPHS_' must be allocated with the same length since a slot in the former must a
  !! a corresponding slot in the latter.
  !! @{
  type(node), allocatable, target :: NODES_(:) !< array storing all the nodes
  integer, allocatable :: NODEGRPHS_(:) !< array storgint he index of the graph including a certain node
  integer :: NODEi_ = 0 !< stores the index (position in 'NODES_') of the node corrently on the stack
  !> @}
  
  !> @defgroup registers_graphs_ Graphs' Registers.
  !! The following registers keep track and manage 'graphs' representing calculation flows.
  !! @{
  type(graph), allocatable, target :: GRAPHS_(:) !< array stroring all the 'graphs'
  integer :: GRAPHi_ = 0 !< stores the index (position in 'GRAPHS_') of the 'graph' currently on the stack
  type(graph), pointer :: GRAPH_ !< pointer to the 'graph' currently on the stack (=> GRAPHS_(GRAPHi_))
  !> @}

  !> @defgroup registers_optimisation_ Optimisation Registers
  !! @{
  type(gopt), allocatable, target :: GOPTS_(:) !< Array storing all the optimisers created
  !> @}
  
end module registers
