!  This file is part of Modello.
!
!  Modello is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation; either version 2 of the License, or
!  (at your option) any later version.
!  
!  Modello is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
!  MA 02110-1301, USA.

module types
  !_DOC_
  !This modules defines environmental parameters used in the calculations and the R interfaces that can be used to 
  !retrive such parameters from the R console.

  use env
  
  implicit none

  private
  public &
       prc_call, &
       number, &
       node, &
       graph, &
       gopt, &
       is_deallocated, &
       is_init, &
       is_empty, &
       is_allocated, &
       is_all_alloc, &
       is_all_assoc, &
       has_dx, &
       mtsz, &
       mtrnk

  !> Type 'procedure_call'
  !! Represents a call to a procedure during the calculations.
  type prc_call
     character(len=:), allocatable :: prcname !< procedure name
     character(len=:), allocatable :: modname !< name of the module containin the procedure
     type(prc_call), pointer :: next => null() !< pointer to the next procedure call
     type(prc_call), pointer :: prev => null() !< pointer to previous call
  end type prc_call
  
  !> Type 'number'
  !! Represents the numbers involved in the calculation .
  type number
     integer, pointer :: id => null() !< identifier index (position in the NUMBERS_ array)
     integer, pointer :: shp(:) => null() !< shape of the number
     integer, allocatable :: init(:)      !< initialisation register
     real(kind=dp_), pointer :: v(:) => null() !< value if the 'number'
     real(kind=dp_), pointer :: dv(:) => null() !< value of the 'number' derivative
  end type number
  
  !> Type 'node'
  !! A node stores the infrmations necessary to reproduce a certain calculation step.
  type node
     integer :: id = 0  !< identifier index (position in the NODES_ array)
     integer :: op = 0  !< operator ideitifier
     integer :: out = 0 !< identifier of the output 'number'
     integer, allocatable :: in(:) !< indentifiers of the input 'numbers'
     integer, allocatable :: flg(:) !< operator flags (can be empty)
  end type node

  !> Type 'graph'
  !! Is a collcetion of nodes and represent a certain calculation flow.
  type :: graph
     integer, allocatable :: nodes(:) !< nodes making the graph
  end type graph

  !> Type 'gopt'
  !! Opytimiser type. It stores all the information necessary to run iterations of
  !! an optimistaion algorithm
  type :: gopt
     integer, allocatable :: xin(:) !< indexes of the input 'numbers'
     integer :: iter = 0
     real(kind=dp_), allocatable :: v1(:) !< first order moment logging
     real(kind=dp_), allocatable :: v2(:) !< second order moment logging
  end type gopt
  
  !> Checks if a 'type' is correctly deallocated (returns .true.) or not (returns .false.).
  !! @author Filippo Monari
  !! @param[in] x a defined 'type'
  interface is_deallocated
     module procedure number__is_deallocated
     module procedure node__is_deallocated
     module procedure graph__is_deallocated
     module procedure gopt__is_deallocated
  end interface is_deallocated

  !> Checks if a 'number' is correctly initialised (returne .true.) or not (returns .false.).
  !! @author Filippo Monari
  !! @param[in] x a 'number'
  interface is_init
     module procedure number__is_init
  end interface is_init

  !> Cehcks if a 'number' or 'graph' is empty (returns .true.) or not (returns .false.).
  !! @author Filippo Monari
  !! @param[in] x a 'number' or 'graph'
  interface is_empty
     module procedure number__is_empty
     module procedure graph__is_empty
  end interface is_empty

  !> Checks if a 'type' is correctly allocated (returns .true.) or not (returns .false.).
  !! @author Filippo Monari
  !! @param[in] x a defined 'type'
  interface is_allocated
     module procedure prc_call__is_allocated
     module procedure number__is_allocated
     module procedure node__is_allocated
     module procedure graph__is_allocated
     module procedure gopt__is_allocated
  end interface is_allocated

  !> Checks if all the element of a 'number' are allocated (returns .true.).
  !! or not (returns .false)
  !! @author Filippo Monari
  !! @param[in] x a 'number'
  interface is_all_alloc
     module procedure number__is_all_alloc
  end interface is_all_alloc

  !> Checks if all the elements of a 'number' are associated (returns .true.).
  !! or not (returns .false.)
  !! @author Filippo Monari
  !! @param[in] x a 'number'
  interface is_all_assoc
     module procedure number__is_all_assoc
  end interface is_all_assoc
  
  !> Checks if a 'number' has a derivative element (returns .true.) or not (returns .false.).
  !! @author Filippo Monari
  !! @param[in] x a 'number'
  interface has_dx
     module procedure number__has_dx
  end interface has_dx

  !> Returns the size of a 'number' or 'graph'.
  !! @author Filippo Monari
  !! @param[in] x a 'number' or 'graph'
  interface mtsz
     module procedure number__size
     module procedure graph__size
  end interface mtsz

  !> Returns the rank of a 'number'.
  !! @author Filippo Monari
  !! @param[in] x a 'number'
  interface mtrnk
     module procedure number__rank
  end interface mtrnk
  
contains

  !> Returns .true. if the procedure call object (prc_call)
  !! is allocated correctly.
  !! @param[in] x prc_call object
  pure function prc_call__is_allocated (x) result(ans)
    implicit none
    type(prc_call), intent(in) :: x
    logical :: ans
    ans = allocated(x%prcname) .and. allocated(x%modname) .and. associated(x%next)
  end function prc_call__is_allocated

  !> Returns .true. if the procedure call object (prc_call) is
  !! deallocated correctly.
  !! @param[in] x prc_call object
  pure function number__is_deallocated (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    ans = &
         .not. associated(x%id) &
         .and. &
         .not. associated(x%shp) &
         .and. &
         .not. allocated(x%init) &
         .and. &
         .not. associated(x%v) &
         .and. &
         .not. associated(x%dv)
  end function number__is_deallocated

  !> Returns .true. if the 'number' is initialised
  !! correctly.
  !! @param[in] x 'number'
  pure function number__is_init (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    ans = allocated(x%init)
  end function number__is_init

  !> Returns .true. if the 'number' is initialised
  !! but empty.
  !! @param[in] x 'number'
  pure function number__is_empty (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    ans = .false.
    if (is_init(x)) ans = &
         all(x%init == init_null_) &
         .and. &
         .not. associated(x%id) &
         .and. &
         .not. associated(x%shp) &
         .and. &
         .not. allocated(x%init) &
         .and. &
         .not. associated(x%v) &
         .and. &
         .not. associated(x%dv) 
  end function number__is_empty

  !> Returns .true. if the 'number' is allocated
  !! correctly.
  !! @param[in] x 'number'
  pure function number__is_allocated (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    ans = .false.
    if (is_init(x)) ans = &
         all(x%init > 0) &
         .and. &
         associated(x%id) &
         .and. &
         associated(x%shp) &
         .and. &
         associated(x%v) &
         .and. &
         associated(x%dv) 
  end function number__is_allocated

  !> Returns .true. if all the elements of the 'number'
  !! are associated.
  !! @param[in] x 'number'
  pure function number__is_all_assoc (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    ans = .false.
    if (is_allocated(x)) ans = all(x%init == init_assoc_)
  end function number__is_all_assoc

  !> Returns .true. if all the elements of a 'number'
  !! are allocated
  !! @param[in] x 'number'
  pure function number__is_all_alloc (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    ans = .false.
    if (is_allocated(x)) ans = all(x%init == init_alloc_)
  end function number__is_all_alloc

  !> Returns .true. if the number as a gradient.
  !! @param[in] x 'number'
  pure function number__has_dx (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    logical :: ans
    ans = .false.
    if (is_allocated(x)) ans = size(x%dv) > 0
  end function number__has_dx

  !> Returns the size of the 'number'.
  !! @param[in] x 'number'
  pure function number__size (x) result(sz)
    implicit none
    type(number), intent(in) :: x
    integer :: sz
    sz  = -1
    if (is_allocated(x)) sz = product(x%shp)
  end function number__size

  !> Returns the rank (number of dimensions) of a 'number'.
  !! @param[in] x 'number'
  pure function number__rank (x) result(rnk)
    implicit none
    type(number), intent(in) :: x
    integer :: rnk
    rnk = -1
    if (is_allocated(x)) rnk = size(x%shp)
  end function number__rank

  !> Returns .true. if the 'node' is deallocated
  !! correctly.
  !! @param[in] x 'node'
  pure function node__is_deallocated (x) result(ans)
    implicit none
    type(node), intent(in) :: x
    logical :: ans
    ans = &
         x%id == 0 &
         .and. &
         x%op == 0 &
         .and. &
         x%out == 0 &
         .and. &
         .not. allocated(x%in) &
         .and. &
         .not. allocated(x%flg)
  end function node__is_deallocated

  !> Returns .true. if the 'node' is allocated correctly.
  !! @param[in] x 'node'
  pure function node__is_allocated (x) result(ans)
    implicit none
    type(node), intent(in) :: x
    logical :: ans
    ans = &
         x%op > 0 &
         .and. &
         x%out /= 0 &
         .and. &
         allocated(x%in) &
         .and. &
         allocated(x%flg)
  end function node__is_allocated

  !> Returns .true. if the 'graph' is deallocated
  !! correctly.
  !! @param[in] x 'graph'
  pure function graph__is_deallocated (x) result(ans)
    implicit none
    type(graph), intent(in) :: x
    logical :: ans
    ans = .not. allocated(x%nodes)
  end function graph__is_deallocated

  !> Returns .true. if the 'graph' is correctly
  !! allocated.
  !! @param[in] x 'graph'
  pure function graph__is_allocated (x) result(ans)
    implicit none
    type(graph), intent(in) :: x
    logical :: ans
    ans = allocated(x%nodes) 
  end function graph__is_allocated

  !> Returns the size (number of node) of
  !! the 'graph'.
  !! @param[in] x
  pure function graph__size (x) result(sz)
    implicit none
    type(graph), intent(in) :: x
    integer :: sz
    sz = 0
    if (is_allocated(x)) sz = size(x%nodes)
  end function graph__size

  !> Returns .true. if the 'graph' is empty
  !! (has 0 nodes).
  !! @param[in] x 'graph'
  pure function graph__is_empty (x) result(ans)
    implicit none
    type(graph), intent(in) :: x
    logical :: ans
    ans = .false.
    if (is_allocated(x)) ans = mtsz(x) == 0
  end function graph__is_empty

  !> Returns .true. if the 'opt' is
  !! allocated correctly.
  !! @param[in] x 'opt'
  pure function gopt__is_allocated (x) result(ans)
    implicit none
    type(gopt), intent(in) :: x
    logical :: ans
    ans = &
         allocated(x%xin) &
         .and. &
         x%iter >= 0 &
         .and. &
         allocated(x%v1) &
         .and. &
         allocated(x%v2)
  end function gopt__is_allocated

  !> Returns .true. if the 'opt' is deallocated
  !! correctly.
  !! @param[in] x 'opt'
  pure function gopt__is_deallocated (x) result(ans)
    implicit none
    type(gopt), intent(in) :: x
    logical :: ans
    ans = &
         .not.allocated(x%xin) &
         .and. &
         x%iter == 0 &
         .and. &
         .not. allocated(x%v1) &
         .and. &
         .not. allocated(x%v2)
  end function gopt__is_deallocated
  
end module types



	
