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

module numbers
  !_DOC_
  !This module defines the data structures to be used in the calculations.
  !New required data structures must be defined in this file as well.

  !* DEPENDENCIES

  use env
  use types
  use registers, only: &
       NUMBERS_, &
       INLOCKS_, &
       NUMNDS_, & 
       NODES_
  use errwarn
  use utils
  use numbers_utils
  use node_operators
  use nodes

  implicit none

  private

  public &
       allocate_numbers, &
       deallocate_numbers, &
       number__append, &
       number__append_slice, &
       number__append_flat_slice, &
       number__append_contiguous_slice, &
       number__append_reshape, &
       number__append_drop_shape, &
       number__append_bind, &
       number__op, &
       number__fw, &
       number__bw_zero, &
       number__bw, &
       number__pop, &
       number__gc


  character(len=*), parameter :: mod_numbers_name_ = 'numbers'

  !> Startng from a source entity or shape vector, appends a new number to the relative register
  !! and returns its 'symbol'
  !! @author Filippo Monari
  !! @param[inout] x 'symbol' to associate to the appended 'number'
  !! @param[in] shp integer(:), shape
  !! @param[in] dx logical, if .true. the derivative is allocated
  !! @param[in] v double precision optional, source
  interface number__append
     module procedure number__append__1
     module procedure number__append__2
     module procedure number__append__3
  end interface number__append

contains

  !> Allocates the number register.
  !! @author Filippo Monari
  !! @param[in] n integer, dimension of the register
  subroutine allocate_numbers (n)
    implicit none
    integer, intent(in) :: n
    call do_safe_within('allocate_numbers', mod_numbers_name_, private_do)
  contains
    subroutine private_do
      call assert(.not. allocated(NUMBERS_), err_alreadyAlloc_, 'NUMBERS_')
      call assert(.not. allocated(INLOCKS_), err_alreadyAlloc_, 'INLOCKS_')
      call assert(.not. allocated(NUMNDS_), err_alreadyAlloc_, 'NUMBDS_')
      call err_safe(private_allocate)
    end subroutine private_do
    subroutine private_allocate
      integer :: info
      info = 0
      allocate(NUMBERS_(n), stat=info)
      call assert(info == 0, err_alloc_, 'NUMBERS_')
      call alloc(INLOCKS_, n, 'INLOCK_')
      call alloc(NUMNDS_, n, 'NUMBER_NDS_')
    end subroutine private_allocate
  end subroutine allocate_numbers

  !> Deallocates the 'NUMBERS_' register.
  !! @author Filippo Monari
  subroutine deallocate_numbers ()
    implicit none
    call do_safe_within('deallocate_numbers', mod_numbers_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(NUMBERS_) .and. allocated(INLOCKS_) .and. &
           allocated(NUMNDS_), err_notAlloc_, 'NUMBERS_')
      call err_safe(private_deallocate)
    end subroutine private_do
    subroutine private_deallocate
      integer :: i, info
      info = 0
      do i = 1, size(NUMBERS_)
         if (is_init(NUMBERS_(i))) then
            call number__reset_nd(NUMBERS_(i))
            call number__deallocate(NUMBERS_(i))
         end if
         if (.not. err_free()) exit
      end do
      if (err_free()) deallocate(NUMBERS_, stat=info)
      call assert(info == 0, err_dealloc_, 'NUMBERS_')
      call dealloc(INLOCKS_, 'INLOCKS_')
      call dealloc(NUMNDS_, 'NUMNDS_')
    end subroutine private_deallocate
  end subroutine deallocate_numbers    

  !> Returns the next free slot in the NUMBERS_ register.
  !! @author Filippo Monari
  function number__next () result(i)
    integer :: i
    integer :: info
    call do_safe_within('number__next', mod_numbers_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(NUMBERS_), err_notAlloc_, 'NUMBERS_')
      call err_safe(private_next)
    end subroutine private_do
    subroutine private_next
      info = 1
      i = 0
      do while (i <= size(NUMBERS_))
         i = i + 1
         if (is_deallocated(NUMBERS_(i))) then
            info = 0
            exit
         end if
      end do
      call assert(info == 0, err_generic_, 'NUMBERS_ is full.')
    end subroutine private_next
  end function number__next

  !> Appends a 'number' with shape 'shp'.
  !! The number is initialised to 0.
  !! @param[inout] x 'number' to associate
  !! @param[in] shp integer(:), shape vector
  !! @param[in] dx logical, if .true. the number gets a gradient
  subroutine number__append__1 (x, shp, dx)
    implicit none
    type(number), intent(out), pointer :: x
    integer, intent(in) :: shp(:)
    logical, intent(in) :: dx
    integer :: i
    call do_safe_within('number__append__1', mod_numbers_name_, private_append)
  contains
    subroutine private_append
      i = number__next()
      call number__allocate(NUMBERS_(i), shp, dx)
      x => NUMBERS_(i)
      if (err_free()) x%id = i
    end subroutine private_append
  end subroutine number__append__1
  
  !> Appends a 'number' with shape 'shp', and as source the scalar 'v'.
  !! @param[inout] x 'number' to associate
  !! @param[in] shp integer(:), shape vector
  !! @param[in] dx logical, if .true. the number gets a gradient
  !! @param[in] v double precision
  subroutine number__append__2 (x, shp, dx, v)
    implicit none
    type(number), intent(out), pointer :: x
    integer, intent(in) :: shp(:)
    logical, intent(in) :: dx
    real(kind=dp_), intent(in) :: v
    integer :: i
    call do_safe_within('number__append__2', mod_numbers_name_, private_append)
  contains
    subroutine private_append
      i = number__next()
      call number__allocate(NUMBERS_(i), shp, dx, v)
      x => NUMBERS_(i)
      if (err_free()) x%id = i
    end subroutine private_append
  end subroutine number__append__2

  !> Appends a 'number' with shape 'shp', and source the vector 'v'.
  !! @param[inout] x 'number' to associate
  !! @param[in] shp integer(:), shape vector
  !! @param[in] dx logical, if .true. the number gets a gradient
  !! @param[in] v double precision(:)
  subroutine number__append__3 (x, shp, dx, v)
    implicit none
    type(number), intent(out), pointer :: x
    integer, intent(in) :: shp(:)
    logical, intent(in) :: dx
    real(kind=dp_), intent(in) :: v(:)
    integer :: i
    call do_safe_within('number__append__3', mod_numbers_name_, private_append)
  contains
    subroutine private_append
      i = number__next()
      call number__allocate(NUMBERS_(i), shp, dx, v)
      x => NUMBERS_(i)
      if (err_free()) x%id = i
    end subroutine private_append
  end subroutine number__append__3

  !> Appends the slice of a 'number'
  !! @author Filippo Monari
  !! @param[inout] x1 'number' to associate to the slice
  !! @param[in] x2 'number' to slice
  !! @param[in] s integer(:,:), slice matrix
  subroutine number__append_slice (x1, x2, s)
    implicit none
    type(number), intent(out), pointer :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: s(:,:)
    call do_safe_within("number__append_slice", mod_numbers_name_, private_slice)
  contains
    subroutine private_slice
      integer :: i
      i = number__next()
      call number__take_slice(NUMBERS_(i), x2, s)
      x1 => NUMBERS_(i)
      if (err_free()) x1%id = i
    end subroutine private_slice
  end subroutine number__append_slice

  !> Appends the flat slice of a 'number'.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' to associate to the slice
  !! @param[in] x2 'number' to slice
  !! @param[in] s integer(:), slice vector
  subroutine number__append_flat_slice (x1, x2, s)
    implicit none
    type(number), intent(inout), pointer :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: s(:)
    call do_safe_within("number__append_slice", mod_numbers_name_, private_slice)
  contains
    subroutine private_slice
      integer :: i
      i = number__next()
      call number__take_flat_slice(NUMBERS_(i), x2, s)
      x1 => NUMBERS_(i)
      if (err_free()) x1%id = i
    end subroutine private_slice
  end subroutine number__append_flat_slice

  !> Apppends a contiguous slice of a 'number'.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' to associate to the slice
  !! @param[in] x2 'number' to slice
  !! @param[in] s1,s2 integer, initial and fine indexes of the slice
  subroutine number__append_contiguous_slice (x1, x2, s1, s2)
    implicit none
    type(number), intent(inout), pointer :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: s1, s2
    call do_safe_within("number__append_slice", mod_numbers_name_, private_append_slice)
  contains
    subroutine private_append_slice
      integer :: i
      i = number__next()
      call number__take_contiguous_slice(NUMBERS_(i), x2, s1, s2)
      x1 => NUMBERS_(i)
      if (err_free()) x1%id = i
    end subroutine private_append_slice
  end subroutine number__append_contiguous_slice

  !> Append the reshaping of a 'number'.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' to associate with the reshaping
  !! @param[in] x2 'number' to reshape
  !! @param[in] shp integer(:), new shape
  subroutine number__append_reshape (x1, x2, shp)
    implicit none
    type(number), intent(inout), pointer :: x1
    type(number), intent(in) :: x2
    integer, intent(in) :: shp(:)
    call do_safe_within('number__append_reshape', mod_numbers_name_, private_append_reshape)
  contains
    subroutine private_append_reshape
      integer :: i
      i = number__next()
      call number__make_reshape(NUMBERS_(i), x2, shp)
      x1 => NUMBERS_(i)
      if (err_free()) x1%id = i
    end subroutine private_append_reshape
  end subroutine number__append_reshape

  !> Append the drop reshaping of a 'number'.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' to associate with the reshaping
  !! @param[in] x2 'number' to reshape
  subroutine number__append_drop_shape (x1, x2)
    implicit none
    type(number), intent(inout), pointer :: x1
    type(number), intent(in) :: x2
    call do_safe_within('number__append_drop_shape', mod_numbers_name_, private_append_drop)
  contains
    subroutine private_append_drop
      integer :: i
      i = number__next()
      call number__make_drop_shape(NUMBERS_(i), x2)
      x1 => NUMBERS_(i)
      if (err_free()) x1%id = i
    end subroutine private_append_drop
  end subroutine number__append_drop_shape

  !> Append the binding of two 'numbers'.
  !! @author Filippo Monari
  !! @param[inout] x1 'number' to associate with the binding
  !! @param[in] x2,x3 'numbers' to bind
  !! @param[in] k integer, dimension along which do the binding
  subroutine number__append_bind (x1, x2, x3, k)
    implicit none
    type(number), intent(inout), pointer :: x1
    type(number), intent(in) :: x2, x3
    integer, intent(in) :: k
    call do_safe_within('number__append_bind', mod_numbers_name_, private_append_bind)
  contains
    subroutine private_append_bind
      integer :: i
      i = number__next()
      call number__make_bind(NUMBERS_(i), x2, x3, k)
      x1 => NUMBERS_(i)
      if (err_free()) x1%id = i
    end subroutine private_append_bind    
  end subroutine number__append_bind

  !> Runs the calculation stored in the node that returned the 'number'
  !! as output
  !! @author Filippo Monari
  !! @param[in] x 'number'
  subroutine number__op (x)
    implicit none
    type(number), intent(inout) :: x
    call do_safe_within("number__op", mod_numbers_name_, private_op)
  contains
    subroutine private_op
      integer :: ndi
      ndi = number__get_nd(x)
      call assert(ndi > 0, err_generic_, "x is not node output.")
      if (err_free()) call node__op(ndi)
    end subroutine private_op
  end subroutine number__op

  !> Runs the forward differentiation stored in the node that
  !! returned the 'number' as output
  !! @author Filippo Monari
  !! @param[in] x 'number'
  subroutine number__fw (x)
    implicit none
    type(number), intent(inout) :: x
    call do_safe_within("number__fw", mod_numbers_name_, private_op)
  contains
    subroutine private_op
      integer :: ndi
      ndi = number__get_nd(x)
      call assert(ndi > 0, err_generic_, "x is not node output.")
      if (err_free()) call node__fw(ndi)
    end subroutine private_op
  end subroutine number__fw

  !> Reset (sets to 0) the gradients relative to the node that
  !! returned the 'number' as output according to the backward schema
  !! @author Filippo Monari
  !! @param[in] x 'number'
  subroutine number__bw_zero (x)
    implicit none
    type(number), intent(inout) :: x
    call do_safe_within("number__fw", mod_numbers_name_, private_op)
  contains
    subroutine private_op
      integer :: ndi
      ndi = number__get_nd(x)
      call assert(ndi > 0, err_generic_, "x is not node output.")
      if (err_free()) call node__bw_zero(ndi)
    end subroutine private_op
  end subroutine number__bw_zero

  !> Runs the backward differentiation stored in the node that
  !! returned the 'number' as output
  !! @author Filippo Monari
  !! @param[in] x 'number'
  !! @todo in all number__op routine modify with node pointer function
  subroutine number__bw (x)
    implicit none
    type(number), intent(inout) :: x
    call do_safe_within("number__fw", mod_numbers_name_, private_op)
  contains
    subroutine private_op
      integer :: ndi
      ndi = number__get_nd(x)
      call assert(ndi > 0, err_generic_, "x is not node output.")
      if (err_free()) call node__bw(ndi)
    end subroutine private_op
  end subroutine number__bw  
  
  !> Pops (removes) a 'number' from its register, given its 'symbol'.
  !! @author Filippo Monari
  !! @parame[in] x 'number'
  subroutine number__pop (x)
    implicit none
    type(number), intent(inout), pointer :: x
    integer :: i, nd
    call do_safe_within('number__pop', mod_numbers_name_, private_do)
  contains
    subroutine private_do
      !call assert(is_all_assoc(x), err_generic_, 'x os not all associated')
      call assert(number__inlock_free(x), err_generic_, 'inlock is not free.')
      call err_safe(private_pop)
    end subroutine private_do
    subroutine private_pop
      i = x%id
      nd = number__get_nd(x)
      if (nd > 0) call node__pop(nd)
      call number__reset_nd(x)
      nullify(x)
      call number__deallocate(NUMBERS_(i))
    end subroutine private_pop
  end subroutine number__pop

  !> Garbage collector for 'numbers'. A garbage 'number' is defined
  !! as one that was not created as origin of a 'graph', does not have
  !! a generating node anymore and it is not input of any other node.
  !! This subroutine deallocates all the garbage 'numbers' in 'NUMBERS_'.
  subroutine number__gc ()
    implicit none
    call do_safe_within('number__gc', mod_numbers_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(NUMBERS_), err_notAlloc_, 'NUMBERS_')
      call err_safe(private_gc)
    end subroutine private_do
    subroutine private_gc
      integer :: i
      do i = 1, size(NUMBERS_)
         if (is_allocated(NUMBERS_(i))) then
            if (number__get_nd(nnn(i)) < 0 .and. number__inlock_free(nnn(i))) then
               call number__reset_nd(NUMBERS_(i))
               call number__deallocate(NUMBERS_(i))
            end if
         end if
      end do
    end subroutine private_gc
  end subroutine number__gc
 
end module numbers
