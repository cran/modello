module numbers_modifiers

  use env
  use numbers
  use errwarn
  use types
  use nodes
  use operators

  implicit none

  character(len=*), parameter :: mod_numbers_modifiers_name_ = 'numbers_modifiers'

contains

  !> Returns slice of a number as specified by the matrix s
  !! @param[in] x 'number' to slice
  !! @param[in] s integer(:,:), slice matrix
  function number__slice (x, s) result(ans)
    implicit none
    type(number), intent(in) :: x
    integer, intent(in), contiguous, target :: s(:,:)
    type(number), pointer :: ans
    call do_safe_within("number__slice", mod_numbers_modifiers_name_, private_slice)
  contains
    subroutine private_slice
      integer, pointer :: ss(:)
      call number__append_slice(ans, x, s)
      if (err_free()) ss(1:size(s)) => s
      call node__append(op_slice_id_, x, ans, ss)
      call graph__append
    end subroutine private_slice
  end function number__slice

  !> Returns the flat slice of a 'number' as
  !! specified by the vector s
  !! @param[in] x 'number' to slice
  !! @param[in] s integer(:), slice vector
  function number__flat_slice (x, s) result(ans)
    implicit none
    type(number), intent(in) :: x
    integer, intent(in) :: s(:)
    type(number), pointer :: ans
    call do_safe_within("number__flat_slice", mod_numbers_modifiers_name_, private_slice)
  contains
    subroutine private_slice
      call number__append_flat_slice(ans, x, s)
      call node__append(op_flat_slice_id_, x, ans, s)
      call graph__append
    end subroutine private_slice
  end function number__flat_slice

  !> Returns a contiguous slice of a 'number'.
  !! A contiguous slice is done along the dominant dimension (columns).
  !! Data are not copied be refernece through a pointer.
  !! @param[in] x 'number' to slice
  !! @param[in] s1 integer, initial index of the slice
  !! @param[in] s2 integer, final index of the slice
  function number__contiguous_slice (x, s1, s2) result(ans)
    type(number), intent(in) :: x
    integer, intent(in) :: s1, s2
    type(number), pointer :: ans
    call do_safe_within("number__slice", mod_numbers_modifiers_name_, private_slice)
  contains
    subroutine private_slice
      call number__append_contiguous_slice(ans, x, s1, s2)
      call node__append(op_contiguous_slice_id_, x, ans, [s1, s2])
      call graph__append
    end subroutine private_slice
  end function number__contiguous_slice

  !> Returns a 'number' reshaping another one.
  !! Data are not copied but referred through a pointer.
  !! @param[in] x 'number' to slice
  !! @param[in] shp integer(:), new shape vector
  function number__reshape (x, shp) result(ans)
    implicit none
    type(number), intent(in) :: x
    integer, intent(in) :: shp(:)
    type(number), pointer :: ans
    call do_safe_within("number__reshape", mod_numbers_modifiers_name_, private_reshape)
  contains
    subroutine private_reshape
      call number__append_reshape(ans, x, shp)
      call node__append(op_reshape_id_, x, ans, shp)
      call graph__append
    end subroutine private_reshape
  end function number__reshape

  !> Returns a 'number' dropping all the dimensions collapsed to 1.
  !! Data are not copied but referenced through a pointer.
  !! @param[in] x 'number'
  function number__drop_shape (x) result(ans)
    implicit none
    type(number), intent(in) :: x
    type(number), pointer :: ans
    call do_safe_within("number__drop_shape", mod_numbers_modifiers_name_, private_drop_shape)
  contains
    subroutine private_drop_shape
      call number__append_drop_shape(ans, x)
      call node__append(op_drop_shape_id_, x, ans, [integer::])
      call graph__append
    end subroutine private_drop_shape
  end function number__drop_shape

  !> Returns the bind between two 'numbers' along the specifide dimension.
  !! @param[in] x1,x2 'numbers' to bind
  !! @param[in] k integer, dimension index
  function number__bind (x1, x2, k) result(ans)
    implicit none
    type(number), intent(in) :: x1, x2
    integer, intent(in) :: k
    type(number), pointer :: ans
    call do_safe_within ('number__bind', mod_numbers_modifiers_name_, private_bind)
  contains
    subroutine private_bind
      call number__append_bind(ans, x1, x2, k)
      call node__append(op_bind_id_, x1, x2, ans, [k])
      call graph__append
    end subroutine private_bind  
  end function number__bind

  !> Returns the bind between two 'numbers' along the specifide dimension.
  !! @param[in] x1,x2 'numbers' to bind
  !! @param[in] k integer, dimension index
  function number__embeddings (f, x, t) result(ans)
    implicit none
    type(number), intent(in) :: f, x
    integer, intent(in) :: t
    type(number), pointer :: ans
    call do_safe_within('number__embeddings', mod_numbers_modifiers_name_, private_emb)
  contains
    subroutine private_emb
      integer :: n
      n = 0
      call assert(is_allocated(f), err_notAlloc_, 'f')
      call assert(is_allocated(x), err_notAlloc_, 'x')
      call assert(mtrnk(f) == 1, err_generic_, 'f has rankl /= 1')
      call assert(maxval(f%v) <= x%shp(mtrnk(x)), err_oorng_, 'f')
      if (err_free()) n = mtrnk(x) - 1
      if (t > 0) then 
         call number__append(ans, [x%shp(1:n),mtsz(f)], has_dx(x))
      else
         call number__append(ans, [mtsz(f),x%shp(1:n)], has_dx(x))
      end if
      call node__append(op_embeddings_id_, f, x, ans, [t])
      call graph__append
      if (err_free()) call op_embeddings(f, x, ans, t)
    end subroutine private_emb
  end function number__embeddings

!  !> Feeds a 'number' with the data coming from an 'inunit'
!  !! @param[in] x 'number'
!  !! @param[in] iu 'inunit'
!  subroutine number__feed (x, iu) 
!    implicit none
!    type(number), intent(inout) :: x
!    integer, intent(in) :: iu
!    call do_safe_within('number__feed', mod_numbers_modifiers_name_, private_feed)
!  contains
!    subroutine private_feed
!      integer :: i
!      i = iii(iu)
!      call assert(rank(x) == 2, err_generic_, 'x rank /= 2')
!      call node__append(op_feed_id_, x, [i])
!      call graph__append
!      if (err_free()) call op_feed(x, i)
!    end subroutine private_feed
!  end subroutine number__feed
  
end module numbers_modifiers
