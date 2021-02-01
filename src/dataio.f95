module dataio
  use env
  use types
  use registers, only: INUNITS_
  use errwarn
  use utils

  private

  public &
       allocate_inunits, &
       deallocate_inunits, &
       inunit__append, &
       inunit__pop, &
       inunit__nrow, &
       inunit__ncol, &
       inunit__read_line, &
       iii
  
  character(len=*), parameter :: mod_dataio_name_ = 'dataio'

  integer :: datau_ = 1000

contains

  !> Opens a file and connects it to a unit.
  !! @param[in] fname character, file name
  !! @param[in] u integer, unit
  !! @param[in] s character, status
  !! @param[in] a character, action
  subroutine open_file (fname, u, s, a)
    implicit none
    character(len=*), intent(in) :: fname, s, a
    integer, intent(in) :: u
    call do_safe_within('open_file', mod_dataio_name_, private_open)
  contains
    subroutine private_open
      integer :: info
      logical :: file_exists
      info = 0
      inquire(file=fname, exist=file_exists)
      call assert(file_exists, err_generic_, 'file '//fname//' not found.')
      if (err_free()) open(u, file=fname, status=s, action=a, iostat=info)
      call assert(info==0, err_generic_, 'error opening file '//fname)
    end subroutine private_open
  end subroutine open_file

  !> Closes a file given it unit.
  !! @param[in] fmane character, file name for error reposting purposes
  !! @param[in] u integer, unit
  !! @param[in] s character, status
  subroutine close_file (fname, u, s)
    implicit none
    character(len=*), intent(in) :: fname, s
    integer, intent(in) :: u
    call do_safe_within('close_file', mod_dataio_name_, private_close)
  contains
    subroutine private_close
      integer :: info
      info = 0
      close(u, status=s, iostat=info)
      call assert(info == 0, err_generic_, 'error in closing '//fname//'.')
    end subroutine private_close
  end subroutine close_file

  !> Allocates the INUNITS_ register.
  !! @param[in] n integer, array size
  subroutine allocate_inunits (n)
    implicit none
    integer, intent(in) :: n
    call do_safe_within('allocate_inunits', mod_dataio_name_, private_allocate)
  contains
    subroutine private_allocate
      integer :: info
      info = 0
      call assert(.not. allocated(INUNITS_), err_alreadyAlloc_, 'INUNITS_')
      if (err_free()) allocate(INUNITS_(n), stat=info)
      call assert(info == 0, err_alloc_, 'INUNITS_')
    end subroutine private_allocate
  end subroutine allocate_inunits

  !> Deallocates the INUNITS_ register.
  subroutine deallocate_inunits ()
    implicit none
    call do_safe_within('deallocate_inunits', mod_dataio_name_, private_deallocate)
  contains
    subroutine private_deallocate
      integer :: i, info
      call assert(allocated(INUNITS_), err_notAlloc_, 'INUNITS_')
      if (err_free()) then
         do i = 1, size(INUNITS_)
            if (is_allocated(INUNITS_(i))) call inunit__deallocate(INUNITS_(i))
            if (.not. err_free()) exit
         end do
      end if
      if (err_free()) then
         deallocate(INUNITS_, stat=info)
         call assert(info==0, err_dealloc_, 'INUNITS_')
      end if
    end subroutine private_deallocate
  end subroutine deallocate_inunits

  !> Allocates an 'inunit'.
  !! It requires the schema of the inpout file written in
  !! the 'data.modello' file.
  !! @param[inout] iu inunit to allocate
  subroutine inunit__allocate (iu)
    implicit none
    type(inunit), intent(inout) :: iu
    integer :: n
    call do_safe_within('inunit__allocate', mod_dataio_name_, private_do)
  contains
    subroutine private_do
      call open_file(data_, datau_, 'old', 'read')
      call err_safe(private_init)
      call err_safe(private_open)
      call close_file(data_, datau_, 'keep')
      datau_ = datau_ + n + 1
    end subroutine private_do
    subroutine private_init
      integer :: info
      info = 0
      read(datau_, *, iostat=info) n
      call assert(info == 0, err_generic_, 'error in reading n')
      call alloc(iu%units, n, 'iu%units')
      call alloc(iu%is_open, n, 'iu%is_open')
    end subroutine private_init
    subroutine private_open
      character(len=50) :: inf
      integer :: info, i, u
      info = 0
      do i = 1, n
         read(datau_, '(A)', iostat=info) inf
         call assert(info == 0, err_generic_, 'error in reading inf')
         if (err_free()) then
            u = datau_ + i
            call open_file(inf, u, 'old', 'read')
            if (err_free()) then
               iu%units(i) = u
               iu%is_open(i) = 1
            end if
         end if
         if (.not. err_free()) exit
      end do
    end subroutine private_open
  end subroutine inunit__allocate

  !> Deallocates an inunit.
  !! @param[inout] iu inunit to deallocate
  subroutine inunit__deallocate (iu)
    implicit none
    type(inunit), intent(inout) :: iu
    call do_safe_within("inunit__deallocate", mod_dataio_name_, private_do)
  contains
    subroutine private_do
      call assert(is_allocated(iu), err_notAlloc_, 'iu')
      call err_safe(private_deallocate)
      call dealloc(iu%units, 'iu%units')
      call dealloc(iu%is_open, 'iu%is_open')
    end subroutine private_do
    subroutine private_deallocate
      integer :: i
      do i = 1, size(iu)
         call close_file('iu%units(i)', iu%units(i), 'keep')
         if (err_free()) then
            iu%is_open(i) = 0
         else
            exit
         end if
      end do
    end subroutine private_deallocate
  end subroutine inunit__deallocate

  !> Gets the next free spot in the INUNITS_ register.
  function inunit__next () result(i)
    implicit none
    integer :: i
    call do_safe_within('inunit__next', mod_dataio_name_, private_do)
  contains
    subroutine private_do
      call assert(allocated(INUNITS_), err_notAlloc_, 'INUNIT_')
      call err_safe(private_next)
    end subroutine private_do
    subroutine private_next
      integer :: info
      info = 1
      i = 0
      do while (i <= size(INUNITS_))
         i = i + 1
         if (is_deallocated(INUNITS_(i))) then
            info = 0
            exit
         end if
      end do
      call assert(info == 0, err_generic_, 'INUNITS_ register if full.')
    end subroutine private_next
  end function inunit__next

  !> Appends a new inunit to the INUNITS_ register.
  !! Return its id (position index in the array)
  !! @param[out] iu integer, inunit id
  subroutine inunit__append (iu)
    implicit none
    integer, intent(out) :: iu
    call do_safe_within('inunit__allocate', mod_dataio_name_, private_append)
  contains
    subroutine private_append
      iu = inunit__next()
      call inunit__allocate(INUNITS_(iu))
    end subroutine private_append
  end subroutine inunit__append

  !> Pops (removes) an inunit from the INUNITS_ register, given its id.
  !! @param[in] iu integer, inunit id
  subroutine inunit__pop (iu)
    implicit none
    integer, intent(in) :: iu
    call do_safe_within('inunit__pop', mod_dataio_name_, private_pop)
  contains
    subroutine private_pop
      call assert(allocated(INUNITS_), err_notAlloc_, 'INUNITS')
      call assert(iu > 0 .and. iu <= size(INUNITS_), err_oorng_, 'iu')
      call inunit__deallocate(INUNITS_(iu))
    end subroutine private_pop
  end subroutine inunit__pop

  !> Returns the number of data rows.
  !! @todo check actually it does not work
  !! @param[in] iu 'inputunit'
  function inunit__nrow (iu) result(m)
    implicit none
    integer, intent(in) :: iu
    integer :: m
    call do_safe_within('inunit__nrow', mod_dataio_name_, private_nrow)
  contains
    subroutine private_nrow
      integer :: info
      info = 0
      m = 0
      do while (info == 0)
         m = m + 1
         read(INUNITS_(iu)%units(1), *, iostat=info)
      end do
      rewind INUNITS_(iu)%units(1)
    end subroutine private_nrow
  end function inunit__nrow

  !> Returns the number of data columns.
  !! @todo check actually it does not work
  !! @param[in] iu 'inunit'
  function inunit__ncol (iu, m) result(n)
    implicit none
    integer, intent(in) :: iu, m
    integer :: n
    call do_safe_within('inunit__ncol', mod_dataio_name_, private_ncol)
  contains
    subroutine private_ncol
      integer :: info
      real(kind=dp_) :: x
      info = 0
      n = 0
      do while (info == 0)
         n = n + 1
         read(INUNITS_(iu)%units(1), *, iostat=info) x
      end do
      call assert(mod(n, m) == 0, err_generic_, &
           'number of values not compatible with number of rows')
      if (err_free()) n = n / m
      rewind INUNITS_(iu)%units(1)
    end subroutine private_ncol
  end function inunit__ncol
  
  !> Reads a line from an inunit.
  !! @param[in] iu integer, inunit id
  !! @param[in] i integer, connection index
  !! @param[inout] x double precision(:), where to put the read values
  !! @param[out] info integer, error flag
  subroutine inunit__read_line (iu, i, x, info)
    implicit none
    type(inunit), intent(in) :: iu
    integer, intent(in) :: i
    real(kind=dp_), intent(inout) :: X(:)
    integer, intent(out) :: info
    info = 0
    read(iu%units(i), *, iostat=info) X
    if (info < 0) then
       rewind iu%units(i)
       read(iu%units(i), *, iostat=info) X
    end if
  end subroutine inunit__read_line

  !> Checks that an inunit id is legal.
  !! @param[in] iu integer, inunit id
  function iii (iu) result(ans)
    implicit none
    integer, intent(in) :: iu
    integer :: ans
    call do_safe_within('iii', mod_dataio_name_, private_check)
  contains
    subroutine private_check
      ans = -1
      call assert(allocated(INUNITS_), err_notAlloc_, 'INUNITS_')
      call assert(iu > 0 .and. iu <= size(INUNITS_), err_oorng_, 'iu')
      call assert(is_allocated(INUNITS_(iu)), err_notAlloc_, 'INUNITS_(iu)')
      if (err_free()) ans = iu
    end subroutine private_check
  end function iii
 
end module dataio
