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

module errwarn
!_DOC_
!Function for handlind and raise errors and warnings.
  
  use env
  use types
  use registers, only: &
       PRC_CALL_HEAD_, &
       PRC_CALL_NEXT_, &
       INFO_, &
       ISCRITICAL_, &
       WINFO_

  implicit none

  private

  public &
       do_within, &
       do_safe_within, &
       do_within_critical, &
       err_safe, &
       err_free, &
       raise_error, &
       assert, &
       warn
  
  interface
     subroutine r__error (msg)
       use iso_c_binding
       character(kind=c_char, len=*), intent(in) :: msg
     end subroutine r__error
     subroutine r__warn (msg)
       use iso_c_binding
       character(kind=c_char, len=*), intent(in) :: msg
     end subroutine r__warn
  end interface
       
contains
   
  !> Append a procedure call to the stack.
  !! @author Filippo Monari
  !! @param prcname character, procedure name
  !! @param modname character, module name
  subroutine prc_call__append (prcname, modname)
    implicit none
    character(len=*), intent(in) :: prcname, modname
    call err_safe(private_append)
  contains
    subroutine private_append
      allocate(PRC_CALL_NEXT_%prcname, source=prcname)
      allocate(PRC_CALL_NEXT_%modname, source=modname)
      allocate(PRC_CALL_NEXT_%next)
      PRC_CALL_NEXT_%next%prev => PRC_CALL_NEXT_
      PRC_CALL_NEXT_ => PRC_CALL_NEXT_%next
    end subroutine private_append
  end subroutine prc_call__append

  !> Pop (remove) the last procedure call from the stack.
  !! @author Filippo Monari
  subroutine prc_call__detach ()
    implicit none
    if (associated(PRC_CALL_NEXT_%prev)) then
       PRC_CALL_NEXT_ => PRC_CALL_NEXT_%prev
       nullify(PRC_CALL_NEXT_%next%prev)
       deallocate(PRC_CALL_NEXT_%next)
       deallocate(PRC_CALL_NEXT_%prcname)
       deallocate(PRC_CALL_NEXT_%modname)
    end if
  end subroutine prc_call__detach

  !> Writes to stderr all the procedure call in the stack.
  !! @author Filippo Monari 
  subroutine trace_call_stack (trace)
    implicit none
    character(len=:), intent(out), allocatable :: trace
    type(prc_call), pointer :: x
    x => PRC_CALL_HEAD_
    allocate(trace, source=new_line('A'))
    do while (associated(x%next))
       trace = trace//"-> "//x%modname//"::"//x%prcname//new_line('A')
       x => x%next
    end do
  end subroutine trace_call_stack

  !> Flushes all the procedure call in the stack.
  !! @author Filippo Monari
  subroutine flush_call_stack ()
    implicit none
    do while (.true.)
       call prc_call__detach()
       if (.not. associated(PRC_CALL_NEXT_%prev)) exit
    end do
  end subroutine flush_call_stack

  !> Execute a block of code within a 'prc_call'.
  !! @author Filippo Monari
  !! @param[in] prcname character, name of the procedure calling the code block
  !! @param[in] modname character, name of the module containing the procedure
  !! @param[in] sbr code block provided a subroutine with no input parameters
  subroutine do_within (prcname, modname, sbr)
    implicit none
    character(len=*), intent(in) :: prcname, modname
    procedure(sbr0_)  :: sbr
    call prc_call__append(prcname, modname)
    call sbr
    call prc_call__detach()
  end subroutine do_within

  !> Execute a block of code wiithin a 'prc_call' only if the stack if error free.
  !! @author Filippo Monari
  !! @param[in] prcname character, name of the procedure calling the code block
  !! @param[in] modname character, name of the module containing the procedure
  !! @param[in] sbr code block provided a subroutine with no input parameters
  subroutine do_safe_within (prcname, modname, sbr)
    implicit none
    character(len=*), intent(in) :: prcname, modname
    procedure(sbr0_) :: sbr
    call prc_call__append(prcname, modname)
    call err_safe(sbr)
    call prc_call__detach()
  end subroutine do_safe_within

  !> Executes a block of code with a critical error section.
  !! Any produced within this section are considered critical and
  !! will cause the termiation of the program.
  !! @author Filippo Monari
  !! @param[in] sbr code block provided as a subroutine with no input parameters
  subroutine critical_error_section (sbr)
    implicit none
    procedure(sbr0_) :: sbr
    ISCRITICAL_ = .true.
    call sbr
    ISCRITICAL_ = .false.
  end subroutine critical_error_section

  !> Execute a block of code within a 'prc_call' considered a critical error section.
  !! @author Filippo Monari
  !! @param[in] prcname character, name of the procedure calling the code block
  !! @param[in] modname character, name of the module containing the procedure
  !! @param[in] sbr code block provided a subroutine with no input parameters
  subroutine do_within_critical (prcname, modname, sbr)
    implicit none
    character(len=*), intent(in) :: prcname, modname
    procedure(sbr0_) :: sbr
    call prc_call__append(prcname, modname)
    call critical_error_section(sbr)
    call prc_call__detach()
  end subroutine do_within_critical
  
  !> Compiles an error message given the error code and a custom message.
  !! For all the error types but 'err_generic_' the message should just be the
  !! name of variable producing the error.
  !! @author Filippo Monari
  !! @param[in] msg character, error message
  !! @param[in] info integer, error code
  function err__msg (msg, info) result(errmsg)
    implicit none
    character(len=*), intent(in) :: msg
    integer, intent(in) :: info
    character(len=:), allocatable :: errmsg
    select case (info)
    case (err_generic_)
       allocate(errmsg, source="[!] error: "//msg//".")
    case (err_alloc_)
       allocate(errmsg, source="[!] error: error in allocating "//msg//".")
    case (err_dealloc_)
       allocate(errmsg, source="[!] error: error in deallocating "//msg//".")
    case (err_unknwnVal_)
       allocate(errmsg, source="[!] error: unknown value of "//msg//".")
    case (err_wrngSz_)
       allocate(errmsg, source="[!] error: "//msg//" has wrong size/shape.")
    case (err_oorng_)
       allocate(errmsg, source="[!] error: "//msg//" is out of range.")
    case (err_alreadyAlloc_)
       allocate(errmsg, source="[!] error: "//msg//" already allocated.")
    case (err_notAlloc_)
       allocate(errmsg, source="[!] error: "//msg//" not allocated.")
    case (err_alreadyAssoc_)
       allocate(errmsg, source="[!] error: "//msg//" already associated.")
    case (err_notAssoc_)
       allocate(errmsg, source="[!] error: "//msg//" not associated.")
    case (err_alreadyInit_)
       allocate(errmsg, source="[!] error: "//msg//" already initialised.")
    case (err_notInit_)
       allocate(errmsg, source="[!] error: "//msg//" not intialised.")
    case (err_missingArg_)
       allocate(errmsg, source="[!] error: argument "//msg//" is missing.")
    case (err_wrngArg_)
       allocate(errmsg, source="[!] error: argument "//msg//" is wrong.")
    case (err_wrngTyp_)
       allocate(errmsg, source="[!] error: "//msg//" is wrong type.")
    case (err_lapack_)
       allocate(errmsg, source="[!] error: lapack subroutine "//msg)
    end select
  end function err__msg

  !> Compiles a warning message given the warning code and a custom message.
  !! For all the warning types but 'warn_generic_' the message should just be the
  !! name of variable producing the error.
  !! @author Filippo Monari
  !! @param[in] msg character, warning message
  !! @param[in] info integer, warning code
  function warn_msg (msg, info) result(warnmsg)
    implicit none
    character(len=*), intent(in) :: msg
    integer, intent(in) :: info
    character(len=:), allocatable :: warnmsg
    select case (info)
    case (warn_generic_)
       allocate(warnmsg, source="[?] warning: "//msg//".")
    case (warn_hasdx_)
       allocate(warnmsg, source="[?] warning: "//msg//" has dx.")
    end select
  end function warn_msg
  
  !> Raises an error.
  !! @author Filippo Monari
  !! @param[in] infomsg character, error message
  !! @param[in] infocode integer, error code
  subroutine raise_error (infomsg, infocode)
    use iso_c_binding
    implicit none
    character(len=*), intent(in) :: infomsg
    integer, intent(in) :: infocode
    character(len=:), allocatable :: err_msg
    INFO_ = infocode
    call trace_call_stack(err_msg)
    err_msg = err_msg//err__msg(infomsg, INFO_)
    call flush_call_stack()
    INFO_ = 0
    call r__error(err_msg//C_NULL_CHAR)
  end subroutine raise_error

  !> Raises a warning.
  !! @param[in] infomsg character, warning message
  !! @param[in] infocode integer, warning code
  subroutine raise_warning (infomsg, infocode)
    use iso_c_binding
    implicit none
    character(len=*), intent(in) :: infomsg
    integer, intent(in) :: infocode
    character(len=:), allocatable :: wrn_msg
    !WINFO_ = infocode
    call trace_call_stack(wrn_msg)
    wrn_msg = wrn_msg//warn_msg(infomsg, infocode)
    call r__warn(wrn_msg//C_NULL_CHAR)
  end subroutine raise_warning

  !> Check if the stack is free from errors.
  !! @author Filippo Monari
  function err_free () result(ans)
    implicit none
    logical :: ans
    ans = INFO_ == 0
  end function err_free

  !> Execute a prcedure only if the stack is error free.
  !! @author Filippo Monari
  !! @param sbr procedure(sbr0_), subroutine defining the procedure to execute
  subroutine err_safe (sbr)
    implicit none
    procedure(sbr0_) :: sbr
    if (err_free()) call sbr()
  end subroutine err_safe
  
  !> Assert that the a condition is true, otherwise raise and error.
  !! @author Filippo Monari
  !! @param[in] cond logical, codition to assert
  !! @param[in] info integer, error code to raise
  !! @param[in] msg character, error message
  subroutine assert (cond, info, msg)
    implicit none
    logical, intent(in) :: cond
    integer, intent(in) :: info
    character(len=*) :: msg
    if (err_free()) then
       if (.not. cond) call raise_error(msg, info)
    end if
  end subroutine assert

  !> Warns if the condition is true.
  !! @author Filippo Monari
  !! @param[in] cond logical, codition to assert
  !! @param[in] info integer, error code to raise
  !! @param[in] msg character, error message
  subroutine warn (cond, info, msg)
    implicit none
    logical, intent(in) :: cond
    integer, intent(in) :: info
    character(len=*) :: msg
    if (err_free()) then
       if (cond) call raise_warning(msg, info)
    end if
  end subroutine warn
  
end module errwarn

  
