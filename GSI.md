
# Modern Fortran code snippets from GSI  (a total of 2)

(FYI, the GSI repository can be found at https://github.com/noaa-emc/GSI. The code challenge will only cover the following 2 code snippets)

## 1. Adapted from m_obsdiagNode.F90

```
module m_obsdiagNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_obsdiagNode
! abstract: module of node type obs_diag and linked-list type obs_diags.
!
! program history log:
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block  
!
  use kinds , only: i_kind,r_kind
  use mpeu_util, only: assert_,tell,warn,perr,die
  implicit none

  private

  public:: obs_diag
  public:: obs_diags
  public:: fptr_obsdiagNode

        ! Primery behaviors:
  public:: obsdiagLList_reset   ! destructor + initializer
  public:: obsdiagLList_appendNode
  public:: obsdiagLList_rewind  ! rewind an obsdiagLList
  public:: obsdiagLList_nextNode

  public:: obsdiagLList_headNode
  public:: obsdiagLList_tailNode

  public:: obsdiagLList_read    ! reader, for input
  public:: obsdiagLList_write   ! writer, for otuput
  public:: obsdiagLList_lsize   ! size inquiry
  public:: obsdiagLList_lcount  ! size inquiry with recount
  public:: obsdiagLList_lsort   ! sort nodes according to their keys
  public:: obsdiagLList_checksum! size consistency checking
  public:: obsdiagLList_summary ! status report

        interface obsdiagLList_reset ; module procedure  lreset_; end interface
        interface obsdiagLList_rewind; module procedure lrewind_; end interface
        interface obsdiagLList_read  ; module procedure   lread_; end interface
        interface obsdiagLList_checksum; module procedure &
          lchecksum_  , &
          lchecksum1_ , &
          lchecksum2_ ; end interface
        interface obsdiagLList_lsize  ; module procedure lsize_   ; end interface
        interface obsdiagLList_lcount ; module procedure lcount_  ; end interface
        interface obsdiagLList_lsort  ; module procedure lsort_   ; end interface
        interface obsdiagLList_write  ; module procedure lwrite_  ; end interface
        interface obsdiagLList_summary; module procedure lsummary_; end interface

        interface obsdiagLList_appendNode; module procedure obsNode_append_; end interface
        interface obsdiagLList_nextNode  ; module procedure &
          obsNode_next_, &
          make_or_next_; end interface

        interface obsdiagLList_headNode  ; module procedure lheadNode_  ; end interface
        interface obsdiagLList_tailNode  ; module procedure ltailNode_  ; end interface

        ! Node lookup, secondary function with its searching component
  public:: obsdiagLookup_build  ! setup, its searching component
  public:: obsdiagLookup_locate ! node lookup, with the searching component
  public:: obsdiagLookup_clean  ! clean, its searching component

        interface obsdiagLookup_build ; module procedure lbuild_; end interface
        interface obsdiagLookup_locate; module procedure locate_; end interface
        interface obsdiagLookup_clean ; module procedure lclean_; end interface

  public:: obsdiagLList_dump
        interface obsdiagLList_dump; module procedure ldump_; end interface
  public:: obsdiagNode_init
  public:: obsdiagNode_assert
  public:: obsdiagNode_set
  public:: obsdiagNode_get
        interface obsdiagNode_init  ; module procedure obsNode_init_; end interface
        interface obsdiagNode_assert; module procedure anode_assert_; end interface
        interface obsdiagNode_set   ; module procedure obsNode_set_ ; end interface
        interface obsdiagNode_get   ; module procedure obsNode_get_ ; end interface

  type obs_diag
     type(obs_diag), pointer :: next => NULL()
     real(r_kind), pointer :: nldepart(:) => null()    ! (miter+1)
     real(r_kind), pointer :: tldepart(:) => null()    ! (miter)
     real(r_kind), pointer :: obssen(:)   => null()    ! (miter)
     real(r_kind) :: wgtjo
     real(r_kind) :: elat, elon         ! earth lat-lon for redistribution
     integer(i_kind) :: idv,iob,ich     ! device, obs., and channel indices
     logical, pointer :: muse(:)          => null()    ! (miter+1), according the setup()s
     logical :: luse
  end type obs_diag

  type fptr_obsdiagNode         ! Fortran array element of a type(obs_diag) pointer
    type(obs_diag),pointer:: ptr => null()
  end type fptr_obsdiagNode

  type:: obs_diags
     integer(i_kind):: n_alloc=0
     type(obs_diag), pointer :: head => NULL()
     type(obs_diag), pointer :: tail => NULL()
     type(fptr_obsdiagNode), allocatable, dimension(:):: lookup
  end type obs_diags

#include "myassert.H"
#include "mytrace.H"
  character(len=*),parameter:: myname="m_obsdiagNode"

#define _obsNode_   obs_diag
#define _obsLList_  obs_diags

contains
function lheadNode_(headLL) result(here_)
! Return the head node
  implicit none
  type(_obsNode_),pointer:: here_
  type(_obsLList_),target,intent(in):: headLL
  here_ => headLL%head
end function lheadNode_

.....

end module m_obsdiagNode
```

## 2. adapted from gsi_bundlemod.F90
```
module GSI_BundleMod
   use kinds, only: i_kind,r_single,r_kind,r_double,r_quad
   use constants, only: zero_single,zero,zero_quad
   use m_rerank, only: rerank
   use mpeu_util, only: perr, die
   public GSI_BundleGetPointer ! Get pointer to variable

   interface GSI_BundleGetPointer    ! get pointer to field(s) in bundle
          module procedure get1_     !   single-field 
          module procedure get2_     !   many-field
          module procedure get31r4_  !   real*4 rank-1 explict pointer (real*4)
          module procedure get31r8_  !   real*8 rank-1 explict pointer (real*8)
          module procedure get32r4_  !   real*4 rank-2 explict pointer (real*8)
          module procedure get32r8_  !   real*8 rank-2 explict pointer (real*4)
          module procedure get33r4_  !   real*4 rank-3 explict pointer (real*4)
          module procedure get33r8_  !   real*8 rank-3 explict pointer (real*8)
   end interface

  subroutine get0_ ( Bundle, fldname, ipnt, istatus, irank, ival )
  .......
    integer(i_kind) :: i, n1d, n2d, n3d

    istatus=0
    n1d = Bundle%n1d
    n2d = Bundle%n2d
    n3d = Bundle%n3d
    ipnt=-1; irank=-1; ival=-1
    do i=1,n1d
       if (trim(fldname).eq.trim(Bundle%r1(i)%shortname)) then
          ipnt=i
          irank=1
          ival=Bundle%ival1(i)
          return
       endif
    enddo
    do i=1,n2d
       if (trim(fldname).eq.trim(Bundle%r2(i)%shortname)) then
          ipnt=i
          irank=2
          ival=Bundle%ival2(i)
          return
       endif
    enddo
    do i=1,n3d
       if (trim(fldname).eq.trim(Bundle%r3(i)%shortname)) then
          ipnt=i
          irank=3
          ival=Bundle%ival3(i)
          return
       endif
    enddo

    if(ipnt<0) istatus=1
  end subroutine get0_


  subroutine get1_ ( Bundle, fldname, ipnt, istatus, irank, ival )
! !INPUT PARAMETERS:

    type(GSI_Bundle),intent(in) :: Bundle
    character(len=*),intent(in) :: fldname        ! required field name

! !OUTPUT PARAMETERS:

    integer(i_kind),intent(out) :: ipnt           ! actual pointer to individual field
    integer(i_kind),intent(out) :: istatus        ! status error code
    integer(i_kind),OPTIONAL,intent(out) :: irank ! field rank (e.g., 1, or 2, or 3)
    integer(i_kind),OPTIONAL,intent(out) :: ival  ! optional pointer to long vector form
    integer(i_kind) :: irank_
    integer(i_kind) :: ival_

    istatus=0
    call get0_ ( Bundle, fldname, ipnt, istatus, irank_, ival_ )
    if(present(irank)) then
       irank=irank_
    endif
    if(present(ival)) then
       ival=ival_
    endif

  end subroutine get1_

end module GSI_BundleMod

```
