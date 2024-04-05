
SUBROUTINE checkmem( mstatus, onvar, caller )

!***********************************************************************
!  subroutine body starts at line  105

!  DESCRIPTION:
!       Reports an error and exits if memory status flag is non-zero.

!  PRECONDITIONS REQUIRED:

!  SUBROUTINES AND FUNCTIONS CALLED:

!  REVISION  HISTORY:
!       Adapted 10/98 by M Houyoux

!***********************************************************************

! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
! File: @(#)$Id: checkmem.f,v 1.2 1999/11/23 12:46:12 mhouyoux Exp $

! COPYRIGHT (C) 1999, MCNC--North Carolina Supercomputing Center
! All Rights Reserved

! See file COPYRIGHT for conditions of use.

! Environmental Programs Group
! MCNC--North Carolina Supercomputing Center
! P.O. Box 12889
! Research Triangle Park, NC  27709-2889

! env_progs@mcnc.org

! Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/edss_tools/edss_tools/src/lib/checkmem.f,v $
! Last updated: $Date: 1999/11/23 12:46:12 $

!***************************************************************************
IMPLICIT NONE

INTEGER, INTENT(IN)                      :: mstatus
CHARACTER (LEN=*), INTENT(IN)            :: onvar
CHARACTER (LEN=*), INTENT(IN)            :: caller


!...........   ARGUMENTS and their descriptions:

!...........   ARGUMENTS and their descriptions:
INTEGER :: trimlen
EXTERNAL     trimlen

!...........   Local variables

INTEGER :: l1
INTEGER :: l2
CHARACTER (LEN=256) :: mesg

CHARACTER (LEN=16) :: progname = 'CHECKMEM' ! program name

!***********************************************************************
!   begin body of function CHECKMEM

!.........  Get lengths of input character strings
l1 = trimlen( onvar )
l2 = trimlen( caller )

!.........  Abort if memory status is non-zero

IF( mstatus > 0 ) THEN
  mesg = 'Failure allocating memory for "' // onvar( 1:l1 ) // '" variable'
  CALL m3exit( caller( 1:l2 ), 0, 0, mesg, 2 )
END IF

RETURN

END SUBROUTINE checkmem

