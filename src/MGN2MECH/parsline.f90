
SUBROUTINE parsline( line, n, segment )

!***********************************************************************
!  subroutine body starts at line

!  DESCRIPTION:
!      This subroutine separates a "list-formatted" line of strings in which
!      the segments may or may not have quotes.  Although fortran requires
!      the quotes for true list-formatting, this subroutine can be used when
!      the quotes are only present to enclose a character (such as space, comma,
!      or semi-colon) that would otherwise be a delimiter.  If an "!" is
!      encountered, everything after it is treated as a comment.

!  PRECONDITIONS REQUIRED:

!  SUBROUTINES AND FUNCTIONS CALLED:

!  REVISION  HISTORY:
!      Created by M. Houyoux 3/99

!****************************************************************************/

! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
! File: @(#)$Id: parsline.f,v 1.10 2003/05/02 13:46:31 cas Exp $

! COPYRIGHT (C) 2000, MCNC--North Carolina Supercomputing Center
! All Rights Reserved

! See file COPYRIGHT for conditions of use.

! Environmental Programs Group
! MCNC--North Carolina Supercomputing Center
! P.O. Box 12889
! Research Triangle Park, NC  27709-2889

! env_progs@mcnc.org

! Pathname: $Source: /afs/isis/depts/cep/emc/apps/archive/edss_tools/edss_tools/src/lib/parsline.f,v $
! Last updated: $Date: 2003/05/02 13:46:31 $

!***************************************************************************
IMPLICIT NONE
CHARACTER (LEN=*),intent (in) :: line         ! character string to parse
INTEGER , intent (in) :: n            ! maximum array length
CHARACTER (LEN=*),intent(out) :: segment( n ) ! parsed string
!...........   EXTERNAL FUNCTIONS
CHARACTER (LEN=2) :: crlf
INTEGER :: findc

EXTERNAL    crlf, findc

!...........   Local parameters
INTEGER , PARAMETER :: ndelim = 4
CHARACTER (LEN=1), PARAMETER :: delimlst( ndelim ) =  &
    (/ ',', ' ', ';', ' ' /)

!...........   Array of 1-char strings for processing
CHARACTER (LEN=1) :: arrstr( 5120 )  ! 256 * 20

!...........  Arrays for sorting non-delimiters on a per-machine basis
INTEGER :: ndindx  ( ndelim )
CHARACTER (LEN=1), SAVE :: delimsrt( ndelim )

!...........   Other local variables
INTEGER :: i, j, l, l1, l2  !  counters and indices
INTEGER :: ixp              !  index to non-delimeters
INTEGER :: ncnt             !  count of fields

LOGICAL :: alpha            !  true when within alpha-numeric
LOGICAL :: delim            !  true when within or past delimiter
LOGICAL,SAVE :: firstime = .true.!  true first time routine is called
LOGICAL :: prevdelim = .true. !  true when last char was a delim
LOGICAL :: NUMBER           !  true when within number in string
LOGICAL :: quoted           !  true when within quotes in string
LOGICAL :: thisnmbr         !  true when current iteration is numbr

CHARACTER (LEN=1) :: cbuf             !  temporary buffer
CHARACTER (LEN=1) :: doubleq = '"'
CHARACTER (LEN=1) :: singleq = "'"
CHARACTER (LEN=1) :: period  = '.'
CHARACTER (LEN=1) :: quotval          !  value of starting quote

CHARACTER (LEN=300) :: mesg             ! message buffer

CHARACTER (LEN=16) :: progname = 'PARSLINE' ! program name

!***********************************************************************
!   begin body of subroutine PARSLINE

!.........  The first time the routine is called, sort the list of delimiters
IF( firstime ) THEN
  DO i = 1, ndelim
    ndindx( i ) = i
  END DO
  
  CALL sortic( ndelim, ndindx, delimlst )
  
  DO i = 1, ndelim
    j = ndindx( i )
    delimsrt( i ) = delimlst( j )
  END DO
  
  firstime = .false.
  
END IF

l2 = len_trim( line )

!.........  Check for comments, and use to set the end of the line
l = INDEX( line( 1:l2 ), '!' )

IF( l <= 0 ) THEN
  l = l2
ELSE
  l = l - 1
END IF

!.........  Skip blank lines
IF( l == 0 ) RETURN

!.........  Initialize count, flags, and segments (npte, initializing in
!           the variable definitions is insufficient)
ncnt    = 0
segment = ' ' ! array
alpha   = .false.
delim   = .true.
NUMBER  = .false.
quoted  = .false.

!.........  Process LINE 1-character at a time
DO i = 1, l
  
  cbuf = line( i:i )
  
!.............  Look for character in delimiters
  ixp = findc( cbuf, ndelim, delimsrt )
  
!.............  Evaluate the current character for number or not
  thisnmbr = ( cbuf >= '0' .AND. cbuf <= '9' )
  
!.............  Waiting for next field...
  IF( delim ) THEN
    
    NUMBER = thisnmbr
    alpha  = ( .NOT. NUMBER .AND. ixp <= 0 )
    
    IF( cbuf == singleq ) THEN
      quoted  = .true.
      delim   = .false.
      quotval = singleq
      prevdelim = .false.
      l1     = i + 1
      ncnt    = ncnt + 1
      
    ELSE IF( cbuf == doubleq ) THEN
      quoted  = .true.
      delim   = .false.
      quotval = doubleq
      prevdelim = .false.
      l1      = i + 1
      ncnt    = ncnt + 1
      
    ELSE IF( alpha ) THEN
      delim = .false.
      prevdelim = .false.
      l1    = i
      ncnt  = ncnt + 1
      
    ELSE IF( NUMBER ) THEN
      delim  = .false.
      prevdelim = .false.
      l1     = i
      ncnt   = ncnt + 1
      
!...............  If another delimeter, then another field, but last
!                 field was blank UNLESS delim is a space
    ELSE IF( cbuf /= delimlst( 2 ) ) THEN
      
      IF( prevdelim ) THEN
        ncnt = ncnt + 1
      ELSE
        prevdelim = .true.
      END IF
      
    END IF  ! Else its a space delimiter
    
!.............  In a quoted field, skip everything unless it is an end quote
  ELSE IF( quoted ) THEN
    
    IF( cbuf == quotval ) THEN
      quoted  = .false.
      delim   = .true.
      prevdelim = .false.
      l2      = i - 1
      
      CALL store_segment
      
    END IF
    
!.............  If start of field was a number, but adjacent character is not
!               a delimiter, then turn field into an alpha
  ELSE IF( NUMBER .AND. .NOT. thisnmbr .AND. ixp <= 0 ) THEN
    alpha  = .true.
    NUMBER = .false.
    
!.............  If start of field was a number or alpha, and this is a
!               delimiter, then end of number has been reached
  ELSE IF( ixp > 0 ) THEN
    alpha = .false.
    NUMBER = .false.
    delim  = .true.
    prevdelim = .true.
    l2     = i - 1
    
    CALL store_segment
    
  END IF
  
END DO

!.........  Store final segment
IF( cbuf == quotval ) l = l - 1
l2 = l

IF( ixp <= 0 ) CALL store_segment

RETURN

!******************  FORMAT  STATEMENTS   ************************************

!...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( a, :, i8, :, 1X ) )

!******************  INTERNAL SUBPROGRAMS  *****************************

contains

!.............  This subprogram stores the segment from the input string
SUBROUTINE store_segment

IF( ncnt <= n ) THEN
  
  segment( ncnt ) = adjustl( line( l1:l2 ) )
  
ELSE
  
  mesg = 'ERROR: Overflow prevented while '// 'parsing line ' // progname
  CALL m3msg2( mesg )
  mesg = 'First 200 characters of line contents are:'
  CALL m3msg2( mesg )
  mesg = line( 1:200 )
  CALL m3msg2( mesg )
  
  mesg = 'Formatting problem.'
  CALL m3exit( progname, 0, 0, mesg, 2 )
  
END IF

END SUBROUTINE store_segment

END SUBROUTINE parsline
