
SUBROUTINE cnvt_efs(gdnam,efstxtf,efmaps)
!***********************************************************************
!  This subroutine converts EFS txt to EFS IOAPI
!
!  Called by TXT2IOAPI
!
!  Call:
!     PARSLINE
!     CHECKMEM
!
!  Created by Tan  07/07/2011 for MEGAN v2.10
!
!  History:
!***********************************************************************
!...........   INCLUDES:
USE PARMS3    !  I/O API parameters
USE IODECL3   !  I/O API function declarations
USE FDESC3    !  I/O API file description data structures
USE SPC_MGN   !  MEGAN species

IMPLICIT NONE

!...........   EXTERNAL FUNCTIONS and their descriptions:
INTEGER :: promptffile, str2int
REAL :: str2real
LOGICAL :: dscgrid
INTEGER :: index1
EXTERNAL     promptffile, dscgrid, str2int, str2real, index1

!.........  LOCAL VARIABLES and their descriptions:
INTEGER :: ios            ! i/o status
INTEGER :: iline          ! current line
INTEGER :: adev           ! unit no. for input data file
INTEGER :: i, j, s, n     ! Counters
CHARACTER (LEN=16) :: cname        ! Coord name
CHARACTER (LEN=10000) line     ! input ine buffer
CHARACTER (LEN=256) :: mesg        ! message buffer
INTEGER :: cid, inx, iny  ! Input grid x and y

CHARACTER (LEN=16) :: efstxtf
CHARACTER (LEN=16) :: efmaps
CHARACTER (LEN=16) :: gdnam

! CID, I, J, LAT, LONG, SR, TEMP, 20 EFS
INTEGER,PARAMETER :: mxtcol = 27      ! Columns in an input line
CHARACTER (LEN=30) :: segment( mxtcol )     ! Input line fields
INTEGER :: xref(mxtcol)

INTEGER,PARAMETER :: nvars = 24       ! Number of output variables
CHARACTER (LEN=16) :: vname( nvars ) ! Output variables

REAL,allocatable :: ibuff( :, :, : )  ! input, output buffer

CHARACTER (LEN=16) :: progname = 'CNVT_EFS'

!***********************************************************************
!... Begin program

!... Get output file grid parameters
gdnam3d = gdnam
IF( .NOT. dscgrid( gdnam3d, cname, gdtyp3d,  &
      p_alp3d, p_bet3d, p_gam3d, xcent3d, ycent3d,  &
      xorig3d, yorig3d, xcell3d, ycell3d, ncols3d, nrows3d, nthik3d ) ) THEN
  mesg = 'Could not get grid description.'
  CALL m3exit ( progname, 0, 0, mesg, 2 )
END IF
PRINT*,'NCOLS3D ',gdnam3d, cname, gdtyp3d,  &
    p_alp3d, p_bet3d, p_gam3d, xcent3d, ycent3d,  &
    xorig3d, yorig3d, xcell3d, ycell3d, ncols3d, nrows3d, nthik3d

!... Allocate memory
allocate ( ibuff( nvars, ncols3d, nrows3d ), stat = ios )
CALL checkmem ( ios, 'IBUFF', progname )
ibuff = 1.0

!... Get input file unit
mesg = 'Enter logical name of EFS data file.'
adev = promptffile( mesg, .true., .true., efstxtf, progname )
IF( adev < 0 ) THEN
  mesg = 'Error opening input EFS file.'
  CALL m3exit ( progname, 0, 0, mesg, 2 )
END IF

!... Set VNAME
DO n = 1,n_mgn_spc
  vname( n+4 ) = 'EF_'//trim(mgn_spc(n))
END DO

!... Read TXT input file
iline = 0
DO
  READ( adev, 1000, IOSTAT = ios ) line
  iline = iline + 1
  IF( ios < 0 ) EXIT       ! End of file, exit loop
  IF( line == ' ' ) cycle  ! Skip blank line
  IF( ios > 0 ) THEN
    WRITE( mesg, 1010 ) 'I/O error', ios, 'reading input file at line', iline
    CALL m3exit( progname, 0, 0, mesg, 2 )
  END IF
  
  CALL parsline( line, mxtcol, segment )  ! break lines into segments
  IF ( iline == 1 ) THEN
! Assumption
! Column 1 : CID
! Column 2 : ICELL
! Column 3 : JCELL
! Column 4 : LAT
! Column 5 : LONG
! column 6 : SR
! Column 7 : TEMP
    DO s = 1 , mxtcol - 3
      mesg = segment( s + 3 )
      CALL upcase( mesg )  ! to all caps
      IF (       trim( mesg ) == 'lat'    ) THEN
        xref( s ) = 1
        vname( xref(s) ) = trim( mesg )
      ELSE IF (  trim( mesg ) == 'long'   ) THEN
        xref( s ) = 2
        vname( xref(s) ) = trim( mesg )
      ELSE IF (  trim( mesg ) == 'dsrad'  ) THEN
        xref( s ) = 3
        vname( xref(s) ) = trim( mesg )
      ELSE IF (  trim( mesg ) == 'dtemp'  ) THEN
        xref( s ) = 4
        vname( xref(s) ) = trim( mesg )
      ELSE IF ( index1(trim( mesg ),n_mgn_spc ,mgn_spc)  &
             /= 0) THEN
        n = index1(trim( mesg ),n_mgn_spc ,mgn_spc)
        xref( s ) = n + 4
        vname( xref(s) ) = 'EF_'//trim( mesg )
      ELSE
        print *,trim(mesg)//' is not valid'
        STOP 
      END IF
      
      PRINT*,'READ VNAME:',vname(xref(s))
    END DO
    cycle  ! go back to read 2nd line
  END IF
  
  cid = str2int( segment(1) )   ! convert character to integer
  inx = str2int( segment(2) )   ! convert character to integer
  iny = str2int( segment(3) )   ! convert character to integer
  PRINT*,'CID ',cid,inx,iny,xref
  DO s = 1, 4
    ibuff(s,inx,iny) = str2real( segment( 3+s ) )        ! convert char to real
    PRINT*,'IBUFF ',s,ibuff(s,inx,iny)
  END DO
  DO s = 5, mxtcol - 3
    ibuff(xref(s),inx,iny) = str2real( segment( 3+s ) )  ! convert char to real
    PRINT*,'IBUFF ',s,ibuff(s,inx,iny)
  END DO
END DO

WRITE( mesg, 1010 ) 'Total number of input lines read:',iline
CALL m3mesg( mesg )

!... Setup additional outfile parameters and open outfile
ftype3d = grdded3
nvars3d = nvars
vname3d(1:nvars) = vname(1:nvars)
vtype3d(1:nvars) = m3real
tstep3d = 0 ! time independent
nlays3d = 1
CALL nameval ( efstxtf, mesg )  ! get input file name
fdesc3d( 1 ) = 'Converted from MEGAN input text file: '
fdesc3d( 2 ) = trim(mesg)
DO s = 1, nvars
! For LAT, and LONG
  IF ( INDEX( vname3d( s ), 'LAT'      ) == 1 ) units3d( s ) = 'DEGREE'
  IF ( INDEX( vname3d( s ), 'LONG'     ) == 1 ) units3d( s ) = 'DEGREE'
! For daily SRAD, and daily TEMP
  IF ( INDEX( vname3d( s ), 'D_SRAD'   ) == 1 ) units3d( s ) = 'Watt/m2'
  IF ( INDEX( vname3d( s ), 'D_TEMP'   ) == 1 ) units3d( s ) = 'K'
! For MEGAN species
  DO n = 1, n_mgn_spc
    IF ( INDEX( vname3d( s ), 'EF_'//mgn_spc( n ) ) == 1 )  &
        units3d( s ) = 'ug'//mgn_spc( n )//'/m2.hr'
  END DO
  
END DO
IF ( .NOT. open3( efmaps, fscrea3, progname ) ) THEN
  mesg = 'Could not open file "'//trim(efmaps)//'" for output'
  CALL m3exit( progname, 0, 0, mesg, 2 )
END IF

!... Write output
DO s = 1, nvars
  PRINT*,'min cell value for:',trim(vname3d(s)),minval(ibuff(s,:,:))
  PRINT*,'max cell value for:',trim(vname3d(s)),maxval(ibuff(s,:,:))
  IF ( .NOT. write3(efmaps,vname3d(s),0,0,ibuff(s,:,:))) THEN
    mesg = 'Error writing to  file "'//trim(efmaps)//'"'
    CALL m3exit(progname,0,0,mesg,2)
  END IF
END DO

IF ( close3(efmaps) ) THEN
  CALL m3mesg( 'Closing EFS IOAPI file')
END IF
CLOSE(adev)

1000  FORMAT( 40A )
1010  FORMAT( 40( a, :, i8, :, 1X ) )

END SUBROUTINE
