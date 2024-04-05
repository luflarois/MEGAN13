
PROGRAM txt2ioapi
!***********************************************************************
!  This program converts comma deliminated line input to gridded
!    netCDF-IOAPI format.
!  The grid parameter is input from GRIDDESC
!  Program always takes second and third input column as grid-x
!    and grid-y index.
!  If you export the file from Excel to .csv file, make sure you convert
!    the DOS text file to Unix text file with "dos2unix" command.
!
!  Requirement:
!     Requires libnetcdf.a and libioapi.a to compile
!     Also, parsline.o and chkmetem.o from SMOKE
!
!     setenv EFSTXTF <ef_map text file>
!     setenv PFTTXTF <pft    text file>
!     setenv LAITXTF <lai    text file>
!     setenv EFMAPS  <output EFS netCDF-ioapi file>
!     setenv PFTS16  <output PFT netCDF-ioapi file>
!     setenv LAIS46  <output LAI netCDF-ioapi file>
!     setenv RUN_EFS <T|F>
!     setenv RUN_LAI <T|F>
!     setenv RUN_PFT <T|F>
!     setenv GRIDDESC <grid desc file>
!
!  Call:
!     PARSLINE
!     CHECKMEM
!
!  Created by Jack Chen 11/04
!  Modified by Tan 11/13/06 for MEGAN v2.0
!
!  History:
!  08/14/07 Tan    - Move to MEGANv2.02 with no update
!  07/06/11 Tan    - Update for MEGANv2.10
!***********************************************************************
USE PARMS3    !  I/O API parameters
USE IODECL3   !  I/O API function declarations
USE FDESC3    !  I/O API file description data structures
USE SPC_MGN   !  MEGAN species
USE LAI_MGN   !  LAI
USE PFT_MGN   !  PFT
IMPLICIT NONE

!...........   EXTERNAL FUNCTIONS and their descriptions:
INTEGER :: promptffile, str2int
REAL :: str2real
LOGICAL :: dscgrid
LOGICAL :: envyn
INTEGER :: index1
EXTERNAL     promptffile, dscgrid, str2int, str2real, index1

!.........  LOCAL VARIABLES and their descriptions:
INTEGER :: ios            ! i/o status
INTEGER :: ldev           ! unit no. for log file
CHARACTER (LEN=256) :: mesg        ! message buffer

LOGICAL :: run_efs
LOGICAL :: run_pft
LOGICAL :: run_lai

CHARACTER (LEN=16) :: efstxtf = 'EFSTXTF'      ! Input EF file logical name
CHARACTER (LEN=16) :: pfttxtf = 'PFTTXTF'      ! Input PFT file logical name
CHARACTER (LEN=16) :: laitxtf = 'LAITXTF'      ! Input LAI file logical name
CHARACTER (LEN=16) :: efmaps = 'EFMAPS'     ! Output EF file logical name
CHARACTER (LEN=16) :: pfts16 = 'PFTS16'     ! Output PFT file logical name
CHARACTER (LEN=16) :: lais46 = 'LAIS46'     ! Output LAI file logical name

CHARACTER (LEN=16) :: gdnam

CHARACTER (LEN=16) :: progname = 'TXT2IOAPI'

!***********************************************************************
!... Begin program
!... Initialize log file unit
ldev = init3()

!... Get output file grid parameters
mesg = 'Coordinate name: '
CALL envstr( 'GDNAM3D', mesg, 'ASACA36km', gdnam, ios )

!... Convert EFS
run_efs = envyn( 'RUN_EFS','Run TXT to IOAPI for EFS', .true.,ios)
IF (ios > 0) THEN
  STOP 'Bad value for RUN_EFS'
END IF
IF (run_efs) THEN
  CALL cnvt_efs(gdnam,efstxtf,efmaps)
END IF

!... Convert PFT
run_pft = envyn( 'RUN_PFT','Run TXT to IOAPI for PFT', .true.,ios)
IF (ios > 0) THEN
  STOP 'Bad value for RUN_PFT'
END IF
IF (run_pft) THEN
  CALL cnvt_pft(gdnam,pfttxtf,pfts16)
END IF

!... Convert LAI
run_lai = envyn( 'RUN_LAI','Run TXT to IOAPI for LAI', .true.,ios)
IF (ios > 0) THEN
  STOP 'Bad value for RUN_LAI'
END IF
IF (run_lai) THEN
  CALL cnvt_lai(gdnam,laitxtf,lais46)
END IF

!... Exit and close file
CALL m3exit(progname,0,0,' ',0)

endprogram
