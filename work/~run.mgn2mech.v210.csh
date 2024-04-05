#!/bin/bash
########################################################################
source ../setcase.bash
## Directory setups
export PRJ='USA' 
export PROMPTFLAG='N'

# Program directory
export PROG='mgn2mech.wmap'
export EXEDIR=${MGNEXE}
export EXE=${EXEDIR}'/'${PROG}

# Input map data directory
export INPDIR $MGNINP/MAP

# Intermediate file directory
export INTDIR $MGNOUT/INT

# Output directory
export OUTDIR $MGNOUT

# MCIP input directory
export METDIR $MGNINP/MGNMET

# Log directory
export LOGDIR $MGNLOG/$PROG
if ( ! -e $LOGDIR ) mkdir -p $LOGDIR
########################################################################

set dom = 36
set JD = 2008201
while ($JD <= 2008201)
########################################################################
# Set up time and date to process
export SDATE $JD        #start date
export STIME 0
export RLENG 240000
export TSTEP 10000
########################################################################

########################################################################
# Set up for MECHCONV
export RUN_SPECIATE   Y    # run MG2MECH

export RUN_CONVERSION Y    # run conversions?
                           # run conversions MEGAN to model mechanism
                           # units are mole/s

export SPCTONHR       N    # speciation output unit in tonnes per hour
                           # This will convert 138 species to tonne per
                           # hour or mechasnim species to tonne per hour.
                           
# If RUN_CONVERSION is set to "Y", one of mechanisms has to be selected.
export MECHANISM    RADM2
#export MECHANISM    RACM
#export MECHANISM    CBMZ
#export MECHANISM    CB05
#export MECHANISM    CB6
#export MECHANISM    SOAX
#export MECHANISM    SAPRC99
#export MECHANISM    SAPRC99Q
#export MECHANISM    SAPRC99X

# Grid name
export GDNAM3D USA${dom}km 

# EFMAPS NetCDF input file
export EFMAPS  $INPDIR/EFMAPS.${PRJ}${dom}.ncf

# PFTS16 NetCDF input file
export PFTS16  $INPDIR/PFTS16.${PRJ}${dom}.ncf

# MEGAN ER filename
export MGNERS $INTDIR/ER.$GDNAM3D.${SDATE}.ncf

# Output filename
export MGNOUT $OUTDIR/MEGANv2.10.$GDNAM3D.$MECHANISM.$SDATE.ncf

########################################################################
## Run speciation and mechanism conversion
if ( $RUN_SPECIATE == 'Y' ) then
   rm -f $MGNOUT
   $EXE | tee $LOGDIR/log.run.$PROG.$GDNAM3D.$MECHANISM.$SDATE.txt
endif

@ JD++
end  # End while JD
