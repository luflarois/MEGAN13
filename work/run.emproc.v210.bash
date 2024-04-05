#!/bin/bash
########################################################################
source ../setcase.bash
## Directory setups
export PRJ='USA' 
export PROMPTFLAG='N'

# Program directory
export PROG='emproc'
export EXEDIR=$MGNEXE
export EXE=$EXEDIR'/'$PROG

# Input map data directory
export INPDIR=$MGNINP'/MAP'

# MCIP input directory
export METDIR=$MGNINP'/MGNMET'

# Intermediate file directory
export INTDIR=$MGNINT

# Output directory
export OUTDIR=$MGNOUT

# Log directory
export LOGDIR=$MGNLOG'/'$PROG
if [ ! -e $LOGDIR ] 
  then
    mkdir -p $LOGDIR
fi
########################################################################

DOM=36 
JD=2008201

while [ $JD -le 2008201 ]
do
   ########################################################################
   # Set up time and date to process
   export SDATE=$JD        #start date
   export STIME='0'
   export RLENG='240000'
   ########################################################################

   ########################################################################
   # Set up for MEGAN
   export RUN_MEGAN='Y'       # Run megan?


   # By default MEGAN will use data from MGNMET unless specify below
   export ONLN_DT='Y'       # Use online daily average temperature
                           # No will use from EFMAPS

   export ONLN_DS='Y'       # Use online daily average solar radiation
                           # No will use from EFMAPS

   # Grid definition
   export GRIDDESC=$MGNRUN'/GRIDDESC'
   export GDNAM3D='USA'${DOM}'km' 
   
   echo 'GRIDDESC='${GRIDDESC}
   
   # EFMAPS
   export EFMAPS=$INPDIR'/EFMAPS.'${PRJ}${DOM}'.ncf'

   # PFTS16
   export PFTS16=$INPDIR'/PFTS16.'${PRJ}${DOM}'.ncf'

  # LAIS46
   export LAIS46=$INPDIR'/LAIS46.'${PRJ}${DOM}'.ncf'

   # MGNMET
   export MGNMET=$METDIR'/MET.MEGAN.'$GDNAM3D.${SDATE}'.ncf'

   # Output
   export MGNERS=$INTDIR'/ER.'$GDNAM3D.${SDATE}'.ncf'

   ########################################################################
   ## Run MEGAN
   echo 'Run MEGAN: '${EXE}
  if [ $RUN_MEGAN == 'Y' ] 
   then
      rm -f $MGNERS
      time $EXE #>&! ${LOGDIR}/'log.run.'${PROG}.${GDNAM3D}.${SDATE}'.txt'
  fi

  let JD++
done  # End while JD
