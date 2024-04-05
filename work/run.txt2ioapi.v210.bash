#!/bin/bash -f
########################################################################
## Common setups
source ../setcase.bash

export PRJ='USA' 
export DOM='36' 

export PROMPTFLAG='N'
export PROG='txt2ioapi'
export EXEDIR=${MGNEXE}
export EXEC=${EXEDIR}'/'${PROG}
export GRIDDESC=${MGNRUN}'/GRIDDESC'
export GDNAM3D='USA'${DOM}'km' 

## File setups
## Inputs
export EFSTXTF=${MGNINP}'/MAP/EF210_'${PRJ}${DOM}'.csv'
export PFTTXTF=${MGNINP}'/MAP/PFT210_'${PRJ}${DOM}'.csv'
export LAITXTF=${MGNINP}'/MAP/LAI210_'${PRJ}${DOM}'.csv'
## Outputs
export EFMAPS=${MGNINP}'/MAP/EFMAPS.'${PRJ}${DOM}'.ncf'
export PFTS16=${MGNINP}'/MAP/PFTS16.'${PRJ}${DOM}'.ncf'
export LAIS46=${MGNINP}'/MAP/LAIS46.'${PRJ}${DOM}'.ncf'

## Run control
export RUN_EFS='T'       # [T|F]
export RUN_LAI='T'       # [T|F]
export RUN_PFT='T'       # [T|F]
########################################################################





## Run TXT2IOAPI
rm -f ${EFMAPS} ${LAIS46} ${PFTS16}
if [ ! -e ${MGNLOG}'/'${PROG} ] 
then
   mkdir -p ${MGNLOG}'/'${PROG}
fi
${EXEC} | tee ${MGNLOG}'/'${PROG}'/log.run.'${PROG}'.'${PRJ}${DOM}'.txt'
