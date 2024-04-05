#!/bin/bash
#
# MET2MGN v2.10 
# --
#
#
# TPAR2IOAPI v2.03a 
# --added 26-category landuse capability for mm5camx (number of landuse categories defined by NLU) 
# --added capability for LATLON and UTM projections
# --added capability for MCIP v3.3 input (2m temperatures)
# --bug in PAR processing subroutine fixed where first few hours in GMT produced zero PAR
# --added code to fill missing par data (if valid data exists for the hours surrounding it)
#
# TPAR2IOAPI v2.0
# --added capability for MM5 or MCIP input
# 
#
#        RGRND/PAR options:
#           export MM5RAD  Y   Solar radiation obtained from MM5
#           OR 
#           export MCIPRAD Y   Solar radiation obtained from MCIP
#                  --MEGAN will internally calculate PAR for each of these options and user needs to  
#                    specify `export PAR_INPUT N' in the MEGAN runfile
#           OR
#           export SATPAR Y (satellite-derived PAR from UMD GCIP/SRB files)
#                  --user needs to specify `export PAR_INPUT Y' in the MEGAN runfile
#
#        TEMP options:
#           export CAMXTEMP Y         2m temperature, calculated from mm5camx output files
#           OR
#           export MM5MET  Y         2m temperature, calculated from MM5 output files
#                                     Note: 2m temperature is calculated since the P-X/ACM PBL
#                                     MM5 configuration (most commonly used LSM/PBL scheme for AQ 
#                                     modeling purposes) does not produce 2m temperatures.
#           OR
#           export MCIPMET Y         temperature obtained from MCIP
#              -export TMCIP  TEMP2   2m temperature, use for MCIP v3.3 or newer
#              -export TMCIP  TEMP1P5 1.5m temperature, use for MCIP v3.2 or older
#
#        TZONE   time zone for input mm5CAMx files 
#        NLAY    number of layers contained in input mm5CAMx files 
#        NLU     number of landuse categories contained in CAMx landuse file 
#

############################################################



############################################################
# Episodes
############################################################
DOM=36 
STJD=2008201
EDJD=2008201

export EPISODE_SDATE='2008201'
export EPISODE_STIME='000000'    

############################################################
#set for grid
############################################################
export GRIDDESC='GRIDDESC'
export GDNAM3D='USA'${DOM}'km'


############################################################
# Setting up directories and common environment variable
############################################################
source ../setcase.bash

export PROG='met2mgn'
export EXE=$MGNEXE'/'$PROG


logdir='logdir/'${PROG}
if [ ! -e ${logdir} ] 
   then
   mkdir -p $logdir
fi
INPPATH=/data3/home/xjiang/MCIP/mcip_out
OUTPATH=${MGNINP}'/MGNMET'
if [ ! -e ${OUTPATH} ] 
  then
    mkdir ${OUTPATH}
fi

export PFILE=${OUTPATH}'/PFILE'
rm -fv ${PFILE}

############################################################
# Looping
############################################################
JDATE=${STJD}
Y4=2008
Y2=08 
MM=07
DD=21 
DDm1=20
while [ $JDATE -le ${EDJD} ]
do

if [${JDATE} -eq 2008367] 
 then
 JDATE=2009001
fi
let jdy  = ${JDATE} - 2000000
#set Y4 = `j2g $JDATE | awk '{print $1}'`
#set Y2 = `echo $Y4 | cut -c 3-4`
#set MM = `j2g $JDATE | awk '{print $2}'`
#set DD = `j2g $JDATE | awk '{print $3}'`

Y4=2008
Y2=08 
MM=07
let DD++

let JDATEm1 = ${JDATE} - 1
if [${JDATEm1} -eq 2008000]
then
JDATEm1=2007365
fi
let jdym1  = ${JDATEm1} - 2000000
#set Y4m1 = `j2g $JDATEm1 | awk '{print $1}'`
#set Y2m1 = `echo $Y4m1 | cut -c 3-4`
#set MMm1 = `j2g $JDATEm1 | awk '{print $2}'`
#set DDm1 = `j2g $JDATEm1 | awk '{print $3}'`
Y4m1=2008
Y2m1=08
MMm1=07
let DDm1++

#set start/end dates
export STDATE=${jdy}'00'
export ENDATE=${jdy}'23'

#TEMP/PAR input choices
#
#set if using MM5 output files
export MM5MET='N'
export MM5RAD='N'
#export numMM5 2
#export MM5file1 /pete/pete5/fcorner/met/links/MMOUT_DOMAIN1_G$Y4$MM$DD
#export MM5file2 /pete/pete5/fcorner/met/links/MMOUT_DOMAIN1_G$Y4$MM$DD

#set if using UMD satellite PAR data
PARDIR=$MGNINP'/PAR'
export SATPAR='N'
satpar1 =${PARDIR}'/'${Y2m1}${MMm1}'par.h'
satpar2 =${PARDIR}'/'${Y2}${MM}'par.h'

if [$satpar1 .eq $satpar2] 
then
  export numSATPAR='1'
  export SATPARFILE1=${satpar2}
else
  export numSATPAR='2'
  export SATPARFILE1=${satpar1}
  export SATPARFILE2=${satpar2}
fi

#set if using MCIP output files
export MCIPMET='Y'
export TMCIP='TEMP2'          #MCIP v3.3 or newer
#export TMCIP  TEMP1P5       #MCIP v3.2 or older

export MCIPRAD='Y' 
if [$JDATE .eq $EPISODE_SDATE] 
then
  export METCRO2Dfile1=${INPPATH}'/METCRO2D.'${GDNAM3D}
else
  export METCRO2Dfile1=${INPPATH}'/METCRO2D.'${GDNAM3D}
  export METCRO2Dfile2=${INPPATH}'/METCRO2D.'${GDNAM3D}
fi
export METCRO3Dfile=${INPPATH}'/METCRO3D.'${GDNAM3D}
export METDOT3Dfile=${INPPATH}'/METDOT3D.'${GDNAM3D}

export OUTFILE=${OUTPATH}'/MET.MEGAN.'${GDNAM3D}'.'${JDATE}'.ncf'
rm -rf $OUTFILE

$EXE |tee ${logdir}'/log.'${PROG}'.'${GDNAM3D}'.'${JDATE}'.txt' 

let JDATE++
done  # End while JDATE
