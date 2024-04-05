#!/bin/bash
#
export MGNHOME='/scratchin/grupos/pad/home/luiz.flavio/MEGAN'
export MGNSRC=$MGNHOME'/src'
export MGNLIB=$MGNHOME'/lib'
export MGNEXE=$MGNHOME'/bin'
export MGNRUN=$MGNHOME'/work'
export MGNINP=$MGNHOME'/Input'
export MGNOUT=$MGNHOME'/Output'
export MGNINT=$MGNHOME'/Output/INT'
export MGNLOG=$MGNHOME'/work/logdir'

if [ ! -e $MGNINP ] 
  then
   mkdir -p $MGNINP'/MAP'
   mkdir -p $MGNINP'/MGNMET'
   mkdir -p $MGNINP'/PAR'
fi
if [ ! -e $MGNINT ] 
   then
      mkdir -p $MGNINT
fi
if [ ! -e $MGNLOG ] 
   then
      mkdir -p $MGNLOG
fi
