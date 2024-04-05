#!/bin/bash
echo 'Megan Model Compile Shell - Luiz Flavio Mar/13'
echo 'Yoy must be on eslogin01 node to compile it'
echo 'You must have the ioapi librarie configured'
echo '----------------------------------------------'
echo 'usage:'
echo '        ./makeall.sh [clean]'
echo ''
echo '----------------------------------------------'
echo 'Loading Tupan Modules (netcdf)'
. /opt/modules/default/etc/modules.sh
module load netcdf
echo 'Compiling parts ...'
cd EMPROC
make $1
cd ..
cd IOAPI2UAM
make $1
cd ..
cd MET2MGN
make $1
cd ..
cd MGN2MECH
make $1
cd ..
cd TXT2IOAPI
make $1
cd ..
echo ''
echo '----------------------------------------------'
echo ''
echo 'All executable are in bin'
