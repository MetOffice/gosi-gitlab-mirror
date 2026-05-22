#!/bin/bash -f
# set -vx

SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)
USE_REF=0

. ./param.default
[ -f ./param.cfg ] && . ./param.cfg || echo "warning: \"param.cfg\" file not found; SETTE will use default paramaters from \"param.default\" file"


if [ $# -gt 0 ]; then
  while getopts c:Rh option; do 
     case $option in
        c) COMPILER=$OPTARG;;
        R) USE_REF=1;;
        h | *) echo ''
               echo 'sette_list_avail_rev.sh : ' 
               echo '     list all sette directory and available revisions created with the compiler specified in param.cfg or in the startup file)'
               echo '-c COMPILER_name :'
               echo '     list all sette directory and available revisions created with the compiler specified'
               echo ''
               exit 42;;
     esac
  done
  shift $((OPTIND - 1))
fi
NEMO_VALIDATION_DIR=${NEMO_VALIDATION_DIR}
NEMO_VALIDATION_REF=${NEMO_VALIDATION_REF}

#
lst_rev () {
    # get the list of revision available for a configuration
    # base directory
    VALSUB=$1
    # <ARCH>/<CONF>
    ARCH_CONF=$2
    # list of all available <REV> identifiers
    ALLLST=${@:3}
    # display
    printf "\n %-28s : " $CONFIG
    for rev in $ALLLST
    do
       if [ -d "${VALSUB}/$rev/${ARCH_CONF}" ]  ; then
          printf "%-14s  " $rev
       else
          printf "%-14s  " "------------ " 
       fi
    done
}


  NEMO_VALID=${NEMO_VALIDATION_DIR}/
  if [ ${USE_REF} == 1 ] ; then 
    NEMO_VALID=${NEMO_VALIDATION_REF}/
  fi
 
 # list of all revision available
 DIRLIST=`find "${NEMO_VALID}" -maxdepth 2 -mindepth 2 -type d -name "${COMPILER}" | sort -u`
 DIRLIST=`dirname ${DIRLIST}`
 DIRLIST=`basename -a $DIRLIST`

 # display header
 echo ""
 echo " Compiler used is : $COMPILER"
 echo ""
 printf " List of all avail. rev. in   :"${NEMO_VALID}"\n"
 printf "                         is   : "
 for dir in `echo $DIRLIST`; do printf "%-14s  " $dir ; done
 printf "\n"

 # start checking configuration revision
 echo " Availability for each config.: "
 echo -n " ------------------------------"
 for CONFIG in GYRE_PISCES ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 WED025 ORCA2_ICE_OBS ORCA2_SAS_ICE AGRIF_DEMO SWG ISOMIP+ OVERFLOW LOCK_EXCHANGE VORTEX ICE_AGRIF IWAVE  
 do
    lst_rev "${NEMO_VALID}" "${COMPILER}/${CONFIG}" "$DIRLIST"
 done
 printf "\n"
 printf "\n"
#
exit
