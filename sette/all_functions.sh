######################################################
# Author : Simona Flavoni for NEMO
# Contact : sflod@locean-ipsl.upmc.fr
#
# Some scripts called by sette.sh
# all_functions.sh   : all functions used by sette.sh  
######################################################
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE.txt)
# ----------------------------------------------------------------------
#
#set -x
#set -o posix
#set -u
#set -e
#+
#
# ================
# all_functions.sh
# ================
#
# ----------------------------------------------
# Set of functions used by sette.sh (NEMO tests) 
# ----------------------------------------------
#
# SYNOPSIS
# ========
#
# ::
#
#  $ ./set_namelist INPUT_NAMELIST VARIABLE VALUE 
#  $ post_test_tidyup 
#
#
# DESCRIPTION
# ===========
#
# function superegrep
#   input variable value
#
# function set_namelist
#   input namelist_name variable value
#   output namelist
#
# function post_test_tidyup
#   creates nemo_validation tree, and save output & debug files
#   this function creates tree of validation in NEMO_VALIDATION_DIR as follows : 
#
#     NEMO_VALIDATION_DIR/REVISION/VARIANT/CONFIGURATION/TEST_NAME
# 
#     NEMO_VALIDATION_DIR  -  base database path as set in the SETTE configuration file (param.cfg)
#     REVISION             -  source-code-revision identifier
#     VARIANT              -  SETTE build and run-time-configuration variant
#     CONFIGURATION        -  SETTE configuration
#     TEST_NAME            -  SETTE test-run name
#
# EXAMPLES
# ========
#
# ::
#
#  $ ./set_namelist namelist          nn_itend        75
#  $ ./set_namelist namelist_ice      cn_icerst_in  \"00101231_restart_ice\"
#  $ post_test_tidyup 
#
#
# TODO
# ====
#
# option debug
#
#
# EVOLUTIONS
# ==========
#
# $Id: all_functions.sh 14981 2021-06-11 13:47:19Z acc $
#
#   * creation
#-
# --------------- set namelist common to all jobs ------
set_common_namelist() {
    NAMELIST_FILE=${1}
    # get stat files
    set_namelist ${NAMELIST_FILE}_cfg sn_cfctl%l_runstat .true.
    set_namelist ${NAMELIST_FILE}_cfg sn_cfctl%l_obsstat .true.
    set_namelist ${NAMELIST_FILE}_cfg sn_cfctl%l_trcstat .true.
    set_namelist ${NAMELIST_FILE}_cfg sn_cfctl%l_lsb_sum .true.

    # set timing, halo, tiling ...
    set_namelist_opt ${NAMELIST_FILE}_cfg ln_timing    ${USING_TIMING} .true. .false.
    set_namelist_opt ${NAMELIST_FILE}_cfg nn_hls       ${USING_EXTRA_HALO} 3 2
    set_namelist_opt ${NAMELIST_FILE}_cfg nn_comm      ${USING_COLLECTIVES} 2 1
    set_namelist_opt ${NAMELIST_FILE}_cfg ln_nnogather ${USING_NOGATHER} .true. .false.

    # some configuration do not support tiling.
    if [[ "${TILE_FLAG}" == "1" ]]; then
        set_namelist_opt ${NAMELIST_FILE}_cfg ln_tile  ${USING_TILING} .true. .false.
    fi
}
# ------------------------------------------------------

# -------------- prepare run function ------------------
prepare_job() {
    IT000=(${1}) ; ITEND=(${2})
    JPNI=(${3})   ; JPNJ=(${4}) ; NPROC=${5}
    TEST_NAME=${6}
    FILES=${7}
    COMP=${8}
    NPROC_MODEL2=${9}
    JOB_FILE="${10}"

    # prepare validation direction
    set_valid_dir
    clean_valid_dir

    # ???
    . ${SETTE_DIR}/prepare_exe_dir.sh

    # case SAO
    if [[ "$TEST_NAME" == "SAO" ]]; then
        ln -s ../../${SETTE_CONFIG_REF}/REF/${MODELDATA} ${EXE_DIR}/.
    fi

    # case PHYOPTS
    # at this stage EXE_DIR is pointing toward the exp directory (changed in prepare_exe)
    if [[ "$TEST_NAME" == EXP-* ]]; then
        PHYEXP=${TEST_NAME#EXP-}
        \cp -f ${EXE_DIR}/namelist_${PHYEXP}_cfg ${EXE_DIR}/namelist_cfg
    fi

    # case CPLOASIS
    if [[ "$TEST_NAME" == CPL ]]; then
        export TOYATM_DIR="${MAIN_DIR}/tools/TOYATM/BLD/bin"
        # copy namcouple to EXP00 (not done by makenemo)
        if [ -f ${OASIS_DIR}/lib/libyaxt.a ]; then
            cp -av ${CONFIG_DIR0}/${config}/EXPREF/namcouple_oasis6 namcouple
        else
            cp -av ${CONFIG_DIR0}/${config}/EXPREF/namcouple .
        fi
    fi

    # update job physical parameters
    set_physical_namelist

    # if required, set up a new job file
    if [ ! -f "${JOB_FILE}" ] ; then
        JOB_FILE=${EXE_DIR}/run_job.sh
        if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
    fi

    # prepare specific namelist parameter (AGRIF or not)
    # only one element means AGRIF is not used
    AGRIF_LVL=${#IT000[@]} ; AGRIF_LVL=$((AGRIF_LVL-1))

    if [ $TEST_NAME == 'NOAGRIF' ]; then
        set_noagrif_namelist
        if [ -f ${EXE_DIR}/AGRIF_FixedGrids.in ]; then 
            sed -i "1s/.*/0/" ${EXE_DIR}/AGRIF_FixedGrids.in
        fi
        AGRIF_LVL=0
    fi

    LVL=0
    while [ "$LVL" -le "${AGRIF_LVL}" ]; do
        # get namelist file name
        EXPNAME=${TEST_NAME}
        if [ "$LVL" -eq 0 ]; then 
            NAMELISTFILE=namelist
        else
            NAMELISTFILE=${LVL}_namelist
        fi

        # set common namelist
        set_common_namelist $NAMELISTFILE

        set_namelist ${NAMELISTFILE}_cfg cn_exp \"${EXPNAME}\"
        set_namelist ${NAMELISTFILE}_cfg nn_it000 ${IT000[$LVL]}
        set_namelist ${NAMELISTFILE}_cfg nn_itend ${ITEND[$LVL]}
        set_namelist ${NAMELISTFILE}_cfg jpni     ${JPNI[$LVL]}
        set_namelist ${NAMELISTFILE}_cfg jpnj     ${JPNJ[$LVL]}

        # ROT specificities
        if [[ "$TEST_NAME" == ROT* ]]; then
            if [[ "${TEST_NAME#ROT-}" == "000" ]]; then set_namelist ${NAMELISTFILE}_cfg nn_rot 0; fi
            if [[ "${TEST_NAME#ROT-}" == "090" ]]; then set_namelist ${NAMELISTFILE}_cfg nn_rot 1; fi
            if [[ "${TEST_NAME#ROT-}" == "180" ]]; then set_namelist ${NAMELISTFILE}_cfg nn_rot 2; fi
        fi
 
        # RST specificities
        if [ $TEST_NAME == 'RST' ]; then
            #if iceberg activated, generation of test iceberg needs to be switch off for the rst run
            if [ ${USING_ICEBERGS} == "yes" ]; then
                COMP="${COMP} ICB"
                set_namelist ${NAMELISTFILE}_cfg ln_use_test .false.
            fi
            #if agrif, need to update initialisation in the short rst run
            if [ "$LVL" -gt 0 ]; then
                set_namelist ${NAMELISTFILE}_cfg ln_init_chfrpar .false.
            fi
        fi
        # set nit000, nn_stock ...
        if [ $TEST_NAME == 'RST' ] || [ $TEST_NAME == 'REF' ]; then
            set_namelist_rst ${NAMELISTFILE} ${ITEND[$LVL]} REF "${COMP}"
        fi

        LVL=$((LVL + 1))
    done

    # prepare job files
    . ${SETTE_DIR}/prepare_job.sh "${FILES}" ${NPROC} ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID} ${NPROC_MODEL2}
}
# --------------- end prepare_run function --------------



# function to find namelists parameters
supergrep () {
            grep "^ *$1 *=" $2 | sed -e "s% *\!.*%%"
    }

usage=" Usage : set_namelist input_namelist variable_name value"
usage=" if value is a string ths is neede syntax : ./set_namelist namelist_name var_name \"new_value\" "

# sync MYSRC files (input CFG and CFG_STg; where g is an optional, single, alphanumeric character)
sync_config() {
   if [ ${SYNC_CONFIGS} == "yes" ]; then

      lREF=$1    # reference
      lCFG=$2    # target

      echo '-------------------------------------------------------------------------------'
      echo '                    SOURCE AND CONFIG FILES SYNCHRONISATION                    '
      # synchronise MY_SRC if $lCFG directory exist
      if [ -d $lREF/MY_SRC ] && [ -d $lCFG ] ; then

         if [ -d $lCFG/MY_SRC ] ; then

            # Manage case number of file in MY_SRC changes
            # find number of file only in $lCFG/MY_SRC
            FCFG=`diff -r $lREF/MY_SRC $lCFG/MY_SRC | grep "Only in $lCFG/MY_SRC" | awk '{print $4}'`

            # if more than 0, trigger a cleaning as Makenemo do not properly manage this case.
            if [ ! -z "$FCFG" ]; then 
               echo ''
               echo "$lCFG contains a different file list than $lREF :"
               diff -r $lREF/MY_SRC $lCFG/MY_SRC | grep "Only in $lCFG/MY_SRC" | awk '{print $4}'
               echo "Synchronisation is not enough because Makenemo does not handle well case where a file in MY_SRC is removed"
               echo "Therefore, we need to clean $lCFG before starting runing it"
               echo ''
               CLEAN_CONFIGS="yes"
            fi
         fi

         # synchronisation $lREF/MY_SRC directory (and delete file in target location if needed)
         echo ''
         echo "configuration $lCFG/MY_SRC will be synchronised with $lREF/MY_SRC"
         echo ''
         rsync -a --delete $lREF/MY_SRC/ $lCFG/MY_SRC

         # rsync keep preserve the modification time stamp.
         # To avoid case where a file in MY_SRC is replace by an older file, we touch the file
         touch --no-create $lCFG/MY_SRC/*
      else
          echo ''
          echo "configuration $lCFG/MY_SRC not synchronised with $lREF/MY_SRC"
          echo ''
      fi

      # synchronize EXPREF
      if [ -d $lREF/EXPREF ] && [ -d $lCFG/EXP00 ] ; then
          echo ''
          echo "configuration $lCFG/EXP00 will be synchronised with $lREF/EXPREF"
          echo "(links are skipped)"
          echo ''
          rsync -a --no-links $lREF/EXPREF/ $lCFG/EXP00/.
      else
          echo ''
          echo "configuration $lCFG/EXP00 not synchronised with $lREF/EXPREF"
          echo ''
      fi
      echo '-------------------------------------------------------------------------------'
   fi
}

# clean _STg config (input CFG CFG_STg TYPE (test or ref))
clean_config() {
   if [ ${CLEAN_CONFIGS} == "yes" ]; then
      lCFG=$1
      echo ''
      echo '-------------------------------------------------------------------------------'
      echo '                         CLEANING CONFIGURATION                                '
      echo ''
      echo "./makenemo -n $(basename $lCFG) ${CUSTOM_DIR:+-t ${CMP_DIR}} -y clean"
      echo ''
      if [ -d $lCFG ]; then
        ./makenemo -n $(basename $lCFG) ${CUSTOM_DIR:+-t ${CMP_DIR}} -y clean
        echo ''
        echo "$(basename $lCFG) configuration has been cleaned"
      else
        echo "$(basename $lCFG) configuration does not exist; we skip cleaning"
      fi
      echo ''
      echo '-------------------------------------------------------------------------------'
   fi
}

# extract usefull informations from .git directory
set_git_var () {
  # git worktree case
  if [ -f ${MAIN_DIR}/.git ]; then
    GIT_DIR=$(readlink -f $(cat ${MAIN_DIR}/.git | sed 's/gitdir: //'))
  # git directory case
  else
    GIT_DIR="${MAIN_DIR}/.git"
  fi
  if [ -d "${GIT_DIR}" ]; then
    # sette.sh
    export SETTE_THIS_BRANCH=`git -C ${MAIN_DIR} log -1 --pretty=%D HEAD | sed 's|.*origin/||g;s|, .*||g;s|.*-> ||g'`
    export VALID_REV=`git -C ${MAIN_DIR} rev-parse --short=8 HEAD 2> /dev/null`
    REV_DATE0=`git -C ${MAIN_DIR} show --no-patch --format=%ct ${NEMO_REV}`
    localchanges=`git -C ${MAIN_DIR} status --short -uno | wc -l`
    # sette_rpt.sh
    branchname=${SETTE_THIS_BRANCH}
  else
    echo "WARNING: local .git directory not found -> related informations unavailable"
    # sette.sh
    export SETTE_THIS_BRANCH="undef"
    export NEMO_REV="undef"
    REV_DATE0=0
    localchanges=0
    # sette_rpt.sh
    branchname="undef"
  fi
}

# define variables from .env file
set_dotenv_var () {
  if [[ -n "${DOTENV_FILE}" && -s ${DOTENV_FILE} ]]; then
    while read l; do
      local NEMO_BRANCH=""
      local NEMO_SHA=""
      local NEMO_DATE=""
      eval "local ${l%=*}='${l#*=}'"
      # sette.sh
      [ -n "${NEMO_BRANCH}" ] && export SETTE_THIS_BRANCH=${NEMO_BRANCH}
      [ -n "${NEMO_SHA}"    ] && export VALID_REV=${NEMO_SHA}
      [ -n "${NEMO_DATE}"   ] && REV_DATE0=${NEMO_DATE}
      # sette_rpt.sh
      [ -n "${NEMO_BRANCH}" ] && export branchname=${NEMO_BRANCH}
      unset ${l%=*}
    done < ${DOTENV_FILE}
    localchanges=0
  else
    echo "WARNING: .env file not found -> related informations unavailable"
    # sette.sh
    export SETTE_THIS_BRANCH="undef"
    export NEMO_REV="undef"
    REV_DATE0=0
    localchanges=0
    # sette_rpt.sh
    branchname="undef"
  fi
}


# define validation dir
set_valid_dir () {
    export NEMO_VALID=${NEMO_VALIDATION_DIR}/${VALID_REV}/${VALID_VAR}/${SETTE_CONFIG%${SETTE_STG}}/${TEST_NAME}
}

# clean valid dir (move old ocean_output/run.stat and tracer to avoid checking them in case something wrong happen.
clean_valid_dir () {
#   set_valid_dir # already done in sette_ref/sette_test
   echo "validation directory is : $NEMO_VALID"
   if [ -d $NEMO_VALID ] ; then
      [ -f ${NEMO_VALID}/ocean.output ] && mv ${NEMO_VALID}/ocean.output ${NEMO_VALID}/ocean.output_old
      [ -f ${NEMO_VALID}/run.stat ]     && mv ${NEMO_VALID}/run.stat     ${NEMO_VALID}/run.stat_old
      [ -f ${NEMO_VALID}/tracer.stat ]  && mv ${NEMO_VALID}/tracer.stat  ${NEMO_VALID}/tracer.stat_old
      [ -f ${NEMO_VALID}/obs.stat ]     && mv ${NEMO_VALID}/obs.stat     ${NEMO_VALID}/obs.stat_old
   fi
}

# set_namelist_opt: function to set namelists parameters based on a yes/no selection
# Mandatory arguments are, in order:
# 1. namelist to be edited
# 2. variable to be set
# 3. yes or no switch setting
# 4. value to set variable to if switch is yes
# 5. value to set variable to if switch is no
set_namelist_opt () {
	minargcount=5
	if [ ${#} -lt ${minargcount} ]
	then
		echo "not enough arguments for set_namelist_opt" >> ${SETTE_DIR}/output.sette
		echo "Usage: set_namelist_opt namelist varname yes_or_no value_if_yes value_if_no" >> ${SETTE_DIR}/output.sette
		exit 1
	fi
	unset minargcount
        if [ $3 != 'yes' ] && [ $3 != 'no' ] ; then
                echo 'option switch must be "yes" or "no"' >> ${SETTE_DIR}/output.sette
                echo "${usage}" >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        if [ $3 == 'yes' ] ; then
                set_namelist $1 $2 $4
        else
                set_namelist $1 $2 $5
        fi
}

# set_namelist_rst: if the test-run name (TEST_NAME) is "SHORT", this function
# configures test runs to restart at half the specified number of timesteps
# (argument 2) by adding corresponding entries to namelist input files with the
# supplied base name (argument 1) and by creating symbolic links to restart
# files produced by a corresponding test run "LONG" and with a specified base
# name (argument 3, without AGRIF prefix), taking into account the specified
# components/options (argument 4, subset of "OCE ICE TOP ABL ICB"); otherwise
# it configures test runs to output restart files halfway through the run.
set_namelist_rst () {
    ITRST=$(( ${2} / 2 ))
    set_namelist ${1}_cfg nn_stock ${ITRST}
    if [[ ${TEST_NAME} == "RST" ]]; then
        NEMO_COMPONENTS="${4} "
        ITRST8=$( printf "%08d" ${ITRST} )
        aprefix=${1%${1#[0-9]_}}
        set_namelist ${1}_cfg nn_it000 $(( ${ITRST} + 1 ))
        if [[ ${NEMO_COMPONENTS} =~ "OCE " ]]; then
            set_namelist ${1}_cfg ln_rstart .true.
            set_namelist ${1}_cfg nn_rstctl 2
            set_namelist ${1}_cfg cn_ocerst_in \"${3}_${ITRST8}_restart\"
            if [[ ${NPROC} -eq 1 ]]; then
                ln -sf ../REF/${aprefix}${3}_${ITRST8}_restart.nc .
            else
                for (( i=1; i<=${NPROC}; i++ )); do
                    L_NPROC=$(printf "%04d\n" $(( $i - 1 )) )
                    ln -sf ../REF/${aprefix}${3}_${ITRST8}_restart_${L_NPROC}.nc .
                done
            fi
        fi
        if [[ "${NEMO_COMPONENTS} " =~ "ICE " ]]; then
            set_namelist ${1}_ice_cfg cn_icerst_in \"${3}_${ITRST8}_restart_ice\"
            if [[ ${NPROC} -eq 1 ]]; then
                ln -sf ../REF/${aprefix}${3}_${ITRST8}_restart_ice.nc .
            else
                for (( i=1; i<=${NPROC}; i++ )); do
                    L_NPROC=$(printf "%04d\n" $(( $i - 1 )) )
                    ln -sf ../REF/${aprefix}${3}_${ITRST8}_restart_ice_${L_NPROC}.nc .
                done
            fi
        fi
        if [[ "${NEMO_COMPONENTS} " =~ "TOP " ]]; then
            set_namelist ${1}_top_cfg ln_rsttr .true.
            set_namelist ${1}_top_cfg nn_rsttr 2
            set_namelist ${1}_top_cfg cn_trcrst_in \"${3}_${ITRST8}_restart_trc\"
            for (( i=1; i<=${NPROC}; i++ )); do
                L_NPROC=$(printf "%04d\n" $(( $i - 1 )) )
                ln -sf ../REF/${aprefix}${3}_${ITRST8}_restart_trc_${L_NPROC}.nc .
            done
        fi
        if [[ "${NEMO_COMPONENTS} " =~ "ABL " ]]; then
            set_namelist ${1}_cfg ln_rstart_abl .true.
            set_namelist ${1}_cfg cn_ablrst_in \"${3}_${ITRST8}_restart_abl\"
            for (( i=1; i<=${NPROC}; i++ )); do
                L_NPROC=$(printf "%04d\n" $(( $i - 1 )) )
                if [[ ${USING_ABL} == "yes" ]]; then
                    ln -sf ../REF/${aprefix}${3}_${ITRST8}_restart_abl_${L_NPROC}.nc .
                fi
            done
        fi
        if [[ "${NEMO_COMPONENTS} " =~ "ICB " ]]; then
            set_namelist ${1}_cfg cn_icbrst_in \"${3}_${ITRST8}_restart_icb\"
            if [[ ${USING_ICEBERGS} == "yes" ]]; then
                for (( i=1; i<=${NPROC}; i++ )); do
                    L_NPROC=$(printf "%04d\n" $(( $i - 1 )) )
                    ln -sf ../REF/${aprefix}${3}_${ITRST8}_restart_icb_${L_NPROC}.nc .
                done
            fi
        fi
    fi
}

# set_namelist: function to set namelists parameters
set_namelist () {
	minargcount=3
	if [ ${#} -lt ${minargcount} ]
	then
		echo "not enough arguments for set_namelist"
		echo "${usage}"
		exit 1
	fi
	unset minargcount
	if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_namelist $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette
      
	VAR_NAME=$( supergrep $2 ${EXE_DIR}/$1 )
	if [ ${#VAR_NAME} -eq 0 ] 
	then
		echo "doing \"set_namelist $@\". "  >> ${SETTE_DIR}/output.sette
		echo "variable: \"$2\" not found in \"${EXE_DIR}/$1\" "  >> ${SETTE_DIR}/output.sette
                NAMREF=$( basename $1 _cfg )_ref
		echo "doing more : search in ${EXE_DIR}/$NAMREF " >> ${SETTE_DIR}/output.sette
                VAR_NAME=$( supergrep $2 ${EXE_DIR}/$NAMREF )
	        if [ ${#VAR_NAME} -eq 0 ] 
	        then
                    echo " variable $VAR_NAME not found in ${EXE_DIR}/$1 nor in ${EXE_DIR}/$NAMREF "
                    echo " check your variable name "
        		echo "exit"
	        	echo "error in executing script : set_namelist $@" >> ${SETTE_DIR}/output.sette
	        	echo "....." >> ${SETTE_DIR}/output.sette
        		exit 1
                fi
                LINEVAR=$( grep -s -n "$VAR_NAME" ${EXE_DIR}/$NAMREF | awk -F: '{ { print $1} }' )
                echo " $VAR_NAME found in ${EXE_DIR}/$NAMREF at line $LINEVAR " >> ${SETTE_DIR}/output.sette

#   search for namelist group name
                NAMGRP=$( head -n$LINEVAR ${EXE_DIR}/$NAMREF | grep --line-buffered "^&nam" | tail -1 | awk -F" " '{ { print $1} }' ) 
                echo " variable $VAR_NAME will be added in $NAMGRP namelist-group of namelist file ${EXE_DIR}/$1 " >> ${SETTE_DIR}/output.sette

# check if namelist group present in namelist_cfg
# if missing group is added at the end of namelist_cfg
                NGRP=$(grep ${NAMGRP} ${EXE_DIR}/$1 | wc -l )
                if [ ${NGRP} -eq 0 ]; then
                   echo ''                                                                          >> ${SETTE_DIR}/output.sette
                   echo "+++++ Group ${NAMGRP} containing ${2} is missing in ${EXE_DIR}/$1 +++++ "  >> ${SETTE_DIR}/output.sette
                   echo "+++++ Group ${NAMGRP}                 is added   in ${EXE_DIR}/$1 +++++ "  >> ${SETTE_DIR}/output.sette
                   echo ''                                                                          >> ${SETTE_DIR}/output.sette
                   echo "${NAMGRP}" >> ${EXE_DIR}/$1
                   echo "/"         >> ${EXE_DIR}/$1
                fi

# Add $VARNAME in namelist file ${EXE_DIR}/$1 in namelist group $NAMGRP
# on mac osx, replace sed --posix by gsed (available with mac port)
                sed    --posix -e "/${NAMGRP}[ !]/a\ ${VAR_NAME} " -e "/${NAMGRP}$/a\ ${VAR_NAME} " ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp || \
                  gsed --posix -e "/${NAMGRP}[ !]/a\ ${VAR_NAME} " -e "/${NAMGRP}$/a\ ${VAR_NAME} " ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp

# if file not empty replace ${EXE_DIR}/$1
               if [ -s ${EXE_DIR}/$1.tmp ] ; then
                   mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1 
                else
                echo "file ${EXE_DIR}/$1.tmp is empty. sed command went wrong "; exit 200
                fi
	fi

        ARGS_LST="${@:3}"
        sed -e "s;${VAR_NAME}.*;${VAR_NAME};" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1
        sed -e "s;${VAR_NAME};$2=${ARGS_LST};"  ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_namelist $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}


# function to tidy up after each test and populate the NEMO_VALIDATION store
post_test_tidyup () {
# Save current exit status of caller script 
    RUN_STATUS=$? 
    echo "Exit status: ${RUN_STATUS}" 
#
# requires the following variables defined and exported from the calling script:
#  SETTE_DIR
#  INPUT_DIR
#  EXE_DIR
#  CONFIG_DIR
#  NEMO_VALIDATION_DIR
#  config
#  CMP_NAM
#  TEST_NAME
echo "SETTE directory is : ${SETTE_DIR}"
echo "INPUT directory is : ${INPUT_DIR}"
echo "EXECUTION directory is : ${EXE_DIR}"
echo "CONFIG directory is : ${CONFIG_DIR}"
echo "VALIDATION directory is : ${NEMO_VALID}"
echo "CONFIGURATION is : ${config}"
echo "COMPILER is : ${CMP_NAM}"
echo "TEST is : ${TEST_NAME}"
echo "TOOLS directory is : ${TOOLS_DIR}"
################################################################
# SMALL DEBUG
    EXIT_STATUS=${RUN_STATUS}
    if [ ! -r ${EXE_DIR}/ocean.output ]
        then
        grep "E R R O R" ${EXE_DIR}/ocean.output && echo "Some ERRORS at execution time, see ${EXE_DIR}/ocean.output"
        EXIT_STATUS=2
        # exit 2 Error now catch in the report 
    fi

    if [ ! -r ${EXE_DIR}/time.step ]
        then
        echo "file time.step does not exist"   >> ${SETTE_DIR}/output.sette
        echo "some problems during execution of model"  >> ${SETTE_DIR}/output.sette 
        EXIT_STATUS=1
        # exit 1 Error now catch in the report
    else
        echo "file time.step exists"  >> ${SETTE_DIR}/output.sette
        echo "execution of model time step loop started"   >> ${SETTE_DIR}/output.sette
    fi

################################################################

################################################################
#
# Creation of NEMO_VALIDATION tree
#    set_valid_dir already done in sette_reference_config
    mkdir -p ${NEMO_VALIDATION_DIR}
    if [ -d ${NEMO_VALIDATION_DIR} ] ; then
	echo "created ${NEMO_VALIDATION_DIR} directory"   >> ${SETTE_DIR}/output.sette
    else 
	echo "problems in creating ${NEMO_VALIDATION_DIR} directory"   >> ${SETTE_DIR}/output.sette
	echo "EXIT,"
	exit 1
    fi
# 
# Exit before populating validation directory if the model run has 
# returned a non-zero exit status 
# On CRAY NEMO exit is not the expected 999 or 123456 (let this bloc in case useful later on).
#    case ${EXIT_STATUS} in
#        0|123456|999) echo " NEMO finished with exit code $EXIT_STATUS " ; post_test_tidyup ;;
#        *) echo " NEMO abort on an unexpected error (segmentation fault or whatever) $EXIT_STATUS "
#    esac

    [ ${EXIT_STATUS} -ne 0 ] && return ${EXIT_STATUS}
#
# Save output & debug files in NEMO_VALIDATION tree
    echo "saving ocean & ice output, run.stat, tracer.stat files ...." >> ${SETTE_DIR}/output.sette
    echo "            " >> ${SETTE_DIR}/output.sette
    [ -f ${EXE_DIR}/ocean.output ] && cp ${EXE_DIR}/*ocean.output ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/run.stat ] && cp ${EXE_DIR}/*run.stat ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/output.namelist.dyn ] && cp ${EXE_DIR}/*output.nam* ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/namelist_cfg ] && cp ${EXE_DIR}/*nam*_cfg ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/tracer.stat ] && cp ${EXE_DIR}/*tracer.stat ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/obs.stat ] && cp ${EXE_DIR}/*obs.stat ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/timing.output ] && cp ${EXE_DIR}/*timing.output ${NEMO_VALIDATION_DIR}/.
    [ -f ${EXE_DIR}/sette_config ] && cp ${EXE_DIR}/sette_config ${NEMO_VALIDATION_DIR}/.

    if [ -n "$(ls ${NEMO_VALIDATION_DIR}/*run*)" ] ; then
	echo "moved run.stat in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "moved run.stat in ${NEMO_VALIDATION_DIR} directory"  
    else
	echo "problem in looking for run.stat file in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "run.stat IS NOT in ${NEMO_VALIDATION_DIR} directory" 
    fi
    if [ -n "$(ls ${NEMO_VALIDATION_DIR}/*ocean.output*)" ] ; then
	echo "moved ocean.output in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "moved ocean.output in ${NEMO_VALIDATION_DIR} directory" 
    else
	echo "problem in looking for ocean.output file in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
	echo "ocean.output IS NOT in ${NEMO_VALIDATION_DIR} directory" 
    fi
    if [ -n "$(ls ${NEMO_VALIDATION_DIR}/*tracer.stat*)" ] ; then
        echo "moved tracer.stat in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
        echo "moved tracer.stat in ${NEMO_VALIDATION_DIR} directory"
    else
        echo "problem in looking for tracer.stat file in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
        echo "tracer.stat IS NOT in ${NEMO_VALIDATION_DIR} directory"
    fi
    if [ -n "$(ls ${NEMO_VALIDATION_DIR}/*obs.stat*)" ] ; then
        echo "moved obs.stat in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
        echo "moved obs.stat in ${NEMO_VALIDATION_DIR} directory"
    else
        echo "problem in looking for obs.stat file in ${NEMO_VALIDATION_DIR} directory"  >> ${SETTE_DIR}/output.sette
        echo "obs.stat IS NOT in ${NEMO_VALIDATION_DIR} directory"
    fi

  return ${EXIT_STATUS}
}

#############################################################
# extra functions to manipulate settings in the iodef.xml file
#
# Examples:
#   set_xio_file_type    iodef.xml one_file
#   set_xio_using_server iodef.xml true
#   set_xio_buffer_size  iodef.xml 50000000
#   set_xio_field_defs   iodef.xml
#
#############################################################

usage2=" Usage : set_xio_file_type input_iodef.xml one_file||multiple_file"
usage3=" Usage : set_xio_using_server input_iodef.xml true||false"
usage4=" Usage : set_xio_buffer_size input_iodef.xml int_buffer_size"
usage5=" Usage : set_xio_field_defs input_iodef.xml"

set_xio_file_type () {
        minargcount=2
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_file_type"
                echo "${usage2}"
                exit 1
        fi
        if [ $2 != "one_file" ] && [ $2 != "multiple_file" ]
        then
                echo "unrecognised argument for set_xio_file_type"
                echo "${usage2}"
                echo $2
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_file_type $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette

        VAR_NAME=$( grep "^.*<.*file_definition.*type.*=" ${EXE_DIR}/$1 | sed -e "s% *\!.*%%" )
        if [ ${#VAR_NAME} -eq 0 ]
        then
                echo "doing \"set_xio_file_type $@\". "
                echo "xml_tag: file_definition with variable: type is empty"
                echo "confirm that an appropriate file_definition is in \"${EXE_DIR}/$1\" "
                echo "exit"
                echo "error in executing script : set_xio_file_type $@" >> ${SETTE_DIR}/output.sette
                echo "....." >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        if [ $2 == "one_file" ] 
        then
           sed -e "s:multiple_file:one_file:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        else
           sed -e "s:one_file:multiple_file:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        fi
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_xio_file_type $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}

set_xio_using_server () {
        minargcount=2
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_using_server"
                echo "${usage2}"
                exit 1
        fi
        inarg=$2
        if [ ${inarg} == "yes" ] ; then inarg="true" ; fi
        if [ ${inarg} == "no" ]  ; then inarg="false" ; fi
        if [ ${inarg} != "true" ] && [ ${inarg} != "false" ]
        then
                echo "unrecognised argument for set_xio_using_server"
                echo "${usage2}"
                echo ${inarg}
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_using_server $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette

        VAR_NAME=$( grep "^.*<.*variable id.*=.*using_server.*=.*bool" ${EXE_DIR}/$1 | sed -e "s% *\!.*%%" )
        if [ ${#VAR_NAME} -eq 0 ]
        then
                echo "doing \"set_xio_using_server $@\". "
                echo "xml_tag: "variable id=using_server" with variable: bool is empty"
                echo "confirm that an appropriate variable id is in \"${EXE_DIR}/$1\" "
                echo "exit"
                echo "error in executing script : set_xio_using_server $@" >> ${SETTE_DIR}/output.sette
                echo "....." >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        if [ ${inarg} == "false" ]
        then
           sed -e "/using_server/s:true:false:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        else
           sed -e "/using_server/s:false:true:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        fi
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_xio_using_server $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}

set_xio_buffer_size () {
        minargcount=2
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_buffer_size"
                echo "${usage4}"
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_buffer_size $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette

        VAR_NAME=$( grep "^.*<.*variable id.*=.*buffer_size.*=.*integer" ${EXE_DIR}/$1 | sed -e "s% *\!.*%%" )
        if [ ${#VAR_NAME} -eq 0 ]
        then
                echo "doing \"set_xio_buffer_size $@\". "
                echo "xml_tag: "variable id=buffer_size" with variable: integer is empty"
                echo "confirm that an appropriate variable id is in \"${EXE_DIR}/$1\" "
                echo "exit"
                echo "error in executing script : set_xio_buffer_size $@" >> ${SETTE_DIR}/output.sette
                echo "....." >> ${SETTE_DIR}/output.sette
                exit 1
        fi
        sed -e "/buffer_size/s:>.*<:>$2<:" ${EXE_DIR}/$1 > ${EXE_DIR}/$1.tmp
        mv ${EXE_DIR}/$1.tmp ${EXE_DIR}/$1

        echo "finished script : set_xio_buffer_size $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}

set_xio_field_defs () {
        minargcount=1
        if [ ${#} -lt ${minargcount} ]
        then
                echo "not enough arguments for set_xio_field_defs"
                echo "${usage5}"
                exit 1
        fi
        unset minargcount
        if [  ! -f ${SETTE_DIR}/output.sette ] ; then
                touch ${SETTE_DIR}/output.sette
        fi

        echo "executing script : set_xio_field_defs $@" >> ${SETTE_DIR}/output.sette
        echo "################" >> ${SETTE_DIR}/output.sette


        [ -f ${EXE_DIR}/field_def_nemo-oce.xml ] || sed -i '/field_def_nemo-oce/d' $1
        [ -f ${EXE_DIR}/field_def_nemo-ice.xml ] || sed -i '/field_def_nemo-ice/d' $1
        [ -f ${EXE_DIR}/field_def_nemo-pisces.xml ] || sed -i '/field_def_nemo-pisces/d' $1

        echo "finished script : set_xio_field_defs $@" >> ${SETTE_DIR}/output.sette
        echo "++++++++++++++++" >> ${SETTE_DIR}/output.sette
        echo "                " >> ${SETTE_DIR}/output.sette
}
