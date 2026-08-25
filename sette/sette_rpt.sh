#!/bin/bash -f
# ======================================================================
#                     ***  SCRIPT  sette_rpt.sh  ***
#  SETTE: simple SETTE report generator
# ======================================================================
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE.txt)
# ----------------------------------------------------------------------
set +x

# This version should be run in the SETTE directory.
# The machine name will be picked up from the sette.sh script but the location of the
# validation directory needs to be set here (currently assumed to reside in the ../cfgs directory)
#
#########################################################################################
######################### Start of function definitions #################################

# report format
format_field1="%-35s"

# Exit codes and warning flags
declare -i {REPRO_EC,RESTA_EC,REFCMP_EC,CPUCMP_EC,OCEOUT_EC,ROT_EC,PHYOPT_EC,VARIANTS_EC,MD_WARN}=0

# List of status time-series files
statfiles=(run.stat tracer.stat obs.stat)

# source sette functions
. ./all_functions.sh

function get_dorv() {
  if [ ${VALID_REV} == 'old' ] ; then
    dorv=`ls -1rt $vdir/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
  else
    dorv=${VALID_REV}
  fi
}

function get_ktdiff() {
  # form diff we used to first line that summarised the diff:
  #   - get all the diff summary lines XX,YY(optional)acdWW,ZZ(optional)
  #   - then the first line
  #   - then get first element using ',','a','c' or 'd' as field separator
  ktdiff=`diff ${1} ${2} | grep -E '^[0-9]+[,.0-9]*[acd][0-9]+[,.0-9]*$' | head -1 | awk -F '[,acd]' '{print $1}'`
}

function rottest() { 
  local err
#
# Rotational symmetry checks. Expects ROT_000, ROT_090, and ROT_180 test-run directories
#
  vdir=$1
  nam=$2

#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
#
# check if directory is here
  if [ ! -d ${db_path} ]; then
    printf "${format_field1} %s %s\n" $nam  "directory                    MISSING :" $dorv
    ROT_EC=$((ROT_EC + 1))
    MD_WARN=1
    return
  fi

  if [ -d ${db_path} ]; then

    # proceed only if output from rotational-symmetry testing test runs is available
    [ ! -d ${db_path}/ROT_000 ] && \
    [ ! -d ${db_path}/ROT_090 ] && \
    [ ! -d ${db_path}/ROT_180 ] && return

    # check ocean output
    runtest $vdir $nam ROT

    # run rotational-symmetry test
    #
    # check incomplete
    for ROT in ROT_000 ROT_180 ROT_090; do
      check_incomplete ${db_path}/$ROT/ $nam
      err=$?
      ROT_EC=$((ROT_EC + err))
      if [ $err != 0 ]; then return ; fi
    done
    #
    # check tracer and run.stat
    for file in ${statfiles[@]}; do
      f1=${db_path}/ROT_000/$file
      f2=${db_path}/ROT_180/$file
      f3=${db_path}/ROT_090/$file

      compare_files $f1 $f2 $file $nam ROT180 $dorv
      ROT_EC=$((ROT_EC + $?))
      #
      compare_files $f1 $f3 $file $nam ROT90 $dorv
      ROT_EC=$((ROT_EC + $?))
    done
  fi
}

function resttest() {
  local err
#
# Restartability checks. Expects LONG and SHORT run directories
# Compares end of LONG stat files with equivalent entries from the SHORT stat files.
#
  vdir=$1
  nam=$2
#
  RESTA_EC=0
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
#
# check if directory is here
  if [ ! -d ${db_path} ]; then
    printf "${format_field1} %s %s\n" $nam  "directory                    MISSING :" $dorv
    RESTA_EC=1
    MD_WARN=1
    return
  fi

  if [ -d ${db_path} ]; then
    #
    # check ocean output
    runtest $vdir $nam $pass RST
    #
    # check incomplete
    for RUN in REF RST; do
      check_incomplete ${db_path}/$RUN/ $nam
      err=$?
      RESTA_EC=$((RESTA_EC + err))
      if [ $err != 0 ]; then return ; fi
    done
    #
    # check run.stat tracer.stat obs.stat
    for file in ${statfiles[@]}; do
      f1=${db_path}/REF/$file
      f2=${db_path}/RST/$file
      compare_files $f1 $f2 $file $nam RESTA $dorv
      RESTA_EC=$(( RESTA_EC + $? ))
    done
    #
  fi
}

function reprotest(){
  local err
#
# Reproducibility checks. Expects REPRO_N_M and REPRO_I_J run directories
# Compares end of stat files from each
#
  vdir=$1
  nam=$2
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
#
# check if directory is here
  if [ ! -d ${db_path} ]; then
    printf "${format_field1} %s %s\n" $nam  "directory                    MISSING :" $dorv
    REPRO_EC=1
    MD_WARN=1
    return
  fi
#
  if [ -d ${db_path} ]; then
    # check ocean output
    runtest $vdir $nam REPRO
    #
    # check reproducibility
    rep1=MPPREF
    rep2=MPP
    if [ $rep1 == $rep2 ]; then
       rep2=''
       # Should this trigger an error ?
    fi
    #
    # check incomplete
    for RUN in $rep1 $rep2; do
      check_incomplete ${db_path}/$RUN/ $nam
      err=$?
      REPRO_EC=$((REPRO_EC + err))
      if [ $err != 0 ]; then return ; fi
    done
    #
    # check run.stat tracer.stat obs.stat
    for file in ${statfiles[@]}; do
      f1=${db_path}/$rep1/$file
      f2=${db_path}/$rep2/$file
      compare_files $f1 $f2 $file $nam REPRO $dorv
      REPRO_EC=$(( REPRO_EC + $? ))
    done
  fi
}

function check_incomplete() {
    local path=$1
    local nam=$2         # Nom de la configuration

    local f1=${path}/run.stat    # Chemin du premier fichier
    local f2=${path}/tracer.stat # Chemin du second fichier

    local err=0
    # check files presence
    if  [ ! -f $f1 ] && [ ! -f $f2 ] ; then
      printf "${format_field1} %s\n" $nam " incomplete test"
      err=1
    else
      err=0
    fi
    #
    return $err
}

function compare_files() {
    local f1=$1          # Chemin du premier fichier
    local f2=$2          # Chemin du second fichier
    local file_name=$3   # Nom du fichier (ex: "run.stat")
    local nam=$4         # Nom de la configuration
    local test_type=$5   # Type de test (ex: "REST")
    local dorv=$6        # Répertoire de révision

    local rtest
    local rref
    local test_name

    # set up names for the various tests
    if [ $test_type == 'RESTA' ]; then
      test_name='restartability'
    elif [ $test_type == 'REPRO' ]; then
      test_name='reproducibility'
    elif [ $test_type == 'ROT90' ]; then
      test_name='90deg rotation'
    elif [ $test_type == 'ROT180' ]; then
      test_name='180deg rotation'
    elif [ $test_type == 'SHA' ]; then
      test_name='sha comparison'
    elif [ $test_type == 'SA' ]; then
      test_name='standalone comp.'
    else
      echo "Error : comparison type not recognised: $test_type"
      return 1
    fi

    msg_passed=("$nam" "$file_name" "$test_name" "passed" ":" "$dorv")
    msg_failed=("$nam" "$file_name" "$test_name" "FAILED" ":" "$dorv" "(results differ after" "$ktdiff" "time steps)")
    fmt_passed="${format_field1} %-11s %-16s %-7s %s %s\n"
    fmt_failed="\e[38;5;160m${format_field1} %-11s %-16s %-7s %s %s %s %-5s %s\e[0m\n"
    ktidx=7

    if [ $test_type == 'SHA' ]; then
      msg_passed=("$nam" "$file_name" "files are identical" "${TESTD}")
      msg_failed=("$nam" "$file_name" "files are DIFFERENT (after" "$ktdiff" "time steps)" "${TESTD}")
      fmt_passed="${format_field1} %-28s %s (%s)\n"
      fmt_failed="${format_field1} %-28s %s %s %-5s (%s)\n"
      ktidx=3
    elif [ $test_type == 'SA' ]; then
      rref=`dirname $f1 | awk -F '/' '{print $NF}'` 
      rtest=`dirname $f2 | awk -F '/' '{print $NF}'` 
      label=`printf "%-7s %s" "${rtest}" "vs ${nam}/${rref}"`
      msg_passed=("${label}" "$file_name" "identical        passed  : ${dorv}")
      msg_failed=("${label}" "$file_name" "differs          FAILED  : ${dorv} (results differ after" "$ktdiff" "time steps)")
      fmt_passed="${format_field1} %-11s %s\n"
      fmt_failed="\e[38;5;160m${format_field1} %-11s %s %s %s\e[0m\n"
      ktidx=3
    fi

    # Compare les fichiers
    if  [ -f $f1 ] && [ -f $f2 ] ; then
       
      # if restartability, only the last X lines are tested
      if [ $test_type == 'RESTA' ]; then
        nl=(`wc -l $f2`)
        tail -${nl[0]} $f1 > f1.tmp$$
        f1=f1.tmp$$
      fi
       
      if cmp -s "$f1" "$f2"; then

	# print passed message
        printf "$fmt_passed" "${msg_passed[@]}"
	 
	# clean tmp file
        if [ $test_type == 'RESTA' ]; then rm -f $f1; fi
	 
	# return error code
        return 0

      else
	
	# get kt
        get_ktdiff $f1 $f2

	# print failed message
        msg_failed[$ktidx]="$ktdiff"

	# print failed message
        printf "$fmt_failed" "${msg_failed[@]}"
	 
	# clean tmp file
        if [ $test_type == 'RESTA' ]; then rm -f $f1; fi
	 
	# return error code
        return 1

      fi
    fi
}

function getavgtime() {
    if [ `grep -c -e 'Average ' $1` -eq 1 ]; then
	grep -e 'Average ' $1 | cut -d '|' -f 3 | sed -e 's/[^0-9\.]//g'
    else
	grep -e 'avg over all MPI processes ' $1 | head -n 1 | sed -e 's/[^0-9\.]//g'
    fi
}

function compare_timing() {
#
# print timing comparison of 2 timing file
  local f1=$1          # Chemin du premier fichier
  local f2=$2          # Chemin du second fichier
  local nam=$3         # Nom de la configuration

  if  [ -f $f1 ] && [ -f $f2 ] ; then
    tnew=$( getavgtime $f1 )
    tref=$( getavgtime $f2 )
    if [[ $? == 0 ]] && [[ -n "${tnew}" ]] && [[ -n "${tref}" ]]; then
      tdif=$( echo ${tnew} ${tref} | awk '{print $1 - $2}')
      if (( $(echo "$tnew > $tref" |bc -l) )); then
        printf "${format_field1} %10s %11s %14s %11s %14s \\e[41;33;196m%10s\\e[0m\n" $nam "ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
      else
        printf "${format_field1} %10s %11s %14s %11s %14s \\e[42;01;196m%10s\\e[0m\n" $nam "ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
      fi
    fi
  fi
}

function runcmpres(){
#
# Compare SETTE test-run output files with reference files from a previous
# SETTE test run
#
  vdir=$1
  nam=$2
  vdirref=$3
  dorvref=$4
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
  db_path_ref="${vdirref}/${dorvref}/${VALID_VAR_REF}/${nam}"
#
# check if reference directory is present
  if [ ! -d ${db_path_ref} ]; then
    printf "${format_field1} %s\n" $nam "REFERENCE directory at $dorvref is MISSING"
    echo " please check ${db_path_ref}"
    REFCMP_EC=1
    return
  fi
  if [ ! -d ${db_path} ]; then
    printf "${format_field1} %s\n" $nam "VALID     directory at $dorv is MISSING"
    echo " please check ${db_path}"
    REFCMP_EC=1
    MD_WARN=1
    return
  fi

#
  if [ -d ${db_path} ]; then
    # Selection of the test run used for the comparison (LONG or one of the reproducibility-test runs)
    TESTD=REF
    #
    # check run.stat tracer.stat obs.stat
    for file in ${statfiles[@]}; do
      f1=${db_path}/${TESTD}/$file
      f2=${db_path_ref}/${TESTD}/$file
      compare_files $f1 $f2 $file $nam SHA $dorv
      REFCMP_EC=$(( REFCMP_EC + $? ))
    done

  fi
}

function runcmptim(){
#
# compare timing.output file with reference file from a previous sette test or previous version
#
  vdir=$1
  nam=$2
  vdirref=$3
  dorvref=$4
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
  db_path_ref="${vdirref}/${dorvref}/${VALID_VAR_REF}/${nam}"
#
# check if reference directory is present
  if [ ! -d ${db_path_ref} ]; then
    CPUCMP_EC=1
    return
  fi
  if [ ! -d ${db_path} ]; then
    CPUCMP_EC=1
    return
  fi

#
  if [ -d ${db_path} ]; then
    # Selection of the test run used for the comparison (LONG or one of the reproducibility-test runs)
    TESTD=REF
    f1a=${db_path}/${TESTD}/timing.output
    f2a=${db_path_ref}/${TESTD}/timing.output
    #
    # Report average CPU time differences (if available)
    compare_timing $f1a $f2a ${nam}
  fi
}

function runtest(){
#
# Check test-run success
#
# This function tests for the presence of the ocean.output file for a specified
# test run (argument 4, test-run name, 'RST' for both restart runs, or 'EXP'
# for PHYOPTS variants) in the test-configuration (argument 2) validation
# sub-directory (argument 1) and for the absence of string "E R R O R" in this
# file during pass 0 or 1 (argument 3) of the test-report generation
#
  vdir=$1                                                   # validation sub-directory
  naml=$2                                                   # test configuration
  ttype=$3                                                  # test-run type: test-run name,
  phyopt=0
  cpl=0
  [[ $ttype == 'RST' ]] && ttype="REF|RST"                  #    'RST' (checks both 'LONG' and 'SHORT' test runs), or
  [[ $ttype == 'EXP' ]] && ttype="^EXP-"      && phyopt=1   #    'EXP' (checks PHYOPTS test runs)
  [[ $ttype == 'CPL' ]] && ttype="^CPL"       && cpl=1      #    'CPL' (checks COUPLING test runs)
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${naml}"
#
# no print needed if the repository is not here (already catch before)
#
  if [ -d ${db_path}/ ]; then
    #
    # apply check for all ttype directory
    rep1=$(ls -rt ${db_path}/ | grep -E $ttype)
    for tdir in $rep1 ; do
       f1o=${db_path}/$tdir/ocean.output
       naml2=$naml
       [ $phyopt == 1 ] && naml2="${naml}/${tdir#EXP-}"
       if  [ ! -f $f1o ] ; then
          printf "${format_field1} %s %s\n" "${naml2}" "ocean.output                 MISSING :" $dorv
          [ $phyopt == 0 ] && OCEOUT_EC=1 && return   # record error and stop testing unless there are
          [ $phyopt == 1 ] && PHYOPT_EC=1             #    further PHYOPTS test variants to be tested
       else
          nerr=`grep 'E R R O R' $f1o | wc -l`
          if [[ $nerr > 0 ]]; then
             printf "\e[38;5;196m${format_field1} %s %s %s\e[0m\n" "${naml2}" "run                          FAILED : " $dorv " ( E R R O R in ocean.output) "
             [ $phyopt == 0 ] && OCEOUT_EC=1 && return   # record error and stop testing unless there are
             [ $phyopt == 1 ] && PHYOPT_EC=1             #    further PHYOPTS test variants to be tested
          elif [ $phyopt == 1 ]; then
             printf "${format_field1} %s %s\n" "${naml2}" "ocean.output phyopts         passed  :" $dorv
          elif [ ${cpl} == 1 ]; then
             printf "${format_field1} %s %s\n" "${naml2}" "ocean.output coupling        passed  :" $dorv
          fi
       fi
    done
  else
     printf "${format_field1} %s %s\n" ${naml} "directory                    MISSING :" $dorv
     [ $phyopt == 0 ] && OCEOUT_EC=1
     [ $phyopt == 1 ] && PHYOPT_EC=1
     MD_WARN=1
     return
  fi
}

function standalonetest(){
#
# Compare time-series output files from a reference run with corresponding
# files produced by a run of an alternative model configuration
#
  vdir=$1    # Validation directory
  nam=$2     # Base configuration name
  rref=$3    # Name of the reference run
  rtest=$4   # Name of the run of the alternative configuration
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
  #
  # check directory presence
  if [ ! -d ${db_path} ] || [ ! -d ${db_path}_${rtest} ]; then
    printf "${format_field1} %-28s %s\n" "${nam}_${rtest} vs ${nam}" "directory" "MISSING"
    VARIANTS_EC=$(( VARIANTS_EC + 1 ))
    return
  fi
  #
  # check stats files
  for testfile in ${statfiles[@]}; do
    f1=${db_path}/${rref}/${testfile}
    f2=${db_path}_${rtest}/${rtest}/${testfile}

    compare_files $f1 $f2 $testfile $nam SA $dorv
    VARIANTS_EC=$(( VARIANTS_EC + $? ))
  done
}

########################### END of function definitions #################################
##                                                                                     ##
##    Main script                                                                      ##
##                                                                                     ##
#########################################################################################
#
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)
# Result comparison is inactive by default
DO_COMPARE=0
#   unless this script is called as 'sette_eval.sh', in which case test types
#   other than the result comparison have to be activated explicitly
script_name=$(basename "${0}")
[[ "${script_name}" == "sette_eval.sh" ]] && SETTE_TEST_TYPES=( "COMPARE" ) && DO_COMPARE=1
# Default compilation-environment identifier
COMPILER='auto'
# LOAD param variable (COMPILER, NEMO_VALIDATION_DIR )
. ./param.default
if [ -f ./param.cfg ]; then
  . ./param.cfg
else
  echo "warning: \"param.cfg\" file not found; SETTE will use default paramaters from \"param.default\" file"
fi
TEST_CONFIGS_AVAILABLE=${TEST_CONFIGS_AVAILABLE[@]:-${TEST_CONFIGS[@]}}     # Workaround for some dated param.cfgs files
if [ -z $USER_INPUT ] ; then USER_INPUT='yes' ; fi        # Default: yes => request user input on decisions.
                                                          # (but may br inherited/imported from sette.sh)
# SETTE-control variables that are available in script sette/sette.sh and their
# default values
USING_QCO='"USING_QCO","yes"'
USING_SI3_1D='"USING_SI3_1D","no"'
USING_XIOS='"USING_XIOS","yes"'
USING_DEBUG='"USING_DEBUG","no"'
USING_TIMING='"USING_TIMING","yes"'
USING_ICEBERGS='"USING_ICEBERGS","yes"'
USING_ABL='"USING_ABL","no"'
USING_EXTRA_HALO='"USING_EXTRA_HALO","no"'
USING_COLLECTIVES='"USING_COLLECTIVES","yes"'
USING_NOGATHER='"USING_NOGATHER","yes"'
USING_TILING='"USING_TILING","yes"'
USING_MPMD='"USING_MPMD","yes"'

# Select automatic generation of the build and run-time-configuration
# identifier based on the architecture-configuration and SETTE-control-option
# settings by default; this can be overridden by secifying a variant with the
# '-v' or '-V' command-line option
VALID_VAR="AUTOMATIC"
VALID_VAR_REF="AUTOMATIC"

# Check of the conformity of the configured compilation-environment identifier
[[ ! "${COMPILER}" =~ ^[[:alnum:].+_-]{1,64}$ ]] && echo "Incompatible compilation-environment name" && exit 1
COMPILER_REF="${COMPILER}"

# Avaliable source-code transformation options: currently only 'passthrough'
SCTRANSFORMS=(passthrough)
# Default source-code transformation option: none
TRANSFORM=""
TRANSFORM_REF=""

# Processing of command-line arguments
rev=""; sha=""; refrev=""; refsha=""
export DOTENV_FILE=""
if [ $# -gt 0 ]; then
  echo ""
  while getopts r:s:R:S:x:un:v:V:m:M:z:Z:d:qXbTitaeCNtAh option; do
     case $option in
        r) rev=$OPTARG
           echo "-r: will use ${rev} revision for current report"
           echo "";;
        s) sha=$OPTARG
           [[ ! ${sha} =~ ^[[:alnum:]]{8}$ ]] && echo "-s: wrong SHA digits number (8 digits needed)" && exit 1
           echo "-s: will use \"${sha}\" SHA (\"$(git show -s --format=%s ${sha})\") for current report"
           echo "";;
        R) refrev=$OPTARG
           DO_COMPARE=1
           ;;
        S) refsha=$OPTARG
           [[ ! ${refsha} =~ ^[[:alnum:]]{8}$ ]] && echo "-S: wrong SHA digits number (8 digits needed)" && exit 1
           echo "-S: will compare current results with \"${refsha}\" SHA (\"$(git show -s --format=%s ${refsha})\")"
           echo ""
           DO_COMPARE=1
           ;;
        x) TEST_TYPES=($OPTARG)
           TEST_TYPES=${TEST_TYPES/CORRUPT/VARIANTS}      # Translation of a legacy option
           TEST_TYPES=${TEST_TYPES/STANDALONE/VARIANTS}   # Translation of a legacy option
           [[ ${TEST_TYPES[*]} =~ .*RESTART.*   ]] && export DO_RESTART=1   || DO_RESTART=0
           [[ ${TEST_TYPES[*]} =~ .*REPRO.*     ]] && export DO_REPRO=1     || DO_REPRO=0
           [[ ${TEST_TYPES[*]} =~ .*PHYOPTS.*   ]] && export DO_PHYOPTS=1   || DO_PHYOPTS=0
           [[ ${TEST_TYPES[*]} =~ .*ROTSYM.*    ]] && export DO_ROTSYM=1    || DO_ROTSYM=0
           [[ ${TEST_TYPES[*]} =~ .*COUPLING.*  ]] && export DO_COUPLING=1  || DO_COUPLING=0
           [[ ${TEST_TYPES[*]} =~ .*COMPARE.*   ]] && export DO_COMPARE=1   || DO_COMPARE=0
           [[ ${TEST_TYPES[*]} =~ .*VARIANTS.*  ]] && export DO_VARIANTS=1  || DO_VARIANTS=0
           echo "-x: will check ${TEST_TYPES[*]} test(s) for current report"
           echo "";;
        u) USER_INPUT='no';;
        n) OPTSTR="$OPTARG"
           TEST_CONFIGS=(${OPTSTR})
           echo "-n: Configuration(s) ${TEST_CONFIGS[@]} will be tested if they are available"
           echo "";;
        # Selection of the build variant (either by selecting the variant
        # identifier or by selecting the relevant options separately (in the
        # case of the reference selection, only the compilation environment and
        # the source-code transformation option can be selected separately):
        #   - variant identifier,
        [vV]) arg="${OPTARG}"
           msg="build and run-time-configuration variant"
           [[ ! "${arg}" =~ ^[[:xdigit:]]{32}$ ]] && echo "-${option}: incompatible identifier of the ${msg}" && exit 1
           [[ "${option}" == "v" ]] && echo "-v: ${msg} ${arg} selected" && VALID_VAR="${arg}"
           [[ "${option}" == "V" ]] && echo "-V: reference ${msg} ${arg} selected" && VALID_VAR_REF="${arg}" && DO_COMPARE=1
           echo "";;
        #   - compilation environment
        [mM]) arg="${OPTARG}"
           msg="compilation environment"
           [[ ! "${arg}" =~ ^[[:alnum:].+_-]{1,64}$ ]] && echo "-${option}: incompatible name of the ${msg}" && exit 1
           [[ "${option}" == "m" ]] && echo "-m: ${msg} '${arg}' selected" && COMPILER="${arg}"
           [[ "${option}" == "M" ]] && echo "-M: reference ${msg} '${arg}' selected" && COMPILER_REF="${arg}" && DO_COMPARE=1
           echo "";;
        #   - source-code-transformation option
        [zZ]) arg=""
           msg="source-code transformation option"
           for sct in ${SCTRANSFORMS[@]}; do [[ "${OPTARG}" == "${sct}" ]] && arg="${sct}"; done
           [[ ! "${arg}" =~ ^[[:alnum:]]{1,64}$ ]] && echo "-${option}: incompatible name of the ${msg}" && exit 1
           [[ -z "${arg}" ]] && echo "-${option}: the requested ${msg} is not available" && exit 1
           [[ "${option}" == "z" ]] && echo "-z: ${msg} '${arg}' selected" && TRANSFORM="${arg}"
           [[ "${option}" == "Z" ]] && echo "-Z: reference ${msg} '${arg}' selected" && TRANSFORM_REF="${arg}" && DO_COMPARE=1
           echo "";;
        #   - SETTE-control values (as mirrored from script sette/sette.sh)
        q) USING_QCO=${USING_QCO/yes/no};;
        X) USING_XIOS=${USING_XIOS/yes/no};;
        b) USING_DEBUG=${USING_DEBUG/no/yes};;
        T) USING_TIMING=${USING_TIMING/yes/no};;
        i) USING_ICEBERGS=${USING_ICEBERGS/yes/no};;
        a) USING_ABL=${USING_ABL/no/yes};;
        e) USING_EXTRA_HALO=${USING_EXTRA_HALO/no/yes};;
        C) USING_COLLECTIVES=${USING_COLLECTIVES/yes/no};;
        N) USING_NOGATHER=${USING_NOGATHER/yes/no};;
        t) USING_TILING=${USING_TILING/yes/no};;
        A) USING_MPMD=${USING_MPMD/yes/no};;
        d) export DOTENV_FILE="${OPTARG}"
           echo "-d: sette will use ${DOTENV_FILE} .env file instead of .git directory to define related variables"
           echo "";;
        # Usage message
        h | *) echo 'Usage: sette_rpt.sh [options]'
               echo
               echo 'Selection of SETTE test-run output:'
               echo ' -r REVISION_number :'
               echo '     display sette results for the specified revision (set old for the latest revision available for each config)'
               echo ' -s commit short (8-digits) SHA :'
               echo '     display sette results for the specified SHA (set old for the latest revision available for each config)'
               echo ' -v build and run-time-configuration variant (overrides indirect selection, see'
               echo '    below)'
               echo ' -n test configuration :'
               echo '     select specific test configurations to be included in the test report'
               echo ' -x test :'
               echo '     select specific tests to be reported (RESTART, REPRO, PHYOPTS, COMPARE, COUPLING, VARIANTS)'
               echo
               echo 'Selection of reference SETTE test-run output:'
               echo ' -R reference REVISION_number :'
               echo '     compare sette results against the specified revision (use to over-ride value set in param.cfg)'
               echo ' -S reference commit short (8-digits) SHA :'
               echo '     compare sette results against the specified SHA (use to over-ride value set in param.cfg)'
               echo ' -V reference build and run-time-configuration variant (overrides indirect'
               echo '    selection, see below)'
               echo
               echo 'Further SETTE option:'
               echo ' -u to run sette_rpt.sh without any user interaction'
               echo
               echo 'Indirect selection of the SETTE build and run-time-configuration variant (these'
               echo 'settings reflect corresponding options of script sette/sette.sh; here, they are'
               echo 'solely used to determine build and run-time-configuration variant identifiers;'
               echo 'options -q, -X, -b, -T, -i, -a, -e, -C, -N, -t, and -A are used to determine'
               echo 'both the variant and the reference variant identifier):'
               echo ' -m compilation environment'
               echo ' -M reference compilation environment'
               echo ' -z source-code transformation option'
               echo ' -Z reference source-code transformation option'
               echo ' -q compilation without QCO option'
               echo ' -X compilation without XIOS support'
               echo ' -b debug compilation option selected'
               echo ' -T timing option suppressed'
               echo ' -a ABL option selected'
               echo ' -e extended halo selected'
               echo ' -C nn_comm=1 option selected'
               echo ' -N ll_nnogather=false selected in global configurations'
               echo ' -t tiling suppressed'
               echo ' -A SPMD mode selected'
               echo ' -d to use .env file instead of .git directory to define related variables'
               echo ''
               exit 42;;
     esac
  done
  shift $((OPTIND - 1))
fi
# if $1 (remaining arguments)
if [[ ! -z $1 ]] ; then rev=$1 ; fi

# Define sette variables from local .git repository (default) or .env file ("-d" option)
if [ -z "${DOTENV_FILE}" ]; then
  set_git_var
else
  set_dotenv_var
fi

# Identifier to select the build and run-time-configuration variant
# (hash-function value)
var="${USING_QCO};${USING_SI3_1D};${USING_XIOS};${USING_DEBUG}"
var="${var};${USING_TIMING};${USING_ICEBERGS};${USING_ABL};${USING_EXTRA_HALO}"
var="${var};${USING_COLLECTIVES};${USING_NOGATHER};${USING_TILING};${USING_MPMD}"
if [[ "${VALID_VAR}" == "AUTOMATIC" ]]; then
  VALID_VAR=$(echo "\"COMPILER\",\"${COMPILER}\";\"TRANSFORM\",\"${TRANSFORM}\";${var}" | md5sum | cut -f 1 -d ' ')
fi
#   and the corresponding comparison-reference identifier
if [[ "${VALID_VAR_REF}" == "AUTOMATIC" ]]; then
  VALID_VAR_REF=$(echo "\"COMPILER\",\"${COMPILER_REF}\";\"TRANSFORM\",\"${TRANSFORM_REF}\";${var}" | md5sum | cut -f 1 -d ' ')
fi
VALID_VAR=$(echo "${VALID_VAR}" | tr '[:upper:]' '[:lower:]' | tr -d -c '[:xdigit:]')
VALID_VAR_REF=$(echo "${VALID_VAR_REF}" | tr '[:upper:]' '[:lower:]' | tr -d -c '[:xdigit:]')
if [[ ${#VALID_VAR} != 32 ]] || [[ ${#VALID_VAR_REF} != 32 ]]; then
  echo "Error: incompatible identifier of the build and run-time-configuration variant"; exit 1
fi

NEMO_VALIDATION_DIR=${NEMO_VALIDATION_DIR}
NEMO_VALIDATION_REF=${NEMO_VALIDATION_REF}
NEMO_VALID=${NEMO_VALIDATION_DIR}
NEMO_VALID_REF=${NEMO_VALIDATION_REF}
[ ! -d "${NEMO_VALID}" ] && echo "${NEMO_VALID} validation directory not found" && exit 1
[ ! -d "${NEMO_VALID_REF}" ] && NEMO_VALID_REF=/path/to/reference/sette/results

# The source-code-revision identifier
if [ -z "${VALID_REV}" ]; then
  if [ -n "${rev}" ]; then     # -r option
    VALID_REV=${rev}
  elif [ -n "${sha}" ]; then   # -s option
    VALID_REV=${sha}
  fi
fi
VALID_REV=$(echo ${VALID_REV} | tr '[:upper:]' '[:lower:]' | tr -d -c '[:xdigit:]+')
if [[ ${#VALID_REV} -lt 8 ]] || [[ ${#VALID_REV} -gt 41 ]]; then
  echo "Error: incompatible source-code-revision identifier" && exit 1
fi
if [ -n "${refrev}" ] ; then
  VALID_REV_REF=${refrev}
elif [ -n "${refsha}" ] ; then
  VALID_REV_REF=${refsha}
else
  VALID_REV_REF=${NEMO_REV_REF}
fi
VALID_REV_REF=$(echo ${VALID_REV_REF} | tr '[:upper:]' '[:lower:]' | tr -d -c '[:xdigit:]+')
if [[ ${#VALID_REV_REF} -lt 8 ]] || [[ ${#VALID_REV_REF} -gt 41 ]]; then
  echo "Error: incompatible reference source-code-revision identifier" && exit 1
fi
if [ -n "${refsha}" ]; then
  if [ -d "$(dirname ${NEMO_VALID_REF}/.)/${VALID_REV_REF}/${VALID_VAR_REF}" ]; then
    NEMO_VALID_REF=$(dirname ${NEMO_VALID_REF}/.)
  else
    echo "${VALID_REV_REF} commit results not found in validation directory"
    VALID_REV_REF=00000000 && NEMO_VALID_REF="/path/to/reference/sette/results"
  fi
fi
echo ""
echo "SETTE validation report generated for : "
echo ""
if [[ $localchanges -gt 0 ]] ; then
  echo "       source-code revision ${VALID_REV} (with local changes)"
  VALID_REV="${VALID_REV}+"
else
  echo "       source-code revision ${VALID_REV}"
fi
echo ""
echo "       build and run-time-configuration variant ${VALID_VAR}:"
echo ""
echo "           COMPILER                 : ${COMPILER}"
echo "           TRANSFORM                : ${TRANSFORM}"
var2="${var#\"}"
var2="${var2%\"}"
for v in ${var2//\";\"/ }; do printf "           %-24s : %s\n" ${v/\",\"/ }; done
echo ""

  # Rotational symmetry 
  if [ ${DO_ROTSYM} -eq 1 ]; then
     echo ""
     echo "   !----rotational symmetry----!   "
     ROTSYM_CONFIGS=(${TEST_CONFIGS[@]/CPL_OASIS})
     for rotational_test in ${ROTSYM_CONFIGS[@]}
     do
        rottest $NEMO_VALID ${rotational_test}
     done
  fi

  # Restartability test
  if [ ${DO_RESTART} -eq 1 ]; then
    echo ""
    echo "   !----restart----!   "
    RESTART_CONFIGS=(${TEST_CONFIGS[@]/ORCA2_ICE_OBS})
    RESTART_CONFIGS=(${RESTART_CONFIGS[@]/CPL_OASIS})
    for restart_test in ${RESTART_CONFIGS[@]}
    do
      [ "${restart_test}" != "ORCA2_ICE_OBS" ] && resttest $NEMO_VALID $restart_test
    done
  fi

  # Reproducibility tests
  if [ ${DO_REPRO} -eq 1 ]; then 
    echo ""
    echo "   !----repro----!   "
    REPRO_CONFIGS=(${TEST_CONFIGS[@]/C1D*})
    REPRO_CONFIGS=(${REPRO_CONFIGS[@]/CPL_OASIS})
    for repro_test in ${REPRO_CONFIGS[@]}
    do
      if [[ ${repro_test} != *"OVERFLOW"* && ${repro_test} != *"LOCK_EXCHANGE"* && ${repro_test} != *"IWAVE"* ]]; then
         reprotest $NEMO_VALID $repro_test
      fi
    done
  fi

  # PHYOPTS tests
  if [ ${DO_PHYOPTS} -eq 1 ]; then
    echo ""
    echo "   !----phyopt----!   "
    PHYOPTS_CONFIGS=(${TEST_CONFIGS[@]/CPL_OASIS})
    for phyopt_test in ${PHYOPTS_CONFIGS[@]}; do
       runtest $NEMO_VALID $phyopt_test "EXP"
    done
  fi

  # Coupling test
  if [ ${DO_COUPLING} -eq 1 ]; then
     echo ""
     echo "   !----coupling----!   "
     COUPLING_CONFIGS=( "CPL_OASIS" )
     for coupling_test in ${TEST_CONFIGS[@]}
     do
        for valid_test in ${COUPLING_CONFIGS[@]} ; do
            [[ ${coupling_test} == ${valid_test} ]] && runtest $NEMO_VALID ${coupling_test} "CPL" && break
        done
     done
  fi

  # Test of standalone model variants
  #   SAO:     comparison of ORCA2_ICE_OBS observation-operator output with
  #            corresponding output from the standalone observation operator
  #            based on ORCA2_ICE_OBS model fields
  #   NOAGRIF: comparison of a run with AGRIF (but without any inset) and
  #            a corresponding run without AGRIF
  if [ ${DO_VARIANTS} -eq 1 ]; then
      echo ""
      echo "   !----standalone----!   "
      #for standalone_run in ORCA2_ICE_OBS:SAO:REF AGRIF_DEMO:NOAGRIF:NOAGRIF; do   # As a workaround for integration testing,
      #   the reference run used for the testing of the ORCA2_ICE_OBS configuration has been adjusted to a run that is unconnected
      #   to the REPRO test type 
      for standalone_run in ORCA2_ICE_OBS:SAO:REF AGRIF_DEMO:NOAGRIF:NOAGRIF; do
          conf=`echo ${standalone_run} | cut -f 1 -d ':'`
          run_test=`echo ${standalone_run} | cut -f 2 -d ':'`
          run_ref=`echo ${standalone_run} | cut -f 3 -d ':'`
          if [[ ${TEST_CONFIGS[@]} =~ "${conf}" ]]; then
              standalonetest ${NEMO_VALID} ${conf} ${run_ref} ${run_test}
          fi
      done
  fi

  # before/after tests
  if [ ${DO_COMPARE:-0} -eq 1 ]; then
    echo ""
    echo "   !----result comparison check----!   "
    if [ $NEMO_VALID_REF != "/path/to/reference/sette/results" ] && [ $VALID_REV_REF != "00000000" ]; then
      echo ''
      echo 'check result differences between :'
      echo "VALID directory : $NEMO_VALID at rev ${VALID_REV}"
      echo 'and'
      echo "REFERENCE directory : $NEMO_VALID_REF at rev $VALID_REV_REF"
      echo ''
      for runcmp_test in ${TEST_CONFIGS[@]}
      do
        runcmpres $NEMO_VALID $runcmp_test $NEMO_VALID_REF $VALID_REV_REF
      done
      echo ''
      echo 'Report timing differences between REFERENCE and VALID (if available) :'
      for repro_test in ${TEST_CONFIGS[@]}
      do
        runcmptim $NEMO_VALID $repro_test $NEMO_VALID_REF $VALID_REV_REF
      done
    else
      echo ''
      echo ' No path or revision for comparison specified. Result are not compare with any other revision. '
      echo ' To do it please fill NEMO_VALID_REF and NEMO_REV_REF in param.cfg. '
      echo ''
    fi
  fi

if [[ ${MD_WARN} -ge 1 ]]; then

    # database path
    get_dorv

    echo
    echo "WARNING: Some test failures are due to missing validation directories"
    echo
    echo "  In case some SETTE test runs associated with this report have yet to complete"
    echo "  or are still scheduled to run, the associated validation output may become"
    echo "  eventually available at"
    echo "  ${NEMO_VALID}/${dorv}/${VALID_VAR}"
    echo
    echo "  It is also possible to exclude specific SETTE tests or SETTE configurations"
    echo "  from the SETTE report using command-line options ('-x' and '-n',"
    echo "  respectively), environment variables ('SETTE_TEST_TYPES' and"
    echo "  'SETTE_TEST_CONFIGS', respectively), or settings in configuration file"
    echo "  'param.cfg' ('TEST_TYPES' and 'TEST_CONFIG_AVAILABLE', respectively)."
    echo
fi
#
# error code
SETTE_EC=$((REPRO_EC+RESTA_EC+REFCMP_EC+CPUCMP_EC+OCEOUT_EC+AGRIF_EC+PHYOPT_EC+ROT_EC+VARIANTS_EC))
echo ""
echo "SETTE Report Exit Code: ${SETTE_EC}"

exit $SETTE_EC
