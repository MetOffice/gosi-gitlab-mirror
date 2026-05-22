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
statfiles="run.stat tracer.stat obs.stat"

function get_dorv() {
  if [ ${VALID_REV} == 'old' ] ; then
    dorv=`ls -1rt $vdir/ | tail -1l `
    dorv=`echo $dorv | sed -e 's:.*/::'`
  else
    dorv=${VALID_REV}
  fi
}

function get_ktdiff() {
  ktdiff=`diff ${1} ${2} | head -2 | grep it | awk '{ print $4 }'`
}

function get_ktdiff2() {
  ktdiff=`diff ${1} ${2} |  head -2 | tail -1l | awk '{print $2}'`
}

function rottest() { 
#
# Rotational symmetry checks. Expects ROT_000, ROT_090, and ROT_180 test-run directories
#
  vdir=$1
  nam=$2
  pass=$3
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
#
# check if directory is here
  if [ ! -d ${db_path} ]; then
    printf "${format_field1} %s %s\n" $nam  "directory                    MISSING :" $dorv
    ROT_EC=1
    MD_WARN=1
    return
  fi

  if [ -d ${db_path} ]; then

    # proceed only if output from rotational-symmetry testing test runs is available
    [ ! -d ${db_path}/ROT_000 ] && \
    [ ! -d ${db_path}/ROT_090 ] && \
    [ ! -d ${db_path}/ROT_180 ] && return

    # check ocean output
    runtest $vdir $nam $pass ROT

    # run rotational-symmetry test
    f1o=${db_path}/ROT_000/ocean.output
    f1s=${db_path}/ROT_000/run.stat
    f1t=${db_path}/ROT_000/tracer.stat
    f2o=${db_path}/ROT_180/ocean.output
    f2s=${db_path}/ROT_180/run.stat
    f2t=${db_path}/ROT_180/tracer.stat
    f3o=${db_path}/ROT_090/ocean.output
    f3s=${db_path}/ROT_090/run.stat
    f3t=${db_path}/ROT_090/tracer.stat

    if  [ ! -f $f1s ] &&  [ ! -f $f1t ] ; then 
      printf "${format_field1} %s\n" $nam " incomplete test"
      ROT_EC=1
      return
    fi
    if  [ ! -f $f2s ] &&  [ ! -f $f2t ] ; then 
      printf "${format_field1} %s\n" $nam " incomplete test"
      ROT_EC=1
      return
    fi
    if  [ ! -f $f3s ] &&  [ ! -f $f3t ] ; then 
      printf "${format_field1} %s\n" $nam " incomplete test"
      ROT_EC=1
      return
    fi
#
    done_oce=0

    if  [  -f $f1s ] && [  -f $f2s ]; then 
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam "run.stat    180deg rotation  passed  :" $dorv
        fi
      else
        get_ktdiff $f1s $f2s
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "run.stat    180deg rotation  FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        ROT_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f2s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f2o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi

    done_oce=0

    if  [  -f $f1s ] && [  -f $f3s ]; then 
      cmp -s $f1s $f3s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam "run.stat     90deg rotation  passed  :" $dorv
        fi
      else
        get_ktdiff $f1s $f3s
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "run.stat     90deg rotation  FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        ROT_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f3s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f3o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi

#
# Check tracer.stat files (if they exist)
#
    if  [  -f $f1t ] && [  -f $f2t ]; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam "tracer.stat 180deg rotation  passed  :" $dorv
        fi
      else
        get_ktdiff2 $f1t $f2t
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "tracer.stat 180deg rotation  FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        ROT_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f2t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi

    if  [  -f $f1t ] && [  -f $f3t ]; then
      cmp -s $f1t $f3t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then 
          printf "${format_field1} %s %s\n" $nam "tracer.stat  90deg rotation  passed  :" $dorv
        fi
      else
        get_ktdiff2 $f1t $f3t
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "tracer.stat  90deg rotation  FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        ROT_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f3t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f3o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi

  fi
}

function resttest() {
#
# Restartability checks. Expects LONG and SHORT run directories
# Compares end of LONG stat files with equivalent entries from the SHORT stat files.
#
  vdir=$1
  nam=$2
  pass=$3
#
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
    # check ocean output
    runtest $vdir $nam $pass RST
    #
    # run restartibility test
    f1o=${db_path}/LONG/ocean.output
    f1s=${db_path}/LONG/run.stat
    f1t=${db_path}/LONG/tracer.stat
    f1h=${db_path}/LONG/obs.stat
    f2o=${db_path}/SHORT/ocean.output
    f2s=${db_path}/SHORT/run.stat
    f2t=${db_path}/SHORT/tracer.stat
    f2h=${db_path}/SHORT/obs.stat

    if  [ ! -f $f1s ] &&  [ ! -f $f1t ] ; then
      printf "${format_field1} %s\n" $nam " incomplete test"
      RESTA_EC=1
      return
    fi
    if  [ ! -f $f2s ] &&  [ ! -f $f2t ] ; then
      printf "${format_field1} %s\n" $nam " incomplete test"
      RESTA_EC=1
      return
    fi
#
    done_oce=0

    if  [  -f $f1s ] && [  -f $f2s ]; then
      nl=(`wc -l $f2s`)
      tail -${nl[0]} $f1s > f1.tmp$$
      cmp -s f1.tmp$$ $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %s %s\n" $nam "run.stat    restartability   passed  :" $dorv
        fi
      else
        get_ktdiff f1.tmp$$ $f2s
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "run.stat    restartability   FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        RESTA_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff f1.tmp$$ $f2s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f2o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check tracer.stat files (if they exist)
#
    if  [  -f $f1t ] && [  -f $f2t ]; then
      nl=(`wc -l $f2t`)
      tail -${nl[0]} $f1t > f1.tmp$$
      cmp -s f1.tmp$$ $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %s %s\n" $nam "tracer.stat restartability   passed  :" $dorv
        fi
      else
        get_ktdiff2 f1.tmp$$ $f2t
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "tracer.stat    restartability   FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        RESTA_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff f1.tmp$$ $f2t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check obs.stat files (if they exist)
#
    if  [  -f $f1h ] && [  -f $f2h ]; then
      nl=(`wc -l $f2h`)
      tail -${nl[0]} $f1h > f1.tmp$$
      cmp -s f1.tmp$$ $f2h
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %s %s\n" $nam "obs.stat    restartability   passed  :" $dorv
        fi
      else
        get_ktdiff2 f1.tmp$$ $f2h
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "obs.stat    restartability   FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        RESTA_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view obs.stat differences"
          read y
          sdiff f1.tmp$$ $f2h
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
    rm f1.tmp$$
  fi
}

function reprotest(){
#
# Reproducibility checks. Expects REPRO_N_M and REPRO_I_J run directories
# Compares end of stat files from each
#
  vdir=$1
  nam=$2
  pass=$3
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
#
# check if directory is here
  if [ ! -d ${db_path} ]; then
    printf "${format_field1} %s %s\n" $nam  "directory                    MISSING :" $dorv
    REPRO_EC=R1
    MD_WARN=1
    return
  fi
#
  if [ -d ${db_path} ]; then
    # check ocean output
    runtest $vdir $nam $pass REPRO
    #
    # check reproducibility
    rep1=`ls -1rt ${db_path}/ | grep REPRO | tail -2l | head -1 `
    rep2=`ls -1rt ${db_path}/ | grep REPRO | tail -1l`
    if [ $rep1 == $rep2 ]; then
       rep2=''
    fi
    f1o=${db_path}/$rep1/ocean.output
    f1s=${db_path}/$rep1/run.stat
    f1t=${db_path}/$rep1/tracer.stat
    f1h=${db_path}/$rep1/obs.stat
    f2o=${db_path}/$rep2/ocean.output
    f2s=${db_path}/$rep2/run.stat
    f2t=${db_path}/$rep2/tracer.stat
    f2h=${db_path}/$rep2/obs.stat

    if  [ ! -f $f1s ] && [ ! -f $f1t ] ; then
      printf "${format_field1} %s\n" $nam " incomplete test"
      REPRO_EC=1
      return
    fi
    if  [ ! -f $f2s ] && [ ! -f $f2t ] ; then
      printf "${format_field1} %s\n" $nam " incomplete test"
      REPRO_EC=1
      return
    fi
#
    done_oce=0

    if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %s %s\n" $nam  "run.stat    reproducibility  passed  :" $dorv
        fi
      else
        get_ktdiff $f1s $f2s
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "run.stat    reproducibility  FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        REPRO_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f2s
          echo "<return> to view ocean.output differences"
          read y
          sdiff $f1o $f2o | grep "|"
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check tracer.stat files (if they exist)
#
    if  [ -f $f1t ] && [ -f $f2t ] ; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then           printf "${format_field1} %s %s\n" $nam "tracer.stat reproducibility  passed  :" $dorv
        fi
      else
        get_ktdiff2 $f1t $f2t
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "tracer.stat reproducibility  FAILED : " $dorv " (results are different after " $ktdiff " time steps)"
        REPRO_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f2t
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
    fi
#
# Check obs.stat files (if they exist)
#
    if  [ -f $f1h ] && [ -f $f2h ] ; then
      cmp -s $f1h $f2h
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then           printf "${format_field1} %s %s\n" $nam "obs.stat    reproducibility  passed  :" $dorv
        fi
      else
        get_ktdiff2 $f1h $f2h
        printf "\e[38;5;196m${format_field1} %s %s %s %-5s %s\e[0m\n" $nam "obs.stat    reproducibility  FAILED  :" $dorv " (results are different after " $ktdiff " time steps)"
        REPRO_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view obs.stat differences"
          read y
          sdiff $f1h $f2h
#
# Only offer ocean.output view if it has not been viewed previously
#
          if [ $done_oce == 0 ]; then
            echo "<return> to view ocean.output differences"
            read y
            sdiff $f1o $f2o | grep "|"
          fi
          echo "<return> to continue"
          read y
        fi
      fi
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

function runcmpres(){
#
# Compare SETTE test-run output files with reference files from a previous
# SETTE test run
#
  vdir=$1
  nam=$2
  vdirref=$3
  dorvref=$4
  pass=$5
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
    TESTD=$(ls -1 ${db_path}/ | grep -m 1 -e '^LONG$' -e '^REPRO_'); TESTD=${TESTD:-LONG}
    f1s=${db_path}/${TESTD}/run.stat
    f1t=${db_path}/${TESTD}/tracer.stat
    f1h=${db_path}/${TESTD}/obs.stat
    f2s=${db_path_ref}/${TESTD}/run.stat
    f2t=${db_path_ref}/${TESTD}/tracer.stat
    f2h=${db_path_ref}/${TESTD}/obs.stat
    if  [ ! -f $f1s ] && [ ! -f $f1t ] ; then
      printf "${format_field1} %s\n" $nam "incomplete test"
      REFCMP_EC=1
      return
    fi
    if  [ ! -f $f2s ] && [ ! -f $f2t ] ; then
      printf "${format_field1} %s\n" $nam "incomplete test"
      REFCMP_EC=1
      return
    fi
#
    done_oce=0

    if  [ -f $f1s ] && [ -f $f2s ] ; then
      cmp -s $f1s $f2s
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %-28s %s (%s)\n" $nam "run.stat" "files are identical " ${TESTD}
        fi
      else
        get_ktdiff $f1s $f2s
        printf "${format_field1} %-28s %s %s %-5s (%s)\n" $nam "run.stat" "files are DIFFERENT (results are different after " $ktdiff " time steps) " ${TESTD}
        REFCMP_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view run.stat differences"
          read y
          sdiff $f1s $f2s
          done_oce=1
          echo "<return> to continue"
          read y
        fi
      fi
    fi
    # Check tracer.stat files (if they exist)
#
    if  [ -f $f1t ] && [ -f $f2t ] ; then
      cmp -s $f1t $f2t
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %-28s %s (%s)\n" $nam "tracer.stat" "files are identical " ${TESTD}
        fi
      else
        get_ktdiff2 $f1t $f2t
        printf "${format_field1} %-28s %s %s %-5s (%s)\n" $nam "tracer.stat" "files are DIFFERENT (results are different after " $ktdiff " time steps) " ${TESTD}
        REFCMP_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view tracer.stat differences"
          read y
          sdiff $f1t $f2t
        fi
      fi
    fi
    # Check obs.stat files (if they exist)
#
    if  [ -f $f1h ] && [ -f $f2h ] ; then
      cmp -s $f1h $f2h
      if [ $? == 0 ]; then
        if [ $pass == 0 ]; then
          printf "${format_field1} %-28s %s (%s)\n" $nam "obs.stat" "files are identical " ${TESTD}
        fi
      else
        get_ktdiff2 $f1h $f2h
        printf "${format_field1} %-28s %s %s %-5s (%s)\n" $nam "obs.stat" "files are DIFFERENT (results are different after " $ktdiff " time steps) " ${TESTD}
        REFCMP_EC=1
#
# Offer view of differences on the second pass
#
        if [ $pass == 1 ]; then
          echo "<return> to view obs.stat differences"
          read y
          sdiff $f1h $f2h
        fi
      fi
    fi
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
  pass=$5
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
    TESTD=$(ls -1 ${db_path}/ | grep -m 1 -e '^LONG$' -e '^REPRO_'); TESTD=${TESTD:-LONG}
    f1a=${db_path}/${TESTD}/timing.output
    f2a=${db_path_ref}/${TESTD}/timing.output
#
# Report average CPU time differences (if available)
#
    if  [ -f $f1a ] && [ -f $f2a ] ; then
      tnew=$( getavgtime $f1a )
      tref=$( getavgtime $f2a )
      if [[ $? == 0 ]] && [[ -n "${tnew}" ]] && [[ -n "${tref}" ]]; then
        if [ $pass == 0 ]; then
          tdif=$( echo ${tnew} ${tref} | awk '{print $1 - $2}')
          if (( $(echo "$tnew > $tref" |bc -l) )); then
            printf "${format_field1} %10s %10s %14s %10s %14s \\e[41;33;196m%10s\\e[0m\n" $nam "ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
          else
            printf "${format_field1} %10s %10s %14s %10s %14s \\e[42;01;196m%10s\\e[0m\n" $nam "ref. time:" $tref "cur. time:" $tnew "diff.:" $tdif
          fi
        fi
      fi
    fi
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
  pass=$3                                                   # pass (0 or 1)
  ttype=$4                                                  # test-run type: test-run name,
  phyopt=0
  cpl=0
  [[ $ttype == 'RST' ]] && ttype="LONG|SHORT"               #    'RST' (checks both 'LONG' and 'SHORT' test runs), or
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
          if [ $pass == 0 ]; then printf "${format_field1} %s %s\n" "${naml2}" "ocean.output                 MISSING :" $dorv ; fi
          [ $phyopt == 0 ] && OCEOUT_EC=1 && return   # record error and stop testing unless there are
          [ $phyopt == 1 ] && PHYOPT_EC=1             #    further PHYOPTS test variants to be tested
       else
          nerr=`grep 'E R R O R' $f1o | wc -l`
          if [[ $nerr > 0 ]]; then
             printf "\e[38;5;196m${format_field1} %s %s %s\e[0m\n" "${naml2}" "run                          FAILED : " $dorv " ( E R R O R in ocean.output) "
             if [ $pass == 1 ]; then
                echo "<return> to view end of ocean.output"
                read y
                tail -100 $f1o
                echo ''
                echo "full ocean.output available here: $f1o"
             fi
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
     if [ $pass == 0 ]; then printf "${format_field1} %s %s\n" ${naml} "directory                    MISSING :" $dorv ; fi
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
  pass=$5    # First (0) or second (1) pass
#
# database path
  get_dorv
  db_path="${vdir}/${dorv}/${VALID_VAR}/${nam}"
#
  EC0=1   # Initial test error code
  if [ ! -d ${db_path} ] || [ ! -d ${db_path}_${rtest} ]; then
    printf "${format_field1} %-28s %s\n" "${nam}_${rtest} vs ${nam}" "directory" "MISSING"
  fi
  for testfile in ${statfiles}; do
    f1=${db_path}/${rref}/${testfile}
    f2=${db_path}_${rtest}/${rtest}/${testfile}
    if [ -f ${f1} ] && [ -f ${f2} ]; then
      EC=1   # Initial test-file error code
      label=`printf "%-7s %s" "${rtest}" "vs ${nam}/${rref}"`
      # Comparison
      cmp -s ${f1} ${f2}
      if [ $? == 0 ]; then
        if [ ${pass} == 0 ]; then
          printf "${format_field1} %-11s %s\n" "${label}" "${testfile}" "identical        passed  : ${dorv}"
          EC=0    # Mark current comparison as succesful
          EC0=0   # Mark overall test as successful
        fi
      else
        if [ ${testfile} == 'run.stat' ]; then
          get_ktdiff ${f1} ${f2}
        else
          get_ktdiff2 ${f1} ${f2}
        fi
        printf "\e[38;5;196m${format_field1} %-11s %s\e[0m\n" "${label}" "${testfile}" "differs          FAILED  : ${dorv} (results differ after ${ktdiff} time steps)"
        if [ ${pass} -eq 1 ]; then
          echo "<enter> to view the difference"
          read y
          sdiff $f1 $f2
          echo "<enter> to continue"
          read y
        fi
      fi
      [ ${EC} == 1 ] && VARIANTS_EC=1   # Unsuccessful comparison found
    fi
  done
  [ ${EC0} == 1 ] && VARIANTS_EC=1   # No successful comparison found
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
if [ $# -gt 0 ]; then
  echo ""
  while getopts r:s:R:S:x:un:v:V:m:M:z:Z:qXbTitaeCNtAh option; do
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
           [[ -z "${arg}" ]] && echo "-${option}: the requested ${msg} is not available"; exit 1
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
               echo ''
               exit 42;;
     esac
  done
  shift $((OPTIND - 1))
fi
# if $1 (remaining arguments)
if [[ ! -z $1 ]] ; then rev=$1 ; fi

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
localchanges=0
if [ -n "${rev}" ]; then     # -r option
  VALID_REV=${rev}
elif [ -n "${sha}" ]; then   # -s option
  VALID_REV=${sha}
else                         # enquire local source-code repository
  VALID_REV=${CI_COMMIT_SHORT_SHA:-$(git -C ${MAIN_DIR} rev-parse --short=8 HEAD 2> /dev/null)}
  localchanges=`git status --short -uno | wc -l`
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

#
for pass in  $RPT_PASSES
do

  if [ $pass == 0 ]; then
    echo ""
    echo "!!---------------1st pass------------------!!"
  fi
  if [ $pass == 1 ]; then
     echo ""
     echo "!!---------------2nd pass------------------!!"
  fi

  # Rotational symmetry 
  if [ ${DO_ROTSYM} -eq 1 ]; then
     echo ""
     echo "   !----rotational symmetry----!   "
     ROTSYM_CONFIGS=(${TEST_CONFIGS[@]/CPL_OASIS})
     for rotational_test in ${ROTSYM_CONFIGS[@]}
     do
        rottest $NEMO_VALID ${rotational_test} $pass
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
      [ "${restart_test}" != "ORCA2_ICE_OBS" ] && resttest $NEMO_VALID $restart_test $pass
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
         reprotest $NEMO_VALID $repro_test $pass
      fi
    done
  fi

  # PHYOPTS tests
  if [ ${DO_PHYOPTS} -eq 1 ]; then
    echo ""
    echo "   !----phyopt----!   "
    PHYOPTS_CONFIGS=(${TEST_CONFIGS[@]/CPL_OASIS})
    for phyopt_test in ${PHYOPTS_CONFIGS[@]}; do
       runtest $NEMO_VALID $phyopt_test $pass "EXP"
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
            [[ ${coupling_test} == ${valid_test} ]] && runtest $NEMO_VALID ${coupling_test} $pass "CPL" && break
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
      #for standalone_run in ORCA2_ICE_OBS:SAO:REPRO_8_4 AGRIF_DEMO:NOAGRIF:ORCA2; do   # As a workaround for integration testing,
      #   the reference run used for the testing of the ORCA2_ICE_OBS configuration has been adjusted to a run that is unconnected
      #   to the REPRO test type 
      for standalone_run in ORCA2_ICE_OBS:SAO:ORCA2 AGRIF_DEMO:NOAGRIF:ORCA2; do
          conf=`echo ${standalone_run} | cut -f 1 -d ':'`
          run_test=`echo ${standalone_run} | cut -f 2 -d ':'`
          run_ref=`echo ${standalone_run} | cut -f 3 -d ':'`
          if [[ ${TEST_CONFIGS[@]} =~ "${conf}" ]]; then
              standalonetest ${NEMO_VALID} ${conf} ${run_ref} ${run_test} ${pass}
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
        runcmpres $NEMO_VALID $runcmp_test $NEMO_VALID_REF $VALID_REV_REF $pass
      done
      echo ''
      echo 'Report timing differences between REFERENCE and VALID (if available) :'
      for repro_test in ${TEST_CONFIGS[@]}
      do
        runcmptim $NEMO_VALID $repro_test $NEMO_VALID_REF $VALID_REV_REF $pass
      done
    else
      echo ''
      echo ' No path or revision for comparison specified. Result are not compare with any other revision. '
      echo ' To do it please fill NEMO_VALID_REF and NEMO_REV_REF in param.cfg. '
      echo ''
    fi
  fi

done

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
# error code
SETTE_EC=$((REPRO_EC+RESTA_EC+REFCMP_EC+CPUCMP_EC+OCEOUT_EC+AGRIF_EC+PHYOPT_EC+ROT_EC+VARIANTS_EC))
echo ""
echo "SETTE Report Exit Code: ${SETTE_EC}"

exit $SETTE_EC
