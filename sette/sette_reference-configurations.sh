#!/bin/bash
############################################################
# Author : Simona Flavoni for NEMO
# Contact: sflod@locean-ipsl.upmc.fr
# 2013   : A.C. Coward added options for testing with XIOS in dettached mode
#
# sette.sh   : principal script of SET TEsts for NEMO (SETTE)
#############################################################
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (./LICENSE.txt)
# ----------------------------------------------------------------------
#set -vx
set -o posix
#set -u
#set -e
# ===========
# DESCRIPTION
# ===========
#
# Variables to be checked by user:
#
# COMPILER          : name of compiler as defined in NEMOGCM/ARCH directory 
# BATCH_COMMAND_PAR :  name of the command for submitting parallel batch jobs
# BATCH_COMMAND_SEQ :  name of the command for submitting sequential batch jobs  
# INTERACT_FLAG     : flag to run in interactive mode "yes"
#                           to run in batch mode "no"
# MPIRUN_FLAG       : flag to run in parallel (MPI) "yes"
#                           to run in sequential mode (NB_PROC = 1) "no"

# NUM_XIOSERVERS    : number of stand-alone IO servers to employ
#                     set to zero if USING_MPMD="no"
#
# Principal script is sette.sh, that calls 
#
#  makenemo  : to create successive exectuables in ${CONFIG_NAME}/BLD/bin/nemo.exe 
#              and links to nemo in ${CONFIG_NAME}/EXP00)
#
#  param.cfg : sets and loads following directories:
#              This will have been run by the parent sette.sh and values exported here
#
#   FORCING_DIR         : is the directory for forcing files (tarfile)
#   INPUT_DIR           : is the directory for input files storing 
#   TMPDIR              : is the temporary directory (if needed)
#   NEMO_VALIDATION_DIR : is the validation directory
#
#   (NOTE: this file is the same for all configrations to be tested with sette)
#
#   all_functions.sh : loads functions used by sette (note: new functions can be added here)
#   set_namelist     : function declared in all_functions that sets namelist parameters 
#   post_test_tidyup : creates validation storage directory and copies required output files 
#                      (run.stat and ocean.output) in it after execution of test.
#
#  VALIDATION tree is:
#
#   NEMO_VALIDATION_DIR/WCONFIG_NAME/WCOMPILER_NAME/TEST_NAME/REVISION
#
#  prepare_exe_dir.sh : defines and creates directory where the test is executed
#                       execution directory takes name of TEST_NAME defined for every test 
#                       in sette.sh. (each test in executed in its own directory)
#
#  set_valid_dir       : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#
#  clean_valid_dir    : rename ocean.output/run.stat and tracer.stat to avoid checking them in the report 
#                       ( not doing it could lead to false positive )
#
#  prepare_job.sh     : to generate the script run_job.sh
#
#  fcm_job.sh         : run in batch (INTERACT_FLAG="no") or interactive (INTERACT_FLAG="yes")
#                        see sette.sh and BATCH_TEMPLATE directory
#
#  NOTE: jobs requiring initial or forcing data need to have an input_CONFIG.cfg in which 
#        can be found paths to the input tar file)
#  NOTE: if job is not launched for any reason you have the executable ready in ${EXE_DIR} 
#        directory
#  NOTE: the changed namelists are left in ${EXE_DIR} directory whereas original namelists 
#        remain in ${NEW_CONF}/EXP00
# 
#  NOTE: a log file, output.sette, is created in ${SETTE_DIR} with the echoes of 
#        executed commands
#
#  NOTE: if sette.sh is stopped in output.sette there is written the last command 
#        executed by sette.sh
#
# example use: ./sette.sh 
#########################################################################################
#
# LOAD param value
SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)

export BATCH_COMMAND_PAR=${BATCH_CMD}
export BATCH_COMMAND_SEQ=${BATCH_CMD}
export INTERACT_FLAG="no"
export MPIRUN_FLAG="yes"
#
# Settings which control the use of stand alone servers (only relevant if using xios)
#
export NUM_XIOSERVERS=4
export JOB_PREFIX=${JOB_PREFIX_MPMD}
#
if [ ${USING_MPMD} == "no" ] 
 then
   export NUM_XIOSERVERS=0
   export JOB_PREFIX=${JOB_PREFIX_NOMPMD}
fi
#

# Directory to run the tests
CONFIG_DIR0=${MAIN_DIR}/cfgs
TOOLS_DIR=${MAIN_DIR}/tools

CMP_NAM=${1:-$COMPILER}
# Architecture names that start in 'auto-' trigger the automatic generation of architecture configuration files
[[ "${CMP_NAM}" != "${CMP_NAM#auto-}" ]] && CMP_NAM_A='auto' || CMP_NAM_A=${CMP_NAM}

# Copy job_batch_CMP_NAM file for specific compiler into job_batch_template
cd ${SETTE_DIR}
cp BATCH_TEMPLATE/${JOB_PREFIX}-${CMP_NAM} job_batch_template || exit 1
# Description of available configurations:
# GYRE_PISCES       :
# GYRE_GO           : GYRE without TOP and PISCES, with non-linear free surface, and with GEOMETRIC and OSMOSIS
# ORCA2_ICE_PISCES  :
# ORCA2_OFF_PISCES  :
# AMM12             :
# ORCA2_SAS_ICE     :
# ORCA2_ICE_OBS     :
#    -> variant ORCA2_ICE_OBS_SAO: test SAO
# AGRIF_DEMO        : test AGRIF in a double zoom configuration in the nordic seas + 1 zoom in the eq. Pacific and
#    -> variant AGRIF_DEMO_NOAGRIF: check that key_agrif without zoom = no key_agrif
# WED025            : regional configuration including sea-ice and tides (Spitzbergen)
# C1D               :
CONFIGS_AVAILABLE=( GYRE_PISCES GYRE_GO ORCA2_ICE_PISCES ORCA2_OFF_PISCES AMM12 ORCA2_SAS_ICE ORCA2_ICE_OBS AGRIF_DEMO WED025 C1D )

. ./all_functions.sh
for config in ${TEST_CONFIGS[@]} ; do

    # Ignore unavailable configurations
    DO_SKIP=1
    for config_available in ${CONFIGS_AVAILABLE[@]} ; do [[ "${config}" == "${config_available}" ]] && DO_SKIP=0 ; done
    [[ ${config} =~ "C1D" ]] && DO_SKIP=0   # Enable testing of C1D variants
    [[ ${DO_SKIP} -eq 1 ]] && continue

    # Each SETTE configuration is associated with a NEMO reference
    # configuration or test case; in most cases the names match
    ref_config="${config}"
    #   but in some cases they do not
    [[ ${config} == "ORCA2_ICE_OBS"    ]] && ref_config="ORCA2_ICE_PISCES"
    [[ ${config} == "GYRE_GO"          ]] && ref_config="GYRE_PISCES"
    [[ ${config} =~ "C1D"              ]] && ref_config="${config/_*}"

    # Most SETTE configurations can be compiled with the default list of
    # components (an empty string avoids overriding the default selection)
    COMPONENTS=""
    #   but for some configurations this list is adjusted
    [[ ${config} == "GYRE_GO"          ]] && COMPONENTS="OCE"
    [[ ${config} == "ORCA2_ICE_OBS"    ]] && COMPONENTS="OCE ICE"
    [[ ${config} =~ "C1D" ]] && [[ ${config} =~ "SAS" ]] && COMPONENTS="OCE ICE TOP SAS"

    # Most SETTE configurations are compiled with the common modification of
    # the pre-processing-key list
    ADD_KEYS_LOC="${ADD_KEYS}"
    DEL_KEYS_LOC="${DEL_KEYS}"
    #   but adjustments are made for specific configurations:
    #    - override key_qco as configurations GYRE_PISCES, ORCA2_OFF_PISCES,
    #      ORCA2_SAS_ICE, and C1D variants use key_linssh
    [[ ${config} == "GYRE_PISCES"      ]] && ADD_KEYS_LOC="${ADD_KEYS/key_qco/}"
    [[ ${config} == "ORCA2_OFF_PISCES" ]] && ADD_KEYS_LOC="${ADD_KEYS/key_qco/}"
    [[ ${config} == "ORCA2_SAS_ICE"    ]] && ADD_KEYS_LOC="${ADD_KEYS/key_qco/}"
    [[ ${config} =~ "C1D"              ]] && ADD_KEYS_LOC="${ADD_KEYS/key_qco/}"
    #    - override key_qco as configuration WED025 uses ln_hpg_isf
    [[ ${config} == "WED025"           ]] && ADD_KEYS_LOC="${ADD_KEYS/key_qco/}"
    #    - override key_linssh (if selected) and key_top for configuration
    #    GYRE_GO
    [[ ${config} == "GYRE_GO"          ]] && ADD_KEYS_LOC="${ADD_KEYS/key_linssh/}" \
                                          && DEL_KEYS_LOC="${DEL_KEYS} key_linssh key_top"
    #    - override key_top and ensure key_linssh and key_asminc for
    #      configuration ORCA2_ICE_OBS
    [[ ${config} == "ORCA2_ICE_OBS"    ]] && ADD_KEYS_LOC="${ADD_KEYS} key_linssh key_asminc" \
                                          && DEL_KEYS_LOC="${DEL_KEYS} key_qco key_top"
    #    - remove key_top for C1D variant C1D_ASICS
    [[ ${config} =~ "C1D" ]] && [[ ${config} =~ "ASICS" ]] && DEL_KEYS_LOC="${DEL_KEYS} key_top"

    # Variants
    config_var="${config}"               # Default: no variant
    COMPONENTS_VAR="${COMPONENTS}"       # Default: no change to sub-components for variant
    DEL_KEYS_VAR_LOC="${DEL_KEYS_LOC}"   # Default: matching key modification between reference and variant
    [[ ${config} == "ORCA2_ICE_OBS" ]] && config_var="${config}_SAO"     && COMPONENTS_VAR="${COMPONENTS} SAO"
    [[ ${config} == "AGRIF_DEMO"    ]] && config_var="${config}_NOAGRIF" && DEL_KEYS_VAR_LOC="${DEL_KEYS_LOC} key_agrif key_agrif_psisters"

    # PSyclone-based source-code processing (if required)
    TRANSFORM_OPT=""
    [[ -n "${TRANSFORM}" ]] && TRANSFORM_OPT="-p ${TRANSFORM}"

    CONFIG_SUFFIX=${SETTE_STG}
    DO_RESTART_1=${DO_RESTART}
    DO_RESTART_2=${DO_RESTART}
    DO_REPRO_1=${DO_REPRO}
    DO_REPRO_2=${DO_REPRO}
    DO_VARIANTS_0=${DO_VARIANTS}
    # Name of the reference run for the VARIANTS test type
    VARIANTS_REF="ORCA2"
    # One of the REPRO test runs can be utilised as a reference run for the VARIANTS test of configuration ORCA2_ICE_OBS
    #[[ ${config} == "ORCA2_ICE_OBS" ]] && [[ ${DO_VARIANTS} == "1" ]] && VARIANTS_REF="" && DO_REPRO_2="1"   # This
    #   adjustment has been disabled as a workaround for integration testing, in order to avoid the running of the same test
    #   run twice in parallel by the current integration-testing implementation when both the REPRO and VARIANTS test types
    #   have been selected

    # The actual names of the NEMO builds that are associated with the
    # currently processed SETTE configuration
    SETTE_CONFIG="${config}${CONFIG_SUFFIX}"
    SETTE_CONFIG_VAR="${config_var}${CONFIG_SUFFIX}"

    # Compilation of the baseline configuration
    if [ ${DO_COMPILE_BASELINE} -eq 1 ] ; then

        cd ${MAIN_DIR}

        # Configuration-specific pre-build actions
        if [[ ${config} =~ "C1D" ]] ; then
            # change EXPREF symlink
            rm -fv ${CONFIG_DIR0}/${config/_*}/EXPREF
            if [[ ${config} == "C1D" || ${config} == "C1D_PAPA" ]]; then
                ln -svr ${CONFIG_DIR0}/${config/_*}/EXP_PAPA ${CONFIG_DIR0}/${config/_*}/EXPREF
            else
                ln -svr ${CONFIG_DIR0}/${config/_*}/EXP_${config#*_} ${CONFIG_DIR0}/${config/_*}/EXPREF
            fi
        fi

        # Cleaning of pre-existing target configurations
        clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        # Synchronisation of pre-existing target configurations
        sync_config ${CONFIG_DIR0}/${ref_config} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}
        # Start the build process
        ./makenemo -m ${CMP_NAM_A} -n ${SETTE_CONFIG} -r ${ref_config} ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 \
                   ${NEMO_DEBUG} -j ${CMPL_CORES} ${COMPONENTS:+-d "${COMPONENTS}"} ${TRANSFORM_OPT} \
                   add_key "${ADD_KEYS_LOC}" del_key "${DEL_KEYS_LOC}" || exit 1

        # Configuration-specific post-build actions
        if [[ ${config} =~ "C1D" ]] ; then
            # restore EXPREF symlink
            rm -fv ${CONFIG_DIR0}/${config/_*}/EXPREF
            ln -svr ${CONFIG_DIR0}/${config/_*}/EXP_PAPA ${CONFIG_DIR0}/${config/_*}/EXPREF
        fi

    fi

    # Compilation of a configuration variant (if any)
    if [ ${DO_COMPILE_VARIANTS} -eq 1 ] ; then

        cd ${MAIN_DIR}

        # Building of variants (if requested)
        if [[ ${DO_VARIANTS} -eq 1 ]] && [[ "${config_var}" != "${config}" ]]; then
            if [ ! ${config} == "AGRIF_DEMO" ] || [ ${DO_VARIANTS_0} -eq 1 ]; then
            # Cleaning and synchronisation of the target directory
            clean_config ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG_VAR}
            sync_config ${CONFIG_DIR0}/${config_ref} ${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG_VAR}
            # Build the NEMO executable for the configuration variant
            ./makenemo -m ${CMP_NAM_A} -n ${SETTE_CONFIG_VAR} -r ${ref_config} ${CUSTOM_DIR:+-t ${CMP_DIR}} -k 0 \
                       ${NEMO_DEBUG} -j ${CMPL_CORES} ${COMPONENTS_VAR:+-d "${COMPONENTS_VAR}"} ${TRANSFORM_OPT} \
                       add_key "${ADD_KEYS_LOC}" del_key "${DEL_KEYS_VAR_LOC}" || exit 1
            fi
        fi

    fi

    # Continue to next configuration unless the RUN test phase has been requested
    [[ ${DO_RUN} -eq 0 ]] && break

# -----------
# GYRE_PISCES
# -----------
if [ ${config} == "GYRE_PISCES" ] ; then

    ITEND=1080   # 90 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=12   # 1 day

    # Configure and submit test runs for the GYRE_PISCES SETTE configuration
    # (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" ] ; then

        # Default test-run configuration for the GYRE_PISCES SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"GYREPIS\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 2
        set_namelist namelist_cfg jpnj 4
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist namelist_top_cfg ln_trcbc  .false.
        # put ln_ironsed, ln_hydrofe to false
        # if not you need input files, and for tests is not necessary
        set_namelist namelist_pisces_cfg ln_varpar .false.
        set_namelist namelist_pisces_cfg ln_ironsed .false.
        set_namelist namelist_pisces_cfg ln_ironice .false.
        set_namelist namelist_pisces_cfg ln_hydrofe .false.
        # put ln_pisdmp to false : no restoring to global mean value
        set_namelist namelist_pisces_cfg ln_pisdmp .false.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=8

        ## Restartability tests for GYRE_PISCES
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"GYREPIS_LONG\"
            set_namelist_rst namelist ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"GYREPIS_SHORT\"
            set_namelist_rst namelist ${ITEND} "GYREPIS_LONG" "OCE TOP"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for GYRE_PISCES
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_2_4"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_4_2"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_2_4" ]]; then
                set_namelist namelist_cfg cn_exp \"GYREPIS_48\"
            else
                set_namelist namelist_cfg cn_exp \"GYREPIS_84\"
                set_namelist namelist_cfg jpni 4
                set_namelist namelist_cfg jpnj 2
            fi
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi # GYRE_PISCES

# ----------------------------------------------------------------------------
# GYRE_GO (based on GYRE_PISCES, without TOP and PISCES, with activated
# GEOMETRIC and OSMOSIS schemes, with QCO, and with higher spatial resolution)
# ----------------------------------------------------------------------------
if [ ${config} == "GYRE_GO" ] ; then

    ITEND=1152   # 90 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=16   # 1.25 days

    # Configure and submit runs for the GYRE_GO SETTE configuration (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" ] ; then

        # Default test-run configuration for the GYRE_GO SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"GYREPIS\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 2
        set_namelist namelist_cfg jpnj 4
        # Increase horizontal and vertical resolution
        set_namelist namelist_cfg nn_GYRE 2
        set_namelist namelist_cfg jpkglo 75
        set_namelist namelist_cfg rn_Dt 6750.
        # Accomodate QCO option
        set_namelist namelist_cfg ln_hpg_zco .false.
        set_namelist namelist_cfg ln_hpg_sco .true.
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        # Activate the GEOMETRIC scheme
        set_namelist namelist_cfg ln_ldfeiv .true.
        set_namelist namelist_cfg nn_aei_ijk_t 32
        set_namelist namelist_cfg nn_eke_opt 2
        set_namelist namelist_cfg ln_adv_wav .true.
        # Activate the OSMOSIS scheme
        set_namelist namelist_cfg ln_zdftke .false.
        set_namelist namelist_cfg ln_zdfevd .false.
        set_namelist namelist_cfg ln_zdfddm .false.
        set_namelist namelist_cfg ln_zdfosm .true.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=8

        ## Restartability tests for GYRE_GO
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"GYREGO_LONG\"
            set_namelist_rst namelist ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"GYREGO_SHORT\"
            set_namelist_rst namelist ${ITEND} "GYREGO_LONG" "OCE"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for GYRE_GO
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_2_4"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_4_2"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_2_4" ]]; then
                set_namelist namelist_cfg cn_exp \"GYREGO_48\"
            else
                set_namelist namelist_cfg cn_exp \"GYREGO_84\"
                set_namelist namelist_cfg jpni 4
                set_namelist namelist_cfg jpnj 2
            fi
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_GYRE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi # GYRE_GO

# ----------------
# ORCA2_ICE_PISCES
# ----------------
if [ ${config} == "ORCA2_ICE_PISCES" ] ; then

    ITEND=1040   # 130 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=8   # 2 days

    # Configure and submit test runs for the ORCA2_ICE_PISCES SETTE
    # configuration (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" ] ; then

        # Default test-run configuration for the ORCA2_ICE_PISCES SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"O2L3P\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg ln_shuman .true.
        set_namelist namelist_cfg jpni 4
        set_namelist namelist_cfg jpnj 8
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist namelist_cfg ln_wave .true.
        set_namelist namelist_cfg ln_cdgw .false.
        set_namelist namelist_cfg ln_sdw  .true.
        if [ ${USING_ABL} == "yes" ]; then
            set_namelist namelist_cfg nn_date0 20130101
            set_namelist namelist_cfg ln_blk .false.
            set_namelist namelist_cfg ln_abl .true.
            set_namelist namelist_cfg ln_tair_pot .true.
            sed -i "/sn_wndi/s/u_10.15JUNE2009_fill/uwnd_ERAI_L25Z10_ORCA2_ana1d/; /sn_wndi/s/ 6./24./; /sn_wndi/s/U_10_MOD/uwnd/; \
                    /sn_wndi/s/true/false/; /sn_wndi/s/yearly/monthly/; /sn_wndi/s/weights_core2_orca2_bicub//" namelist_cfg
            sed -i "/sn_wndj/s/v_10.15JUNE2009_fill/vwnd_ERAI_L25Z10_ORCA2_ana1d/; /sn_wndj/s/ 6./24./; /sn_wndj/s/V_10_MOD/vwnd/; \
                    /sn_wndj/s/true/false/; /sn_wndj/s/yearly/monthly/; /sn_wndj/s/weights_core2_orca2_bicub//" namelist_cfg
            sed -i "/sn_tair/s/t_10.15JUNE2009_fill/tair_ERAI_L25Z10_ORCA2_ana1d/; /sn_tair/s/ 6./24./; /sn_tair/s/T_10_MOD/tair/; \
                    /sn_tair/s/true/false/; /sn_tair/s/yearly/monthly/; /sn_tair/s/weights_core2_orca2_bilin//" namelist_cfg
            sed -i "/sn_humi/s/q_10.15JUNE2009_fill/humi_ERAI_L25Z10_ORCA2_ana1d/; /sn_humi/s/ 6./24./; /sn_humi/s/Q_10_MOD/humi/; \
                    /sn_humi/s/true/false/; /sn_humi/s/yearly/monthly/; /sn_humi/s/weights_core2_orca2_bilin//" namelist_cfg
        fi
        set_namelist_opt namelist_cfg ln_icebergs ${USING_ICEBERGS} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        # for debugging purposes set_namelist namelist_cfg rn_test_box -180.0, 180.0, -90.0, 90.0
        set_namelist namelist_top_cfg ln_trcbc  .false.
        # put ln_ironsed, ln_hydrofe to false
        # if not you need input files, and for tests is not necessary
        set_namelist namelist_pisces_cfg ln_varpar .false.
        set_namelist namelist_pisces_cfg ln_ironsed .false.
        set_namelist namelist_pisces_cfg ln_ironice .false.
        set_namelist namelist_pisces_cfg ln_hydrofe .false.
        # put ln_pisdmp to false : no restoring to global mean value
        set_namelist namelist_pisces_cfg ln_pisdmp .false.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=32

        ## Restartability tests for ORCA2_ICE_PISCES
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"O2L3P_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist namelist_cfg ln_use_calving .false.
            set_namelist namelist_cfg ln_use_test .true.
            set_namelist namelist_cfg nn_test_icebergs 10
	        set_namelist namelist_cfg rn_test_box -180.0, 180.0, -90.0, 90.0
            set_namelist namelist_ice_cfg ln_icediachk .true.
            set_namelist namelist_top_cfg ln_trcdta .false.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"O2L3P_SHORT\"
            set_namelist_rst namelist ${ITEND} "O2L3P_LONG" "OCE ICE TOP ABL ICB"
            set_namelist namelist_cfg ln_use_calving .false.
            set_namelist namelist_cfg ln_use_test .false.
            set_namelist namelist_ice_cfg ln_icediachk .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for ORCA2_ICE_PISCES
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_4_8"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_8_4"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_4_8" ]]; then
                set_namelist namelist_cfg cn_exp \"O2L3P_48\"
            else
                set_namelist namelist_cfg cn_exp \"O2L3P_84\"
                set_namelist namelist_cfg jpni 8
                set_namelist namelist_cfg jpnj 4
            fi
            set_namelist namelist_cfg ln_use_calving   .true.
            set_namelist namelist_cfg ln_use_test      .true.
            set_namelist namelist_cfg nn_test_icebergs 10
	          set_namelist namelist_cfg rn_test_box -180.0, 180.0, -90.0, 90.0
            set_namelist namelist_top_cfg ln_trcdta .false.

            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_ICE_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi # ORCA2_ICE_PISCES

# ----------------
# ORCA2_OFF_PISCES
# ----------------
if [ ${config} == "ORCA2_OFF_PISCES" ]  ;  then

    ITEND=380   # 95 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=16   # 4 day

    # Configure and submit test runs for the ORCA2_OFF_PISCES SETTE
    # configuration (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" ] ; then

        # Default test-run configuration for the ORCA2_OFF_PISCES SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"OFFP\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 4
        set_namelist namelist_cfg jpnj 8
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist namelist_cfg ln_qsr_rgb .true.
        set_namelist namelist_top_cfg ln_trcbc  .false.
        # put ln_ironsed, ln_hydrofe to false
        # if not you need input files, and for tests is not necessary
        set_namelist namelist_pisces_cfg ln_varpar .false.
        set_namelist namelist_pisces_cfg ln_ironsed .false.
        set_namelist namelist_pisces_cfg ln_ironice .false.
        set_namelist namelist_pisces_cfg ln_hydrofe .false.
        # put ln_pisdmp to false : no restoring to global mean value
        set_namelist namelist_pisces_cfg ln_pisdmp .false.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
        NPROC=32

        ## Restartability tests for ORCA2_OFF_PISCES
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"OFFP_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist namelist_top_cfg ln_trcdta .false.
            set_xio_using_server iodef.xml ${USING_MPMD}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"OFFP_SHORT\"
            set_namelist_rst namelist ${ITEND} "OFFP_LONG" "TOP"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME}  ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC  ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for ORCA2_OFF_PISCES
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_4_8"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_8_4"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_4_8" ]]; then
                set_namelist namelist_cfg cn_exp \"OFFP_48\"
            else
                set_namelist namelist_cfg cn_exp \"OFFP_84\"
                set_namelist namelist_cfg jpni 8
                set_namelist namelist_cfg jpnj 4
            fi
            set_namelist namelist_top_cfg ln_trcdta .false.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_OFF_PISCES.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi # ORCA2_OFF_PISCES

# -----
# AMM12
# -----
if [ ${config} == "AMM12" ] ;  then

    ITEND=576   # 8 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=12   # 4 hours

    # Configure and submit test runs for the AMM12 SETTE configuration (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" ] ; then

        # Default test-run configuration for the AMM12 SETTE configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 4
        set_namelist namelist_cfg jpnj 8
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
	# Disable tiling pending further work
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=32

        ## Restartability tests for AMM12
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"AMM12_LONG\"
            set_namelist_rst namelist ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"AMM12_SHORT\"
            set_namelist_rst namelist ${ITEND} "AMM12_LONG" "OCE"
            set_namelist namelist_cfg nn_date0 20120102
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests for AMM12
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_8_4"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_4_8"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_8_4" ]]; then
                set_namelist namelist_cfg cn_exp \"AMM12_84\"
                set_namelist namelist_cfg jpni 8
                set_namelist namelist_cfg jpnj 4
            else
                set_namelist namelist_cfg cn_exp \"AMM12_48\"
            fi
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AMM12.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi # AMM12

# -------------
# ORCA2_SAS_ICE
# -------------
if [ ${config} == "ORCA2_SAS_ICE" ] ;  then

    ITEND=256   # 32 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=16   # 2 days

    # Configure and submit test runs for the ORCA2_SAS_ICE SETTE configuration
    # (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" ] ; then

        # Default test-run configuration for the ORCA2_SAS_ICE SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"ORCA2_SAS_ICE\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 4
        set_namelist namelist_cfg jpnj 8
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=32

        ## Restartability tests
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist_rst namelist ${ITEND}
            set_namelist namelist_ice_cfg ln_icediachk .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_SAS_ICE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist_rst namelist ${ITEND} "ORCA2_SAS_ICE" "OCE ICE"
            set_namelist namelist_cfg nn_date0 010109
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_SAS_ICE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_4_8"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_8_4"
        for name in ${names}; do
            export TEST_NAME=${name}
            if [[ ${name} == "REPRO_4_8" ]]; then

                ITEND=80   # 5 days
                [[ "${USING_DEBUG}" == "yes" ]] && ITEND=16   # 1 day

            fi
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_4_8" ]]; then
                set_namelist namelist_cfg cn_exp \"ORCA2_SAS_ICE_48\"
            else
                set_namelist namelist_cfg cn_exp \"ORCA2_SAS_ICE_84\"
                set_namelist namelist_cfg jpni 8
                set_namelist namelist_cfg jpnj 4
            fi
            set_namelist namelist_cfg nn_itend ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_SAS_ICE.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi

# -------------
# ORCA2_ICE_OBS
# -------------
## Test assimilation interface code, OBS and ASM for reproducibility
## Restartability not tested (ASM code not restartable while increments are being applied)
if [ ${config} == "ORCA2_ICE_OBS" ] ;  then

    ITEND=80   # 10 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=16   # 2 days

    # Configure and submit test runs for the ORCA2_ICE_OBS SETTE configuration
    # (if any)
    if [ ${DO_REPRO} == "1" -o ${DO_VARIANTS} == "1" ] ; then

        # Default test-run configuration for the ORCA2_ICE_OBS SETTE
        # configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        MODELDATA="./O2L3OBS_84_1ts_00010101_00010110_SAO.nc"
        [[ ${ITEND} == "16" ]] && MODELDATA="./O2L3OBS_84_1ts_00010101_00010102_SAO.nc"
        set_namelist namelist_cfg cn_exp \"O2L3OBS\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg ln_read_cfg .true.
        set_namelist namelist_cfg jpni 4
        set_namelist namelist_cfg jpnj 8
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
        set_namelist namelist_cfg sn_cfctl%l_obsstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist namelist_cfg ln_diaobs .true.
        set_namelist namelist_cfg nn_obsgroups 5
        # Exclude the initial state from the application of the OBS operator:
        #   - ensure the time-step length is set to 3 hours
        set_namelist namelist_cfg rn_Dt 10800
        #   - set the start date for the application of the OBS operator to 3
        #     hours from the start of the model run
        set_namelist namelist_cfg dn_dobsini 00010101.030000
        set_namelist namelist_cfg dn_dobsend 00010102.000000
        # Ensure all diagnostic output, including that related to the SBC, is
        # available at each time step
        set_namelist namelist_cfg nn_fsbc 1
        # Configure the diagnostic output to correspond to the model variables
        # used by OBS:
        #   - increase the diagnostic output frequency
        #   - maximise the output precision
        #   - reduce the number of diagnostic output fields
        #   - replace the output of the sea-ice fraction (whose non-zero values
        #     are capped at a minimum of 1e-6) with the corresponding output
        #     from SBC (which does not include such limit)
        #   - collate output associated with different grids into a common
        #     output file
        sed -i -E -e 's/"5d"/"1ts"/g;s/name_suffix="_grid_T"/name_suffix="_SAO"/' \
                  -e 's/freq_op[^<]*<\/field/\//' \
                  -e 's/<field /<field prec="8" /g' \
                  -e '/<file_group id="oce_5d" /p;/<file id="file1"/p;/<(\/)?file(_group)?[> ]/d' \
                  -e '/<field [^>]*name="([suv]o)?(zos)?(thetao)?"/p;/<field /d' \
                  -e '/<\/file_definition>/i<field prec="8" field_ref="ice_cover" name="siconc"/></file></file_group>' ./file_def_nemo-oce.xml
        # Create namobs_dta namelists to append
        temp_obs_namelist=${EXE_DIR}/temp_obs_namelist_cfg
cat > ${temp_obs_namelist} << EOF
&namobs_dta
cn_groupname = 'pro'
ln_enabled = .true.
ln_prof = .true.
cn_obsfiles = 'profiles_01.nc'
cn_obstypes = 'POTM','PSAL'
/
&namobs_dta
cn_groupname = 'vel3d'
ln_enabled = .true.
ln_prof = .true.
cn_obsfiles = 'vel_01.nc'
cn_obstypes = 'UVEL','VVEL'
/
&namobs_dta
cn_groupname = 'sst'
ln_enabled = .true.
ln_surf = .true.
cn_obsfiles = 'sst_01.nc'
cn_obstypes = 'SST'
/
&namobs_dta
cn_groupname = 'sla'
ln_enabled = .true.
ln_surf = .true.
cn_obsfiles = 'sla_01.nc'
cn_obstypes = 'SLA'
/
&namobs_dta
cn_groupname = 'sic'
ln_enabled = .true.
ln_surf = .true.
cn_obsfiles = 'sic_01.nc'
cn_obstypes = 'ICECONC'
/
&namsao
   sao_files(1) = "${MODELDATA}"
   nn_sao_idx(1) = 1
   nn_sao_freq = 1
EOF
        for(( n=1; n <= ${ITEND}; n++ )); do
            echo "   sao_files($(( n + 1 ))) = \"${MODELDATA}\"" >> ${temp_obs_namelist}
            echo "   nn_sao_idx($(( n + 1 ))) = ${n}" >> ${temp_obs_namelist}
        done
        echo "/" >> ${temp_obs_namelist}
        cat ${temp_obs_namelist} >> namelist_cfg
        set_namelist namelist_cfg ln_bkgwri .true.
        set_namelist namelist_cfg ln_trainc .true.
        set_namelist namelist_cfg ln_dyninc .true.
        set_namelist namelist_cfg ln_sshinc .true.
        set_namelist namelist_cfg ln_asmiau .true.
        set_namelist namelist_cfg ln_use_test .true.
        set_namelist namelist_cfg nn_test_icebergs 10
        #remove all useless options for pisces (due to ORCA2_ICE_PISCES reference configuration)
        set_namelist namelist_top_cfg ln_trcdta .false.
        set_namelist namelist_top_cfg ln_trcbc  .false.
        # put ln_ironsed, ln_river, ln_ndepo, ln_dust to false
        # if not you need input files, and for tests is not necessary
        set_namelist namelist_pisces_cfg ln_varpar .false.
        set_namelist namelist_pisces_cfg ln_ironsed .false.
        set_namelist namelist_pisces_cfg ln_ironice .false.
        set_namelist namelist_pisces_cfg ln_hydrofe .false.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=32
        # Identical base configuration for ORCA2_ICE_OBS and ORCA2_ICE_OBS_SAO
        # SETTE runs
        cp ./namelist_cfg ../../${config}_SAO${CONFIG_SUFFIX}/EXP00/

        # Reproducibility test runs
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_4_8"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_8_4"
        # Standalone test runs
        [[ ${DO_VARIANTS} == "1" ]] && [[ -n ${VARIANTS_REF} ]] && names="${names} ${VARIANTS_REF}"
        [[ ${DO_VARIANTS} == "1" ]] && [[ -z ${VARIANTS_REF} ]] && VARIANTS_REF="REPRO_8_4"
        [[ ${DO_VARIANTS} == "1" ]]                             && names="${names} SAO"
        for name in ${names}; do
            if [[ ${name} == "SAO" ]]; then
                SETTE_CONFIG="${config}_SAO${CONFIG_SUFFIX}"
            else
                SETTE_CONFIG="${config}${CONFIG_SUFFIX}"
            fi
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            if [[ ${name} == "SAO" ]]; then
                [[ -e ${MODELDATA} ]] || ln -s ../../${config}${CONFIG_SUFFIX}/${VARIANTS_REF}/${MODELDATA} ./
            else
                JOB_FILE=${EXE_DIR}/run_job.sh
                if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            fi
            if [[ ${name} == "REPRO_4_8" ]]; then
                set_namelist namelist_cfg cn_exp \"O2L3OBS_48\"
            else
                set_namelist namelist_cfg cn_exp \"O2L3OBS_84\"
                set_namelist namelist_cfg jpni 8
                set_namelist namelist_cfg jpnj 4
            fi

            cd ${SETTE_DIR}
            . ./prepare_job.sh input_ORCA2_ICE_OBS.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            # The SAO test run depends on the test run specified by variable VARIANTS_REF
            if [[ ${DO_VARIANTS} != "1" ]] || [[ ${name} != "${VARIANTS_REF}" ]]; then
                . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
            fi
        done

    fi

fi

# ----------
# AGRIF_DEMO
# ----------
if [ ${config} == "AGRIF_DEMO" ] ;  then

    ITEND=16   # 2 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=4   # 12 hours

    # Continue to RUN test phase, if requested
    [[ ${DO_RUN} -eq 0 ]] && break

    # Configure and submit test runs for the AGRIF_DEMO configuration (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" -o ${DO_VARIANTS} == "1" ] ; then

        # Default test-run configuration for the AGRIF_DEMO SETTE configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"AGRIF_DEMO\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist 1_namelist_cfg cn_exp \"AGRIF_DEMO\"
        set_namelist 1_namelist_cfg nn_it000 1
        set_namelist 1_namelist_cfg nn_itend ${ITEND}
        set_namelist 1_namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist 1_namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt 1_namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist 2_namelist_cfg cn_exp \"AGRIF_DEMO\"
        set_namelist 2_namelist_cfg nn_it000 1
        set_namelist 2_namelist_cfg nn_itend $(( ${ITEND} * 4 ))
        set_namelist 2_namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist 2_namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt 2_namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist 3_namelist_cfg cn_exp \"AGRIF_DEMO\"
        set_namelist 3_namelist_cfg nn_it000 1
        set_namelist 3_namelist_cfg nn_itend $(( ${ITEND} * 4 * 3 ))
        set_namelist 3_namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist 3_namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt 3_namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_namelist_opt 1_namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt 1_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt 1_namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_namelist_opt 2_namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt 2_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt 2_namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_namelist_opt 3_namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt 3_namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt 3_namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=16

        ## Restartability tests
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"AGRIF_DEMO_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist 1_namelist_cfg cn_exp \"AGRIF_DEMO_LONG\"
            set_namelist_rst 1_namelist ${ITEND}
            set_namelist 2_namelist_cfg cn_exp \"AGRIF_DEMO_LONG\"
            set_namelist_rst 2_namelist $(( ${ITEND} * 4 ))
            set_namelist 3_namelist_cfg cn_exp \"AGRIF_DEMO_LONG\"
            set_namelist_rst 3_namelist $(( ${ITEND} * 4 * 3 ))
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AGRIF_DEMO.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"AGRIF_DEMO_SHORT\"
            set_namelist_rst namelist ${ITEND} "AGRIF_DEMO_LONG" "OCE ICE TOP"
            set_namelist 1_namelist_cfg cn_exp \"AGRIF_DEMO_SHORT\"
            set_namelist_rst 1_namelist ${ITEND} "AGRIF_DEMO_LONG" "OCE ICE TOP"
            set_namelist 1_namelist_cfg ln_init_chfrpar .false.
            set_namelist 2_namelist_cfg cn_exp \"AGRIF_DEMO_SHORT\"
            set_namelist_rst 2_namelist $(( ${ITEND} * 4 )) "AGRIF_DEMO_LONG" "OCE ICE TOP"
            set_namelist 2_namelist_cfg ln_init_chfrpar .false.
            set_namelist 3_namelist_cfg cn_exp \"AGRIF_DEMO_SHORT\"
            set_namelist_rst 3_namelist $(( ${ITEND} * 4 * 3 )) "AGRIF_DEMO_LONG" "OCE ICE TOP"
            set_namelist 3_namelist_cfg ln_init_chfrpar .false.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AGRIF_DEMO.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_2_8"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_4_4"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            for nname in namelist_cfg 1_namelist_cfg 2_namelist_cfg 3_namelist_cfg; do
                if [[ ${name} == "REPRO_2_8" ]]; then
                    set_namelist ${nname} cn_exp \"AGRIF_DEMO_28\"
                    if [[   ${nname} == "1_namelist_cfg" ]]; then
			set_namelist ${nname} jpni 1
			set_namelist ${nname} jpnj 7
                    elif [[ ( ${nname} == "2_namelist_cfg" ) || ( ${nname} == "3_namelist_cfg" ) ]]; then
			set_namelist ${nname} jpni 3
			set_namelist ${nname} jpnj 3
		    else
			set_namelist ${nname} jpni 2
			set_namelist ${nname} jpnj 8
		    fi
                else
                    set_namelist ${nname} cn_exp \"AGRIF_DEMO_44\"
                    if [[   ${nname} == "1_namelist_cfg" ]]; then
			set_namelist ${nname} jpni 7
			set_namelist ${nname} jpnj 1
                    elif [[ ( ${nname} == "2_namelist_cfg" ) || ( ${nname} == "3_namelist_cfg" ) ]]; then
			set_namelist ${nname} jpni 1
			set_namelist ${nname} jpnj 9
		    else
			set_namelist ${nname} jpni 4
			set_namelist ${nname} jpnj 4
		    fi
                fi
            done
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AGRIF_DEMO.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

        ## test code corruption with AGRIF_DEMO (phase 1) ==> Compile with key_agrif but run with no zoom
        if [ ${DO_VARIANTS_0} == "1" ] ;  then

            ITEND=150   # 5 days and 9 hours
            [[ "${USING_DEBUG}" == "yes" ]] && ITEND=16   # 2 days

            export TEST_NAME="${VARIANTS_REF}"
            cd ${MAIN_DIR}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            NPROC=32
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"ORCA2\"
            set_namelist namelist_cfg nn_itend ${ITEND}
            # Use "original" parent grid bathymetry
            set_namelist namelist_cfg cn_domcfg "'ORCA_R2_zps_domcfg.nc'"
            # Set the number of fine grids to zero:
            sed -i "1s/.*/0/" ${EXE_DIR}/AGRIF_FixedGrids.in
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AGRIF_DEMO.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

            ## test code corruption with AGRIF_DEMO (phase 2) ==> Compile without key_agrif (to be compared with AGRIF_DEMO_ST/ORCA2)
            SETTE_CONFIG="AGRIF_DEMO_NOAGRIF"${CONFIG_SUFFIX}
            export TEST_NAME="NOAGRIF"
            EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"ORCA2\"
            # Use "original" parent grid bathymetry
            set_namelist namelist_cfg cn_domcfg "'ORCA_R2_zps_domcfg.nc'"
            set_namelist namelist_cfg nn_it000 1
            set_namelist namelist_cfg nn_itend ${ITEND}
            set_namelist namelist_cfg sn_cfctl%l_runstat .true.
            set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
            set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
            set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
            set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
            set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
            set_namelist_opt namelist_cfg ln_nnogather ${USING_NOGATHER} .true. .false.
            set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
            set_xio_using_server iodef.xml ${USING_MPMD}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            NPROC=32
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_AGRIF_DEMO.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

    fi

fi

# ------
# WED025
# ------
if [ ${config} == "WED025" ] ;  then

    ITEND=720   # 20 days
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=12   # 8 hours

    # Configure and submit test runs for the WED025 configuration (if any)
    if [ ${DO_RESTART} == "1" -o ${DO_REPRO} == "1" ] ; then

        # Default test-run configuration for the WED025 configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"WED025\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 4
        set_namelist namelist_cfg jpnj 8
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 2
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=32

        ## Restartability tests
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"WED025_LONG\"
            set_namelist_rst namelist ${ITEND}
            set_namelist namelist_cfg nn_date0 20000115
            #set_namelist namelist_ice_cfg ln_icediachk .true.
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_WED025.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"WED025_SHORT\"
            set_namelist_rst namelist ${ITEND} "WED025_LONG" "OCE ICE"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_WED025.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

        ## Reproducibility tests
        names=""
        [[ ${DO_REPRO_1} == "1" ]] && names="${names} REPRO_6_7"
        [[ ${DO_REPRO_2} == "1" ]] && names="${names} REPRO_8_4"
        for name in ${names}; do
            export TEST_NAME=${name}
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
            cd ${EXE_DIR}
            if [[ ${name} == "REPRO_6_7" ]]; then
                set_namelist namelist_cfg cn_exp \"WED025_67\"
                set_namelist namelist_cfg jpni 6
                set_namelist namelist_cfg jpnj 7
            else
                set_namelist namelist_cfg cn_exp \"WED025_84\"
                set_namelist namelist_cfg jpni 8
                set_namelist namelist_cfg jpnj 4
            fi
            set_namelist namelist_cfg nn_date0 20000115
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_WED025.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done

    fi

fi

# -----
# C1D_*
# -----
if [[ ${config} =~ "C1D" ]]  ; then

    ITEND=87600   # 365 days (PAPA)
    [[ ${config} =~ "ASICS" ]] && ITEND=17040
    [[ ${config} =~ "BATS"  ]] && ITEND=26280
    [[ ${config} =~ "SAS"   ]] && ITEND=8760
    [[ "${USING_DEBUG}" == "yes" ]] && ITEND=240   # 1 day

    # Configure and submit test runs for the C1D configuration (if any)
    if [ ${DO_RESTART} == "1" ] ; then

        # Default test-run configuration for the C1D configuration
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
        cd ${EXE_DIR}
        set_namelist namelist_cfg cn_exp \"${config//_}\"
        set_namelist namelist_cfg nn_it000 1
        set_namelist namelist_cfg nn_itend ${ITEND}
        set_namelist namelist_cfg jpni 1
        set_namelist namelist_cfg jpnj 1
        set_namelist namelist_cfg sn_cfctl%l_runstat .true.
        set_namelist namelist_cfg sn_cfctl%l_trcstat .true.
        set_namelist namelist_cfg sn_cfctl%l_lsb_sum .true.
        set_namelist_opt namelist_cfg ln_timing ${USING_TIMING} .true. .false.
        set_namelist_opt namelist_cfg nn_hls ${USING_EXTRA_HALO} 3 1
        set_namelist_opt namelist_cfg nn_comm ${USING_COLLECTIVES} 2 1
        set_namelist_opt namelist_cfg ln_tile ${USING_TILING} .true. .false.
        set_xio_using_server iodef.xml ${USING_MPMD}
        NPROC=1

        ## Restartability tests for C1D
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            export TEST_NAME="LONG"
            cd ${SETTE_DIR}
            . ./prepare_exe_dir.sh
            JOB_FILE=${EXE_DIR}/run_job.sh
            if [ -f ${JOB_FILE} ] ; then \rm ${JOB_FILE} ; fi
        fi
        if [ ${DO_RESTART_1} == "1" ] ;  then
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"${config//_}_LONG\"
            set_namelist_rst namelist ${ITEND}
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_${config/_*}.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            export TEST_NAME="SHORT"
            . ./prepare_exe_dir.sh
            set_valid_dir
            clean_valid_dir
            cd ${EXE_DIR}
            set_namelist namelist_cfg cn_exp \"${config//_}_SHORT\"
            set_namelist_rst namelist ${ITEND} "${config//_}_LONG" "OCE"
            cd ${SETTE_DIR}
            . ./prepare_job.sh input_${config/_*}.cfg $NPROC ${TEST_NAME} ${MPIRUN_FLAG} ${JOB_FILE} ${NUM_XIOSERVERS} ${NEMO_VALID}
        fi
        if [ ${DO_RESTART_1} == "1" -o ${DO_RESTART_2} == "1" ] ;  then
            cd ${SETTE_DIR}
            . ./fcm_job.sh $NPROC ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        fi

    fi

fi

done
#
# Return to SETTE_DIR (last fcm_job.sh will have moved to EXE_DIR)
cd ${SETTE_DIR}
