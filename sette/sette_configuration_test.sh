#!/bin/bash
############################################################
# Author : Simona Flavoni for NEMO
# Contact: sflod@locean-ipsl.upmc.fr
# 2013   : A.C. Coward added options for testing with XIOS in dettached mode
# 2026   : P. Mathiot generalised the script
#
# Purpose : prepare, run and process simulation required by the testing suite.
#############################################################################
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
# ------------------------------------------------------------------------------
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
if [ ${USING_MPMD} == "no" ]; then
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

. ./all_functions.sh

# Loop on all the configuration to test (see param.cfg / param.default)
for config in ${TEST_CONFIGS[@]} ; do

    echo ''
    echo "Process $config"
    echo ''

    # default settings
    SAO_FLAG="0"     # overwritten by input_CONFIG.cfg if needed
    NOAGRIF_FLAG="0" # overwritten by input_CONFIG.cfg if needed
    MPP_FLAG="1"     # overwritten by input_CONFIG.cfg if needed
    RST_FLAG="1"     # overwritten by input_CONFIG.cfg if needed
    PHY_FLAG="0"     # overwritten by input_CONFIG.cfg if needed
    ROT_FLAG="0"     # overwritten by input_CONFIG.cfg if needed
    CPL_FLAG="0"     # overwritten by input_CONFIG.cfg if needed
    
    # some configs do not support tiling
    TILE_FLAG="1"    # overwritten by input_CONFIG.cfg if needed

    ATM_NPROC=0   # overwritten by input_CONFIG.cfg if needed

    # components used for compilation
    COMPONENTS=""

    # type of simulations
    TYPE=''

    # Most SETTE configurations are compiled with the common modification of
    # the pre-processing-key list
    ADD_KEYS_LOC="${ADD_KEYS}"
    DEL_KEYS_LOC="${DEL_KEYS}"

    # Make sure variant variables are empty
    COMPONENTS_VAR=''
    DEL_KEYS_VAR_LOC=''
    config_var=''

    # Each SETTE configuration is associated with a NEMO reference
    # configuration or test case; in most cases the names match
    ref_config=''
 
    # if input_${config}.cfg not available we skip
    if [ ! -f ${SETTE_DIR}/INPUT_CARDS/input_${config}.cfg ]; then i
        echo "input_${config}.cfg is missing, we skip ${config} tests"
        DO_SKIP=1
        continue
    else    
        DO_SKIP=0
        . ${SETTE_DIR}/INPUT_CARDS/input_${config}.cfg
    fi

    # Ignore unavailable configurations
    [[ ${config} =~ "C1D" ]] && DO_SKIP=0   # Enable testing of C1D variants
    [[ ${DO_SKIP} -eq 1 ]] && continue

    # Variants
    # If not defined in input_config card, used default one
    config_var="${config_var:-${config}}"                    # Default: no variant
    COMPONENTS_VAR=${COMPONENTS_VAR:-"${COMPONENTS}"}        # Default: no change to sub-components for variant
    DEL_KEYS_VAR_LOC=${DEL_KEYS_VAR_LOC:-"${DEL_KEYS_LOC}"}  # Default: matching key modification between reference and variant

    # What do we do with these test ? No avail config are going through !!!!
    [[ ${config} =~ "C1D" ]] && [[ ${config} =~ "SAS" ]] && COMPONENTS="OCE ICE TOP SAS"
    [[ ${config} =~ "C1D" ]] && [[ ${config} =~ "ASICS" ]] && DEL_KEYS_LOC="${DEL_KEYS} key_top"
    
    # define config type
    ref_config=${ref_config:-${config}}
    TYPE=${CONFIG_TYPE:-cfgs}
   
    # 
    CONFIG_DIR0=${MAIN_DIR}/${TYPE}

    # PSyclone-based source-code processing (if required)
    TRANSFORM_OPT=""
    [[ -n "${TRANSFORM}" ]] && TRANSFORM_OPT="-p ${TRANSFORM}"

    CONFIG_SUFFIX=${SETTE_STG}
    DO_REF=0
    DO_RST=${DO_RESTART}
    DO_MPP=${DO_REPRO}
    DO_SAO=${DO_VARIANTS}
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
    SETTE_CONFIG_REF="${config}${CONFIG_SUFFIX}"
    SETTE_CONFIG_VAR="${config_var}${CONFIG_SUFFIX}"
    SETTE_CONFIG=${SETTE_CONFIG_REF}

    export XIOS_OASIS="not_empty_variable"

    # Skip CPL_OASIS test case if an OASIS build is not specified
    if [[ ${config} == "CPL_OASIS" ]] ; then
        cd ${MAIN_DIR}

        ARCH_FILE=$(find arch/ -name arch-${CMP_NAM}.fcm)
        # Generate archfile if compiler is set to "auto"
        if [[ -z "${ARCH_FILE}" && ${CMP_NAM_A} == "auto" ]]; then
            ENV_FILE=$(find arch/ -name arch-${CMP_NAM_A}.env)
            [ -n "${ENV_FILE}" ] && source ${ENV_FILE}
            ./arch/build_arch-auto.sh
            ARCH_FILE=$(find arch/ -name arch-${CMP_NAM_A}.fcm)
        fi
        # Test that OASIS path is defined in the arch file and that it exists
        [[ ! -f ${ARCH_FILE} ]] && echo "WARNING: arch-${CMP_NAM_A}.fcm file not found -> cannot check for OASIS directory -> CPL_OASIS testcase skipped !" && break
        OASIS_DIR=$(sed -rn "/^%OASIS_(PREFIX|HOME) /s/%OASIS_(PREFIX|HOME) +(.*)/\2/p" ${ARCH_FILE})
        [[ -z ${OASIS_DIR} ]] && echo "WARNING: String matching \"%OASIS_(PREFIX|HOME)\" not found in arch-${CMP_NAM_A}.fcm file -> CPL_OASIS testcase skipped !" && break
        OASIS_DIR=$(eval "echo ${OASIS_DIR}")       # Expand environment variables
        [[ ! -d ${OASIS_DIR} ]] && echo "WARNING: OASIS directory \"${OASIS_DIR}\" in arch-${CMP_NAM_A}.fcm file does not exist -> CPL_OASIS testcase skipped !" && break
    fi

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

        # Configuration-specific build actions that can be carried out
        # independent of the baseline-configuration compilation
        if [[ ${config} == "CPL_OASIS" ]] ; then
            cd ${MAIN_DIR}
            ./tools/maketools -m ${CMP_NAM_A} -n TOYATM
        fi

    fi

    # Continue to next configuration unless the RUN test phase has been requested
    [[ ${DO_RUN} -eq 0 ]] && break

    cd ${SETTE_DIR}

    EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
    set_xio_using_server iodef.xml ${USING_MPMD}

    if [[ ${DO_VARIANTS} -eq 1 ]] ; then
        EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG_VAR}/EXP00
        set_xio_using_server iodef.xml ${USING_MPMD}
    fi

    # enforce run of REF if RST|MPP activated
    if [ "${DO_RST}" == "1" ] ; then DO_REF=1 ; fi

    # *_FLAGS are overwritten by config cards when needed.
    if [ "${DO_COUPLING}" == "1" ] && [ ${CPL_FLAG} == "1" ] ; then DO_REF=0 ; fi

# ------------------------------------------------------
    
    # prepare reference run  
    if [ ${DO_REF} == "1" ] ; then 
        SETTINGS=("${REF_NIT000}" "${REF_NITEND}" "${REF_JPNI}" "${REF_JPNJ}" "${REF_NPROC}" REF "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
        prepare_job "${SETTINGS[@]}" ; JOB_FILE_REF=${JOB_FILE}
    fi

    # prepare restartability run
    # script to run RST added to REF run_job.sh
    if [ ${DO_RST} == "1" ] && [ ${RST_FLAG} == "1" ] ; then
        SETTINGS=("${RST_NIT000}" "${RST_NITEND}" "${RST_JPNI}" "${RST_JPNJ}" "${RST_NPROC}" RST "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
        prepare_job "${SETTINGS[@]}"
    fi
    
    # submit reference (and restartability run if needed) job
    if [ ${DO_REF} == "1" ] ; then . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE_REF} ${INTERACT_FLAG} ${MPIRUN_FLAG} ; fi


# ------------------------------------------------------
    # manage SAO run
    # SAO_FLAG overwriten by config card if not 0
    if [ ${DO_SAO} == "1" ] && [ ${SAO_FLAG} == "1" ] ; then
        
        # run reference SAO (to be removed later as = REF)
        SETTE_CONFIG=${SETTE_CONFIG_REF}
        SETTINGS=("${REF_NIT000}" "${REF_NITEND}" "${REF_JPNI}" "${REF_JPNJ}" "${REF_NPROC}" SAOREF "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
        prepare_job "${SETTINGS[@]}" ; JOB_FILE_REF=${JOB_FILE}

        # prepare stand alone observation run
        SETTE_CONFIG=${SETTE_CONFIG_VAR}
        SETTINGS=("${REF_NIT000}" "${REF_NITEND}" "${REF_JPNI}" "${REF_JPNJ}" "${REF_NPROC}" SAO "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
        prepare_job "${SETTINGS[@]}"

        SETTE_CONFIG=${SETTE_CONFIG_REF}

        # submit SAO run
        . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE_REF} ${INTERACT_FLAG} ${MPIRUN_FLAG}
    fi
# ------------------------------------------------------
    # manage coupling run
    if [ ${DO_COUPLING} == "1" ] && [ ${CPL_FLAG} == "1" ] ; then

        echo ''
        echo "Process $config CPL"
        echo ''

        # prepare reproducibility run
        SETTINGS=("${REF_NIT000}" "${REF_NITEND}" "${REF_JPNI}" "${REF_JPNJ}" "${REF_NPROC}" CPL "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
        prepare_job "${SETTINGS[@]}"

        # submit reproducibility job
        . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
    fi

# ------------------------------------------------------
    # manage reproducibility run
    if [ ${DO_MPP} == "1" ] && [ ${MPP_FLAG} == "1" ]; then 

        echo ''
        echo "Process $config MPP"
        echo ''

        # prepare reproducibility reference run (to be removed later as = REF)
        SETTINGS=("${REF_NIT000}" "${REF_NITEND}" "${REF_JPNI}" "${REF_JPNJ}" "${REF_NPROC}" MPPREF "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
        prepare_job "${SETTINGS[@]}"

        # submit reproducibility reference job
        . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}

        # prepare reproducibility run
        SETTINGS=("${MPP_NIT000}" "${MPP_NITEND}" "${MPP_JPNI}" "${MPP_JPNJ}" "${MPP_NPROC}" MPP "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
        prepare_job "${SETTINGS[@]}"

        # submit reproducibility job
        . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
    fi
# ------------------------------------------------------
    # manage physical option variants
    # reset EXE_DIR to find various namelist experiements
    # PHY_FLAG overwrite by config card if not 0
    EXE_DIR=${CMP_DIR:-${CONFIG_DIR0}}/${SETTE_CONFIG}/EXP00
    if [ ${PHY_FLAG} == "1" ] && [ ${DO_PHYOPTS} == "1" ]; then

        # manage debug
        [[ "${USING_DEBUG}" == "yes" ]] && PHY_NITEND=${PHYDBG_NITEND}

        for file in $(echo `ls ${EXE_DIR}/namelist_*_cfg `) ; do
            # get test name
            TEST_NAME=`echo $file | sed -e "s/.*namelist_//" | sed -e "s/_cfg//"`
            TEST_NAME="EXP-${TEST_NAME}"

            echo ''
            echo "Process $config $TEST_NAME phyical option"
            echo ''

            # prepare physical variant run
            SETTINGS=("${REF_NIT000}" "${PHY_NITEND}" "${REF_JPNI}" "${REF_JPNJ}" "${REF_NPROC}" "${TEST_NAME}" "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
            prepare_job "${SETTINGS[@]}"

            # submit physical variant job
            . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done
    fi

# ------------------------------------------------------
    # manage rotational tests
    if [ ${ROT_FLAG} == "1" ] && [ ${DO_ROTSYM} == "1" ] ; then
        for TEST_NAME in "ROT_000" "ROT_090" "ROT_180" ; do

            echo ''
            echo "process $config $TEST_NAME test"
            echo ''

            # prepare physical variant run
            SETTINGS=("${REF_NIT000}" "${REF_NITEND}" "${REF_JPNI}" "${REF_JPNJ}" "${REF_NPROC}" "${TEST_NAME}" "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
            prepare_job "${SETTINGS[@]}"
            
            # submit physical variant job
            . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done
    fi

# ------------------------------------------------------
    # manage agrif tests
    # NOAGRIF_FLAG overwrite by config card if not 0
    if [ ${DO_VARIANTS} -eq 1 ] && [ ${NOAGRIF_FLAG} == "1" ] ; then
        for VARIANT_NAME in "AGRIF_DEMO_NOAGRIF" "AGRIF_DEMO"; do
        
            echo ''
            echo "process $config $VARIANT_TEST test"
            echo ''

            # test code corruption with AGRIF_DEMO (phase 2) ==> Compile without key_agrif (to be compared with AGRIF_DEMO_ST/ORCA2)
            SETTE_CONFIG="${VARIANT_NAME}"${CONFIG_SUFFIX}
            
            # prepare physical variant run
            SETTINGS=("${NOAGRIF_NIT000}" "${NOAGRIF_NITEND}" "${NOAGRIF_JPNI}" "${NOAGRIF_JPNJ}" "${NOAGRIF_NPROC}" "NOAGRIF" "${INPUT_FILES}" "${RST_COMP}" "${ATM_NPROC}")
            prepare_job "${SETTINGS[@]}"

            # submit noagrif variant job 
            . ${SETTE_DIR}/fcm_job.sh ${TOTAL_NPROCS} ${JOB_FILE} ${INTERACT_FLAG} ${MPIRUN_FLAG}
        done
    fi

# ------------------------------------------------------
done
