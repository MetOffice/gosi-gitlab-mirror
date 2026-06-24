#!/bin/bash
# ======================================================================
#               ***  SCRIPT  sette_list_avail_rev.sh  ***
#  SETTE: list available SETTE test-run records
# ======================================================================
#
# ----------------------------------------------------------------------
# NEMO/SETTE 5.1.a, NEMO Consortium (2026)
# Software governed by the CeCILL license (see ./LICENSE.txt)
# ----------------------------------------------------------------------

SETTE_DIR=$(cd $(dirname "$0"); pwd)
MAIN_DIR=$(dirname $SETTE_DIR)
TOOLS_DIR=${MAIN_DIR}/tools
USE_REF=0
COMPILER=""

. ./param.default
if [ -f ./param.cfg ]; then
  . ./param.cfg
else
  echo "warning: \"param.cfg\" file not found; SETTE will use default paramaters from \"param.default\" file"
fi

if [ $# -gt 0 ]; then
  while getopts m:Rh option; do 
     case $option in
        m) COMPILER="${OPTARG}"
           if [[ ! "${COMPILER}" =~ ^[[:alnum:].+_-]{1,64}$ ]]; then
             echo "  -c: incompatible compilation-environment name specified"
             exit 1
           fi
           ;;
        R) USE_REF=1;;
        h | *) echo "sette_list_avail_rev.sh:"
           echo "" 
           echo "     lists the revision identifiers, as well as the configuration identifiers"
           echo "     associated with a specified compilation-environment name, of the"
           echo "     SETTE-validation-database records"
           echo ""
           echo "  -m COMPILER"
           echo "     compilation-environment name"
           echo ""
           echo "  -R"
           echo "     list identifiers available in the reference SETTE validation database"
           echo ""
           exit 42;;
     esac
  done
  shift $((OPTIND - 1))
fi

NEMO_VALID="${NEMO_VALIDATION_DIR}/"
if [[ ${USE_REF} -eq 1 ]] ; then 
  NEMO_VALID="${NEMO_VALIDATION_REF}/"
fi

echo "Revision identifiers, as well as configuration identifiers associated with the"
echo "compilation-environment name ${COMPILER}, of the records in"
echo -n "the SETTE "
[[ ${USE_REF} -eq 1 ]] && echo -n "reference "
echo "validation database at"
echo "${NEMO_VALID}"
echo ""
${TOOLS_DIR}/SETTE/sette_toolkit.py --path "${NEMO_VALID}" catalogue --depth 3 --compiler "${COMPILER}"
